(in-package :beast)


;;;; Notes
;;; Entities are stored in an {id -> entity} hash table.
;;;
;;; Entities are also indexed by aspect in a nested hash table:
;;;
;;;     {aspect-symbol -> {id -> entity}}
;;;
;;; Entities are indexed by system too:
;;;
;;;     {system-symbol ->
;;;         ({id -> entity}   ; arg1
;;;          {id -> entity})  ; arg2
;;;     }
;;;
;;; Systems are stored as:
;;;
;;;     {system-symbol -> (system-function arity type-specifier-list)}
;;;
;;; TODO: Figure out the distinct problem.


;;;; Global Data Structures ---------------------------------------------------
(defvar *entity-id-counter* 0)
(defvar *entity-index* (make-hash-table))
(defvar *aspect-index* (make-hash-table))
(defvar *system-index* (make-hash-table))
(defvar *systems* (make-hash-table))


;;;; Entities -----------------------------------------------------------------
(defclass entity ()
  ((id
     :reader entity-id :initform (incf *entity-id-counter*)
     :documentation
     "The unique ID of the entity.  This may go away in the future.")
   (%beast/aspects
     :allocation :class :initform nil
     :documentation
     "A list of the aspects this entity class inherits.  **Don't touch this.**"))
  (:documentation "A single entity in the game world."))

(defmethod print-object ((e entity) stream)
  (print-unreadable-object (e stream :type t :identity nil)
    (format stream "~D" (entity-id e))))


(defun entity-satisfies-system-type-specifier-p (entity specifier)
  (every (lambda (aspect) (typep entity aspect))
         specifier))

(defun index-entity (entity)
  "Insert `entity` into the entity index."
  (setf (gethash (entity-id entity) *entity-index*) entity))

(defun index-entity-aspects (entity)
  "Insert `entity` into appropriate aspect indexes."
  (loop :for aspect :in (slot-value entity '%beast/aspects)
        :do (setf (gethash (entity-id entity)
                           (gethash aspect *aspect-index*))
                  entity)))

(defun index-entity-systems (entity)
  "Insert `entity` into appropriate system indexes."
  (loop
    :with id = (entity-id entity)
    :for system :being :the hash-keys :of *systems*
    :using (hash-value (nil nil type-specifiers))
    :do (loop :for argument-index :in (gethash system *system-index*)
              :for specifier :in type-specifiers
              :when (entity-satisfies-system-type-specifier-p entity specifier)
              :do (setf (gethash id argument-index) entity))))


(defun unindex-entity (id)
  "Remove `entity` from the entity-level index."
  (remhash id *entity-index*))

(defun unindex-entity-aspects (id)
  "Remove `entity` from the aspect indexes."
  (loop
    :for index :being :the hash-values :of *aspect-index*
    :do (remhash id index)))

(defun unindex-entity-systems (id)
  "Remove `entity` from the system indexes."
  (loop
    :for argument-indexes :being :the hash-values :of *system-index*
    :do (loop :for index :in argument-indexes
              :do (remhash id index))))


(defgeneric entity-created (entity)
  (:method ((entity entity)) nil)
  (:documentation
  "Called after an entity has been created and indexed.

  The default method does nothing, but users can implement their own auxillary
  methods to run code when entities are created.

  "))

(defgeneric entity-destroyed (entity)
  (:method ((entity entity)) nil)
  (:documentation
  "Called after an entity has been destroyed and unindexed.

  The default method does nothing, but users can implement their own auxillary
  methods to run code when entities are destroyed.

  "))


(defun create-entity (class &rest initargs)
  "Create an entity of the given entity class and return it.

  `initargs` will be passed along to `make-instance`.

  The `entity-created` generic function will be called just before returning the
  entity.

  "
  (let ((entity (apply #'make-instance class initargs)))
    (index-entity entity)
    (index-entity-aspects entity)
    (index-entity-systems entity)
    (entity-created entity)
    entity))

(defun destroy-entity (entity)
  "Destroy `entity` and return it.

  The `entity-destroyed` generic function will be called after the entity has
  been destroyed and unindexed.

  "
  (let ((id (entity-id entity)))
    (unindex-entity id)
    (unindex-entity-aspects id)
    (unindex-entity-systems id))
  (entity-destroyed entity)
  entity)

(defun clear-entities ()
  "Destroy all entities.

  `destroy-entity` will be called for each entity.

  Returns a list of all the destroyed entites.

  "
  (mapc #'destroy-entity (hash-table-values *entity-index*)))


(defun get-entity (id)
  "Return the entity with the given `id`, or `nil` if it is unknown."
  (gethash id *entity-index*))

(defun all-entities ()
  "Return a list of all entities."
  (hash-table-values *entity-index*))

(defun map-entities (function &optional (type 'entity))
  "Map `function` over all entities that are subtypes of `type`.

  Normally you should run code on entities using systems, but this function can
  be handy for debugging purposes.

  "
  (mapcar function
          (remove-if-not (lambda (entity) (typep entity type))
                         (hash-table-values *entity-index*))))


(defmacro define-entity (name aspects &rest slots)
  "Define an entity class.

  `name` should be a symbol that will become the name of the class.

  `aspects` should be a list of the aspects this entity should inherit from.

  `slots` can be zero or more extra CLOS slot definitions.

  Examples:

    (define-entity potion (drinkable))

    (define-entity cheese (edible visible)
      (flavor :accessor cheese-flavor :initarg :flavor))

  "
  `(progn
    (defclass ,name (entity ,@aspects)
      ((%beast/aspects :allocation :class :initform ',aspects)
       ,@slots))
    (defun ,(symb name '?) (object)
      (typep object ',name))
    (find-class ',name)))


;;;; Aspects ------------------------------------------------------------------
(defun initialize-aspect-index (name)
  (when (not (hash-table-key-exists-p *aspect-index* name))
    (setf (gethash name *aspect-index*) (make-hash-table))))

(defmacro define-aspect (name &rest fields)
  "Define an aspect class.

  `name` should be a symbol that will become the name of the class.

  `fields` should be zero or more field definitions.  Each field definition can
  be a symbol (the field name), or a list of the field name and extra CLOS slot
  options.

  Field names will have the aspect name and a slash prepended to them to create
  the slot names.  `:initarg` and `:accessor` slot options will also be
  automatically generated.

  Example:

    (define-aspect edible
      energy
      (taste :initform nil))
    =>
    (defclass edible ()
      ((edible/energy :initarg :edible/energy
                      :accessor edible/energy)
       (edible/taste :initarg :edible/taste
                     :accessor edible/taste
                     :initform nil)))

  "
  (flet ((clean-field (f)
           (etypecase f
             (symbol (list f))
             (list f))))
    `(progn
      (defclass ,name ()
        ,(loop
           :for (field . field-options) :in (mapcar #'clean-field fields)
           :for field-name = (symb name '/ field)
           :collect `(,field-name
                      :accessor ,field-name
                      :initarg ,(ensure-keyword field-name) ; *opens trenchcoat*
                      ,@field-options)))

      (defun ,(symb name '?) (object)
        (typep object ',name))

      (initialize-aspect-index ',name)

      (find-class ',name))))


;;;; Systems ------------------------------------------------------------------
(defun rebuild-system-index (arglist)
  (loop
    :for (nil . type-specifier) :in arglist
    :for index = (make-hash-table)
    :do (loop
          :for entity :being :the hash-values :of *entity-index*
          :when (entity-satisfies-system-type-specifier-p entity type-specifier)
          :do (setf (gethash (entity-id entity) index) entity))
    :collect index))

(defun initialize-system-index (name function arglist)
  (setf (gethash name *systems*)
        (list function (length arglist) (mapcar #'cdr arglist))

        (gethash name *system-index*)
        (rebuild-system-index arglist)))


(defun build-system-runner-1 (name type-specifiers)
  (with-gensyms (argument-indexes entity)
    `(let ((,argument-indexes (gethash ',name *system-index*)))
      (loop :for ,entity :being :the hash-values :of (first ,argument-indexes)
            :do (locally
                  (declare (type ,(first type-specifiers) ,entity))
                  (,name ,entity))))))

(defun build-system-runner-2 (name type-specifiers)
  (with-gensyms (argument-indexes e1 e2)
    `(let ((,argument-indexes (gethash ',name *system-index*)))
      (loop
        :for ,e1 :being :the hash-values :of (first ,argument-indexes)
        :do (loop :for ,e2 :being :the hash-values :of (second ,argument-indexes)
                  :do (locally
                        (declare (type ,(first type-specifiers) ,e1)
                                 (type ,(second type-specifiers) ,e2))
                        (,name ,e1 ,e2)))))))

(defun build-system-runner-n (name)
  `(apply #'map-product #',name
    (mapcar #'hash-table-values (gethash ',name *system-index*))))


(defun build-system-runner (name arity type-specifiers)
  (case arity
    (0 nil)
    (1 (build-system-runner-1 name type-specifiers))
    (2 (build-system-runner-2 name type-specifiers))
    (t (build-system-runner-n name))))


(defmacro define-system (name-and-options arglist &body body)
  "Define a system.

  `name-and-options` should be a list of the system name (a symbol) and any
  system options.  A bare symbol can be used if no options are needed.

  `arglist` should be a list of system arguments.  Each argument should be
  a list of the argument name and zero or more aspect/entity classes.

  Defining a system `foo` defines two functions:

  * `foo` runs `body` on a single entity and should only be used for debugging,
    tracing, or disassembling.
  * `run-foo` should be called to run the system on all applicable entities.

  Available system options:

  * `:inline`: when true, try to inline the system function into the
    system-running function to avoid the overhead of a function call for every
    entity.  Defaults to `nil`.

  Examples:

    (define-system age ((entity lifetime))
      (when (> (incf (lifetime/age entity))
               (lifetime/lifespan entity))
        (destroy-entity entity)))

  "
  (let ((argument-type-specifiers
          (loop :for arg :in (mapcar #'ensure-list arglist)
                :collect `(and entity ,@(cdr arg)))))
    (destructuring-bind (name &key inline)
        (ensure-list name-and-options)
      `(progn
        (declaim (ftype (function (,@argument-type-specifiers)
                                  (values null &optional))
                        ,name)
                 ,(if inline
                    `(inline ,name)
                    `(notinline ,name)))
        (defun ,name (,@(mapcar #'car arglist))
          ,@body
          nil)

        (defun ,(symb 'run- name) ()
          ,(build-system-runner name (length arglist) argument-type-specifiers))

        (initialize-system-index ',name #',name ',arglist)

        ',name))))

