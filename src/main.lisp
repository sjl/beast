(in-package :beast)


;;;; Notes
;;; Entities are stored in an {id -> entity} hash table.
;;;
;;; Entities are also indexed by aspect in a nested hash table:
;;;
;;;     {aspect-symbol -> {id -> entity}}
;;;
;;; Entities are indexed by system too, as a vector of hash tables, one entry
;;; for each of the system's arguments:
;;;
;;;     {system-symbol ->
;;;        #({id -> entity}   ; arg1
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


;;;; Utils --------------------------------------------------------------------
(defun symb (&rest args)
  (values (intern (format nil "~{~A~}" args))))


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
    :do (loop :for argument-index :across (gethash system *system-index*)
              :for specifier :in type-specifiers
              :when (entity-satisfies-system-type-specifier-p entity specifier)
              :do (setf (gethash id argument-index) entity))))


(defun unindex-entity (id)
  "Remove `entity` from the entity-level index."
  (remhash id *entity-index*))

(defun unindex-entity-aspects (id)
  "Remove `entity` from the aspect indexes."
  (loop
    :for index :being :the :hash-values :of *aspect-index*
    :do (remhash id index)))

(defun unindex-entity-systems (id)
  "Remove `entity` from the system indexes."
  (loop
    :for argument-indexes :being :the hash-values :of *system-index*
    :do (loop :for index :across argument-indexes
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
  (let ((entities (all-entities)))
    (mapc #'destroy-entity entities)
    entities))


(defun get-entity (id)
  "Return the entity with the given `id`, or `nil` if it is unknown."
  (gethash id *entity-index*))

(defun all-entities ()
  "Return a list of all entities.

  Normally you should run code on entities using systems, but this function can
  be handy for debugging purposes.

  "
  (loop :for entity :being :the :hash-values :of *entity-index* :collect entity))

(defun map-entities (function &optional (type 'entity))
  "Map `function` over all entities that are subtypes of `type`.

  Normally you should run code on entities using systems, but this function can
  be handy for debugging purposes.

  "
  (loop :for entity :being :the :hash-values :of *entity-index*
        :when (typep entity type)
        :collect (funcall function entity)))


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
  (when (not (gethash name *aspect-index*))
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
           (ctypecase f
             (symbol (list f))
             (list f))))
    `(progn
      (defclass ,name ()
        ,(loop
           :for (field . field-options) :in (mapcar #'clean-field fields)
           :for field-name = (symb name '/ field)
           :collect `(,field-name
                      :accessor ,field-name
                      :initarg ,(intern (string field-name) :keyword)
                      ,@field-options)))

      (defun ,(symb name '?) (object)
        (typep object ',name))

      (initialize-aspect-index ',name)

      (find-class ',name))))


;;;; Systems ------------------------------------------------------------------
(defun rebuild-system-index (arglist)
  (coerce (loop
            :for (nil . type-specifier) :in arglist
            :for index = (make-hash-table)
            :do (loop
                  :for entity :being :the :hash-values :of *entity-index*
                  :when (entity-satisfies-system-type-specifier-p entity type-specifier)
                  :do (setf (gethash (entity-id entity) index) entity))
            :collect index)
          'vector))

(defun initialize-system-index (name function arglist)
  (setf (gethash name *systems*)
        (list function (length arglist) (mapcar #'cdr arglist))

        (gethash name *system-index*)
        (rebuild-system-index arglist)))


(defun build-system-runner (name type-specifiers)
  (unless (null type-specifiers)
    (let ((argument-indexes (gensym "AI"))
          (arguments (loop :repeat (length type-specifiers) :collect (gensym "E"))))
      `(let ((,argument-indexes (gethash ',name *system-index*)))
         ,(labels ((recur (types args n)
                     (if (null types)
                       `(,name ,@arguments)
                       `(loop
                          :for ,(first args) :of-type ,(first types)
                          :being :the :hash-values :of (aref ,argument-indexes ,n)
                          :do ,(recur (rest types) (rest args) (1+ n))))))
            (recur type-specifiers arguments 0))))))


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
          (loop :for arg :in arglist ; either foo or (foo a1 a2)
                :for classes = (if (listp arg) (rest arg) nil)
                :collect `(and entity ,@classes))))
    (destructuring-bind (name &key inline) (if (listp name-and-options)
                                               name-and-options
                                               (list name-and-options))
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
          ,(build-system-runner name argument-type-specifiers))

        (initialize-system-index ',name #',name ',arglist)

        ',name))))

