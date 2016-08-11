(in-package #:beast)

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


;;;; Entities
(defvar *entity-id-counter* 0)
(defvar *entity-index* (make-hash-table))


(defclass entity ()
  ((id :reader entity-id :initform (incf *entity-id-counter*))
   (%beast/aspects :allocation :class :initform nil)))

(defmethod print-object ((e entity) stream)
  (print-unreadable-object (e stream :type t :identity nil)
    (format stream "~D" (entity-id e))))


(defun get-entity (id)
  (gethash id *entity-index*))

(defun all-entities ()
  (hash-table-values *entity-index*))

(defun map-entities (function &optional (type 'entity))
  (mapcar function
          (remove-if-not (lambda (entity) (typep entity type))
                         (hash-table-values *entity-index*))))

(defun clear-entities ()
  (mapc #'destroy-entity (hash-table-values *entity-index*)))


(defun entity-satisfies-system-type-specifier-p (entity specifier)
  (every (lambda (aspect) (typep entity aspect))
         specifier))

(defun index-entity (entity)
  (setf (gethash (entity-id entity) *entity-index*) entity))

(defun index-entity-aspects (entity)
  (loop :for aspect :in (slot-value entity '%beast/aspects)
        :do (setf (gethash (entity-id entity)
                           (gethash aspect *aspect-index*))
                  entity)))

(defun index-entity-systems (entity)
  (loop
    :with id = (entity-id entity)
    :for system :being :the hash-keys :of *systems*
    :using (hash-value (function arity type-specifiers))
    :do (loop :for argument-index :in (gethash system *system-index*)
              :for specifier :in type-specifiers
              :when (entity-satisfies-system-type-specifier-p entity specifier)
              :do (setf (gethash id argument-index) entity))))


(defun unindex-entity (id)
  (remhash id *entity-index*))

(defun unindex-entity-aspects (id)
  (loop
    :for index :being :the hash-values :of *aspect-index*
    :do (remhash id index)))

(defun unindex-entity-systems (id)
  (loop
    :for argument-indexes :being :the hash-values :of *system-index*
    :do (loop :for index :in argument-indexes
              :do (remhash id index))))


(defgeneric entity-created (entity)
  (:method ((entity entity)) nil))

(defgeneric entity-destroyed (entity)
  (:method ((entity entity)) nil))


(defun create-entity (class &rest initargs)
  (let ((entity (apply #'make-instance class initargs)))
    (index-entity entity)
    (index-entity-aspects entity)
    (index-entity-systems entity)
    (entity-created entity)
    entity))

(defun destroy-entity (entity)
  (let ((id (entity-id entity)))
    (unindex-entity id)
    (unindex-entity-aspects id)
    (unindex-entity-systems id))
  (entity-destroyed entity)
  entity)


(defmacro define-entity (name aspects &rest slots)
  `(progn
    (defclass ,name (entity ,@aspects)
      ((%beast/aspects :allocation :class :initform ',aspects)
       ,@slots))
    (defun ,(symb name '?) (object)
      (typep object ',name))
    (find-class ',name)))


;;;; Aspects
(defvar *aspect-index* (make-hash-table))

(defun initialize-aspect-index (name)
  (when (not (hash-table-key-exists-p *aspect-index* name))
    (setf (gethash name *aspect-index*) (make-hash-table))))

(defmacro define-aspect (name &rest fields)
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


;;;; Systems
(defvar *system-index* (make-hash-table))
(defvar *systems* (make-hash-table))


(defun rebuild-system-index (arglist)
  (loop
    :for (argument-name . type-specifier) :in arglist
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
  (let ((argument-type-specifiers (loop :for arg :in arglist
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

