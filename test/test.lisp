(in-package :beast/test)

;;;; Boilerplate --------------------------------------------------------------
(defmacro define-test (name &body body)
  `(test ,(beast::symb 'test/ name)
    (let ((*package* ,*package*)
          (*callbacks* nil))
      (clear-entities)
      ,@body)))

(defun run-tests ()
  (1am:run))

(defun set-equal (a b)
  (null (set-exclusive-or a b :test 'equal)))


;;;; Setup --------------------------------------------------------------------
(defparameter *results* nil)
(defparameter *callbacks* nil)

(define-aspect a-foo f)
(define-aspect a-bar b)
(define-aspect a-baz z)
(define-aspect a-callbacks-foo)
(define-aspect a-callbacks-bar)

(define-system sys-everything ((e))
  (push e *results*))

(define-system sys-foo ((e a-foo))
  (push e *results*))

(define-system sys-bar ((e a-bar))
  (push e *results*))

(define-system sys-foo-bar ((e a-foo a-bar))
  (push e *results*))

(define-system sys-foo-bar-baz ((e a-foo a-bar a-baz))
  (push e *results*))


(define-system sys2-foo-foo ((x a-foo) (y a-foo))
  (push (list x y) *results*))

(define-system sys2-foo-bar ((x a-foo) (y a-bar))
  (push (list x y) *results*))

(define-system sys2-foobar-foo ((x a-foo a-bar) (y a-foo))
  (push (list x y) *results*))

(define-system sys2-foobar-barbaz ((x a-foo a-bar) (y a-bar a-baz))
  (push (list x y) *results*))

(define-system sys3-foo-bar-baz ((x a-foo) (y a-bar) (z a-baz))
  (push (list x y z) *results*))


(define-entity e ())
(define-entity e-foo (a-foo))
(define-entity e-bar (a-bar))
(define-entity e-baz (a-baz))
(define-entity e-foo-bar (a-foo a-bar))
(define-entity e-foo-bar-baz (a-foo a-bar a-baz))

(define-entity e-callbacks-foo (a-callbacks-foo))
(define-entity e-callbacks-bar (a-callbacks-bar))
(define-entity e-callbacks-foobarplus (a-callbacks-foo a-callbacks-bar))

(defmethod entity-created :after ((e a-callbacks-foo))        (push (list :created :foo e) *callbacks*))
(defmethod entity-created :after ((e a-callbacks-bar))        (push (list :created :bar e) *callbacks*))
(defmethod entity-created :after ((e e-callbacks-foobarplus)) (push (list :created :ent e) *callbacks*))

(defmethod entity-destroyed :after ((e a-callbacks-foo))        (push (list :destroyed :foo e) *callbacks*))
(defmethod entity-destroyed :after ((e a-callbacks-bar))        (push (list :destroyed :bar e) *callbacks*))
(defmethod entity-destroyed :after ((e e-callbacks-foobarplus)) (push (list :destroyed :ent e) *callbacks*))


;;;; Tests --------------------------------------------------------------------
(define-test create-entities
  (let ((a (create-entity 'e))
        (b (create-entity 'e)))
    (is (set-equal (list a b) (beast:all-entities)))
    (let ((c (create-entity 'e)))
      (is (set-equal (list a b c) (beast:all-entities))))))

(define-test destroy-entities
  (let ((a (create-entity 'e))
        (b (create-entity 'e)))
    (is (set-equal (list a b) (beast:all-entities)))
    (destroy-entity a)
    (is (set-equal (list b) (beast:all-entities)))
    (let ((c (create-entity 'e)))
      (is (set-equal (list b c) (beast:all-entities)))
      (destroy-entity b)
      (is (set-equal (list c) (beast:all-entities)))
      (destroy-entity c)
      (is (set-equal (list) (beast:all-entities))))))

(define-test clear-entities
  (let ((a (create-entity 'e))
        (b (create-entity 'e)))
    (is (set-equal (list a b) (beast:all-entities)))
    (clear-entities)
    (is (set-equal (list) (beast:all-entities)))))

(define-test callbacks
  (let (*callbacks* f b fbp)
    (is (set-equal `() *callbacks*))

    (setf f (create-entity 'e-callbacks-foo))
    (is (set-equal `((:created :foo ,f)) *callbacks*))
    (setf *callbacks* nil)

    (setf b (create-entity 'e-callbacks-bar))
    (is (set-equal `((:created :bar ,b)) *callbacks*))
    (setf *callbacks* nil)

    (setf fbp (create-entity 'e-callbacks-foobarplus))
    (is (set-equal `((:created :bar ,fbp)
                     (:created :foo ,fbp)
                     (:created :ent ,fbp)) *callbacks*))
    (setf *callbacks* nil)

    (destroy-entity fbp)
    (is (set-equal `((:destroyed :bar ,fbp)
                     (:destroyed :foo ,fbp)
                     (:destroyed :ent ,fbp)) *callbacks*))
    (setf *callbacks* nil)

    (destroy-entity f)
    (is (set-equal `((:destroyed :foo ,f)) *callbacks*))
    (setf *callbacks* nil)

    (destroy-entity b)
    (is (set-equal `((:destroyed :bar ,b)) *callbacks*))))

(define-test map-entities
  (create-entity 'e-foo :a-foo/f 1)
  (create-entity 'e-foo :a-foo/f 2)
  (is (set-equal (list 1 2) (beast:map-entities #'a-foo/f)))
  (create-entity 'e-foo :a-foo/f 3)
  (is (set-equal (list 1 2 3) (beast:map-entities #'a-foo/f)))
  (create-entity 'e-bar :a-bar/b 0)
  (is (set-equal (list 1 2 3) (beast:map-entities #'a-foo/f 'a-foo)))
  (is (set-equal (list 0) (beast:map-entities #'a-bar/b 'a-bar))))

(define-test get-entities
  (let ((a (create-entity 'e))
        (b (create-entity 'e)))
    (is (eq a (beast::get-entity (entity-id a))))
    (is (eq b (beast::get-entity (entity-id b))))))

(define-test aspect-mixins
  (let ((f (create-entity 'e-foo :a-foo/f :foo))
        (b (create-entity 'e-bar :a-bar/b :bar))
        (fb (create-entity 'e-foo-bar
                           :a-foo/f :foo
                           :a-bar/b :bar)))
    (is (eql (a-foo/f f) :foo))
    (is (eql (a-bar/b b) :bar))
    (is (eql (a-foo/f fb) :foo))
    (is (eql (a-bar/b fb) :bar))))

(define-test system-running-arity-1
  (let ((f1 (create-entity 'e-foo :a-foo/f :foo))
        (f2 (create-entity 'e-foo :a-foo/f :foo))
        (b1 (create-entity 'e-bar :a-bar/b :bar))
        (b2 (create-entity 'e-bar :a-bar/b :bar))
        (fb (create-entity 'e-foo-bar :a-foo/f :foo :a-bar/b :bar)))
    (let ((*results* nil))
      (run-sys-everything)
      (is (set-equal *results* (list f1 f2 b1 b2 fb))))

    (let ((*results* nil))
      (run-sys-foo)
      (is (set-equal *results* (list f1 f2 fb))))

    (let ((*results* nil))
      (run-sys-bar)
      (is (set-equal *results* (list b1 b2 fb))))

    (let ((*results* nil))
      (run-sys-foo-bar)
      (is (set-equal *results* (list fb))))))

(define-test system-running-arity-2
  (let ((f (create-entity 'e-foo :a-foo/f :foo))
        (b (create-entity 'e-bar :a-bar/b :bar))
        (fb (create-entity 'e-foo-bar :a-foo/f :foo :a-bar/b :bar)))
    (let ((*results* nil))
      (run-sys2-foo-foo)
      (is (set-equal *results* (list (list f f)
                                     (list f fb)
                                     (list fb f)
                                     (list fb fb)))))

    (let ((*results* nil))
      (run-sys2-foo-bar)
      (is (set-equal *results* (list (list f b)
                                     (list f fb)
                                     (list fb b)
                                     (list fb fb)))))

    (let ((*results* nil))
      (run-sys2-foobar-foo)
      (is (set-equal *results* (list (list fb f)
                                     (list fb fb)))))))

(define-test system-running-arity-3
  (let ((f (create-entity 'e-foo))
        (b (create-entity 'e-bar))
        (z (create-entity 'e-baz))
        (fb (create-entity 'e-foo-bar))
        (fbz (create-entity 'e-foo-bar-baz)))
    (let ((*results* nil))
      (run-sys3-foo-bar-baz)
      (is (set-equal *results* (list
                                 (list f     b     z)
                                 (list f     b   fbz)
                                 (list f    fb     z)
                                 (list f    fb   fbz)
                                 (list f    fbz    z)
                                 (list f    fbz  fbz)

                                 (list fb    b     z)
                                 (list fb    b   fbz)
                                 (list fb   fb     z)
                                 (list fb   fb   fbz)
                                 (list fb   fbz    z)
                                 (list fb   fbz  fbz)

                                 (list fbz   b     z)
                                 (list fbz   b   fbz)
                                 (list fbz  fb     z)
                                 (list fbz  fb   fbz)
                                 (list fbz  fbz    z)
                                 (list fbz  fbz  fbz)))))))
