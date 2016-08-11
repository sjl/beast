(in-package #:beast-test)

;;;; Boilerplate
(defmacro define-test (name &body body)
  `(test ,name
    (let ((*package* ,*package*))
      (clear-entities)
      ,@body)))

(defun run-tests ()
  (1am:run))

(defun set-equal (a b)
  (null (set-exclusive-or a b :test 'equal)))


;;;; Setup
(defparameter *results* nil)

(define-aspect a-foo f)
(define-aspect a-bar b)

(define-system sys-everything ((e))
  (push e *results*))

(define-system sys-foo ((e a-foo))
  (push e *results*))

(define-system sys-bar ((e a-bar))
  (push e *results*))

(define-system sys-foo-bar ((e a-foo a-bar))
  (push e *results*))


(define-system sys2-foo-foo ((x a-foo) (y a-foo))
  (push (list x y) *results*))

(define-system sys2-foo-bar ((x a-foo) (y a-bar))
  (push (list x y) *results*))

(define-system sys2-foobar-foo ((x a-foo a-bar) (y a-foo))
  (push (list x y) *results*))


(define-entity e ())
(define-entity e-foo (a-foo))
(define-entity e-bar (a-bar))
(define-entity e-foo-bar (a-foo a-bar))


;;;; Tests
(define-test create-entities
  (let ((a (create-entity 'e))
        (b (create-entity 'e)))
    (is (set-equal (list a b) (beast::all-entities)))
    (let ((c (create-entity 'e)))
      (is (set-equal (list a b c) (beast::all-entities))))))

(define-test get-entities
  (let ((a (create-entity 'e))
        (b (create-entity 'e)))
    (is (eq a (get-entity (entity-id a))))
    (is (eq b (get-entity (entity-id b))))))

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
