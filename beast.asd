(asdf:defsystem #:beast
  :name "beast"
  :description "Basic Entity/Aspect/System Toolkit"

  :author "Steve Losh <steve@stevelosh.com>"

  :license "MIT/X11"
  :version "1.0.0"

  :depends-on ()

  :serial t
  :components ((:module "vendor"
                :serial t
                :components ((:file "quickutils")))
               (:file "package")
               (:module "src"
                :serial t
                :components ((:file "beast"))))

  :in-order-to ((asdf:test-op (asdf:test-op #:beast-test))))


(asdf:defsystem #:beast-test
  :name "beast-test"

  :depends-on (#:1am)

  :serial t
  :components ((:file "package-test")
               (:module "test"
                :serial t
                :components ((:file "test"))))

  :perform
  (asdf:test-op (op system) (uiop:symbol-call :beast-test :run-tests)))
