(asdf:defsystem #:beast
  :name "beast"
  :description "Basic Entity/Aspect/System Toolkit"

  :author "Steve Losh <steve@stevelosh.com>"

  :license "MIT/X11"
  :version "0.0.1"

  :depends-on ()

  :serial t
  :components ((:file "quickutils")
               (:file "package")
               (:file "beast"))

  :in-order-to ((asdf:test-op (asdf:test-op #:beast-test))))


(asdf:defsystem #:beast-test
  :name "beast-test"

  :depends-on (#:1am)

  :serial t
  :components ((:file "package-test")
               (:file "test"))

  :perform (asdf:test-op (op system)
             (uiop:symbol-call :beast-test :run-tests)))
