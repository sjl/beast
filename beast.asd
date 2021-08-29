(asdf:defsystem :beast
  :description "Basic Entity/Aspect/System Toolkit"
  :author "Steve Losh <steve@stevelosh.com>"
  :homepage "https://docs.stevelosh.com/beast/"

  :license "MIT"
  :version "1.2.0"

  :depends-on ()

  :serial t
  :components ((:module "src"
                :serial t
                :components ((:file "package")
                             (:file "main"))))

  :in-order-to ((asdf:test-op (asdf:test-op :beast/test))))


(asdf:defsystem :beast/test
  :depends-on (:1am :beast)

  :serial t
  :components ((:module "test"
                :serial t
                :components ((:file "package")
                             (:file "test"))))

  :perform
  (asdf:test-op (op system) (uiop:symbol-call :beast/test :run-tests)))
