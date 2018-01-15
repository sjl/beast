(asdf:defsystem :beast
  :description "Basic Entity/Aspect/System Toolkit"
  :author "Steve Losh <steve@stevelosh.com>"
  :homepage "https://sjl.bitbucket.io/beast/"

  :license "MIT"
  :version "1.1.0"

  :depends-on ()

  :serial t
  :components ((:module "vendor"
                :serial t
                :components ((:file "quickutils-package")
                             (:file "quickutils")))
               (:file "package")
               (:module "src"
                :serial t
                :components ((:file "beast"))))

  :in-order-to ((asdf:test-op (asdf:test-op :beast-test))))
