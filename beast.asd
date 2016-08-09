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
               (:file "beast")))
