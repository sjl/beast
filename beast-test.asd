(asdf:defsystem :beast-test
  :depends-on (:1am :beast)

  :serial t
  :components ((:file "package-test")
               (:module "test"
                :serial t
                :components ((:file "test"))))

  :perform
  (asdf:test-op (op system) (uiop:symbol-call :beast-test :run-tests)))
