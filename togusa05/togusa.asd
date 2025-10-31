(in-package :asdf-user)

(defsystem "togusa"
  :version "0.0.1"
  :author ""
  :license ""
  :depends-on (cl-json
               sqlite)
  :components ((:module "src"
                :components
                ((:file "wasmcloud")
                 (:file "main"))))
  :description ""
  :in-order-to ((test-op (test-op "togusa/tests"))))

(defsystem "togusa/tests"
  :author ""
  :license ""
  :depends-on ("togusa"
               "rove")
  :components ((:module "tests"
                :components
                ((:file "main"))))
  :description "Test system for togusa"
  :perform (test-op (op c) (symbol-call :rove :run c)))
