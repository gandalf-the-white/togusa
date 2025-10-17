(in-package :asdf-user)

(defsystem "togusa"
  :version "0.0.1"
  :author ""
  :license ""
  :depends-on (cl-json
               sqlite
               hunchentoot
               easy-routes
               clack
               com.inuoe.jzon)
  :components ((:module "src"
                :components
                ((:file "db")
                 (:file "wasmcloud")
                 (:file "api")
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
                ((:file "wasmcloud")
                 (:file "main"))))
  :description "Test system for togusa"
  :perform (test-op (op c) (symbol-call :rove :run c)))
