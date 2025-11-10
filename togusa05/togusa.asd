(in-package :asdf-user)

(defsystem "togusa"
  :version "0.0.1"
  :author ""
  :license ""
  :depends-on (cl-json
               sqlite
               hunchentoot
               easy-routes
               com.inuoe.jzon)
  :components ((:module "src"
                :components
                ((:file "dbase" :depends-on ("wasmcloud"))
                 (:file "wasmcloud")
                 (:file "api" :depends-on ("dbase" "wasmcloud"))
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
