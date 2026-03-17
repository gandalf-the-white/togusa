(in-package :asdf-user)

(defsystem "togusa"
  :version "0.0.1"
  :author "spike spiegel"
  :license "MIT"
  :depends-on (:cl-json
               :sqlite
               :hunchentoot
               :easy-routes
               :yason)
  :components ((:module "src"
                :components
                ((:file "package")
                 (:file "utils")
                 (:file "wasmcloud")
                 (:file "dbase")
                 (:file "api")
                 (:file "main"))))
  :description "")

