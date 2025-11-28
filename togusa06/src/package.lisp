(defpackage :togusa
  (:use :cl)
  (:import-from :sqlite
   :execute-to-list :connect
   :disconnect :execute-non-query
   :execute-single)
  (:import-from :cl-json
   :decode-json-from-string :encode-json-to-string)
  (:import-from :hunchentoot
   :define-easy-handler))


(in-package :togusa)

(defparameter *api-port* 8080)
(defparameter *server* nil)
