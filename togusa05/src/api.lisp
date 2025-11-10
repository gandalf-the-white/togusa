(defpackage :api
  (:use :cl :dbase :wasmcloud)
  (:local-nicknames (:jzon :com.inuoe.jzon))
  (:import-from :cl-json :decode-json-from-string :encode-json-to-string)
  (:import-from :easy-routes :defroute))

(in-package :api)

;; ================================================
;; C O M M O N  
;; ================================================

(defparameter *default-port* 8080)
(defparameter *acceptor* nil)

;; ================================================
;; E A S Y R O U T E S 
;; ================================================

(setf *acceptor* (make-instance 'easy-routes:routes-acceptor
                                :address "127.0.0.1"
                                :port *default-port*))

(defun start-accesptor ()
  (hunchentoot:start *acceptor*)
  (princ "Server started"))

(defun stop-acceptor ()
  (hunchentoot:stop *acceptor*)
  (princ "Server stopped"))
