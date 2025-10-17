(defpackage :api
  (:use :cl :db :wasmcloud)
  (:local-nicknames (:jzon :com.inuoe.jzon))
  (:import-from :cl-json :decode-json-from-string :encode-json-to-string)
  (:import-from :easy-routes :defroute))

(in-package :api)

(defparameter +default-port+ 8080)
(defparameter *db-path* db:*manifest-db*)
(defparameter *acceptor* nil)

(db:init-db *db-path*)

;;;; =====================
;;;; JSON Tools
;;;; =====================

;; (defun json-response (obj &key (code 200))
;;   (list code '(:content-type "application/json")
;;         (list (if (stringp obj)
;;                   obj
;;                   (encode-json-to-string obj)))))

;; (defun json-response (obj &key (code 200))
;;  (setf (reply-content-type) "application/json")
;;  (list code
;;        '(:content-type "application/json")
;;        (list (jzon:stringify obj))))

;;;; =====================
;;;; Easy Routes
;;;; =====================

(setf *acceptor* (make-instance 'easy-routes:routes-acceptor
                                :address "127.0.0.1"
                                :port +default-port+))

;; (defroute hello ("/" :method :GET) ()
;; (format nil "Welcome to the wasmcloud orchestrator!"))

(defroute create-manifest ("/manifest" :method :POST) ()
  (setf (hunchentoot:content-type*) "application/json")
  (let ((response (make-hash-table))
        (body (jzon:parse (hunchentoot:raw-post-data))))
    (let ((description (gethash "description" body))
          (version (gethash "version" body))
          (name (gethash "name" body)))
      (setf (gethash "name" response) name
            (gethash "version" response) version
            (gethash "description" response) description)
      
      (let ((m (wasmcloud:make-manifest :name name :version version :description description)))
        (wasmcloud:save-manifest-to-db m)))
    (jzon:stringify response)))

(defroute get-manifest ("/manifest/:name" :method :GET)
  (&path (name 'string))
  (let ((m (wasmcloud:load-manifest-from-db name)))
    (setf (hunchentoot:content-type*) "application/json")
    (wasmcloud:manifest->json-string m)))

(defroute update-manifest ("/manifest/:name" :method :PATCH)
  (&path (name 'string))
  (setf (hunchentoot:content-type*) "application/json")
  (let* ((body (jzon:parse (hunchentoot:raw-post-data)))
         (m (wasmcloud:load-manifest-from-db name)))
    (dolist (c (manifest-components m))
      (let ((config (component-config c)))
        (when (gethash "instances" body)
          (setf (getf config :instances) (gethash "instances" body)))
        (when (gethash "spread" body)
          (setf (getf config :spread)
                (mapcar (lambda (s)
                          (list
                           :name (gethash "name" s)
                           :weight (gethash "weight" s)
                           :requirements (list :zone (gethash "zone" (gethash "requirements" s)))))
                        (gethash "spread" body))))))
    (wasmcloud:save-manifest-to-db m)
    (json-response (list :status "updated"))))

;;;; =====================
;;;; Handle Server
;;;; =====================

(defun start-accesptor ()
  (hunchentoot:start *acceptor*)
  (princ "Server started"))

(defun stop-acceptor ()
  (hunchentoot:stop *acceptor*)
  (princ "Server stopped"))
