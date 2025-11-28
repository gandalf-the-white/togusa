(defpackage :api
  (:use :cl :dbase :wasmcloud :easy-routes))

(in-package :api)

;; ================================================
;; S E R V E R
;; ================================================

(defparameter *server* nil)
(defparameter *port* 8000)


(defun start-server (&key (port 8000))
  (format t "~&Starting API on port ~a~&" port)
  (force-output)
  (with-db ("./datas/wasmcloud.db")
    (init-db))
  (setf *server*
        (hunchentoot:start
         (make-instance 'easy-routes:easy-routes-acceptor :port port))))

(defun stop-server ()
  (when *server*
    (hunchentoot:stop *server*)
    (setf *server* nil)))

;; ================================================
;;  H E L P E R
;; ================================================

(defun worker->plist (worker)
  (list :zone (wasmcloud::node-zone worker)
        :label (wasmcloud::node-label worker)
        :type "worker"))

(defun encode-json (data)
  (with-output-to-string (out)
    (yason:encode (data out))))

(defun parse-json-body ()
  (let ((text (raw-post-data :force-text t)))
    (when text
      (yason:parse text))))

;; ================================================
;;  C L U S T E R
;; ================================================

(defun create-worker (&key id zone label)
  (let ((worker (make-worker :id id :zone zone :label label)))
    (save-node worker)
    worker))

(defun find-worker (id)
  "Retourne le worker avec l'ID donné, ou NIL si ce n'est pas un worker ou n'existe pas."
  (let ((node (load-node id)))
    (when (and node (typep node 'worker))
      node)))

(defun list-workers ()
  "Retourne la liste de tous les workers (chargés avec load-node)."
  (let ((rows (sqlite:execute-to-list
               *manifest-db*
               "SELECT id FROM nodes WHERE type = 'worker'")))
    (mapcar (lambda (row)
              (load-node (first row)))
            rows)))

(defun update-worker (id &key zone label)
  "Met à jour le worker ID (zone et/ou label), le persiste, et le retourne.
Retourne NIL si l'ID n'existe pas ou n'est pas un worker."
  (let ((worker (find-worker id)))
    (when worker
      (when zone
        (setf (node-zone worker) zone))
      (when label
        (setf (node-label worker) label))
      (save-node worker)
      worker)))

(defun delete-worker (id)
  "Supprime le worker ID de la base.
Retourne T si quelque chose a été supprimé, NIL sinon."
  ;; On vérifie d'abord que c'est bien un worker
  (when (find-worker id)
    ;; Supprimer d'abord les labels
    (sqlite:execute-non-query
     *manifest-db*
     "DELETE FROM node_labels WHERE node_id = ?"
     id)
    ;; Puis le node lui-même (limité aux workers par sécurité)
    (sqlite:execute-non-query
     *manifest-db*
     "DELETE FROM nodes WHERE id = ? AND type = 'worker'"
     id)
    t))

;; ================================================
;;  W E B
;; ================================================

(defroute root ("/") ()
  "Hello!!")

;; ================================================
;; R O U T E 
;; ================================================


(defroute workers-create
    ("/workers" :method :post :decorators (@json))
    ()
  (handler-case
      (let* ((json (parse-json-body))
             (id (or (getf json :id)
                     (getf json "id")))
             (zone (or (getf json :zone)
                       (getf json "zone")))
             (label-raw (or (getf json :label)
                            (getf json "label")))
             (worker (create-worker :id id :zone zone :label label)))
        (setf (content-type*) "application/json; charset-utf-8"
              hunchentoot:*reply* 201)
        (encode-json (worker->plist worker)))
    (error (e)
      (setf hunchentoot:*reply* 400)
      (encode-json (list :error (format nil "~A" e))))))

;; ================================================
;; M E T H O D 
;; ================================================

(defun test ()
  (format t "API TEST~%"))
