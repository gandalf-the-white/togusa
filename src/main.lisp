(uiop:define-package togusa
  (:use #:cl)
  (:import-from :cl-json :decode-json-from-string :encode-json-to-string)
  (:import-from :uiop :read-file-string))

(in-package #:togusa)

;;; -------------------------------------------------------------------
;;; Structures
;;; -------------------------------------------------------------------

(defclass host ()
  ((name   :initarg :name   :accessor host-name)
   (role   :initarg :role   :accessor host-role)
   (region :initarg :region :accessor host-region)
   (zone   :initarg :zone   :accessor host-zone)))

(defclass cluster ()
  ((name  :initarg :name  :accessor cluster-name)
   (hosts :initarg :hosts :accessor cluster-hosts)))

;;; -------------------------------------------------------------------
;;; Lecture du cluster.json
;;; -------------------------------------------------------------------

(defun load-cluster (path)
  "Lit cluster.json et retourne une instance de CLUSTER."
  (let* ((data (decode-json-from-string (read-file-string path)))
         (cluster (cdr (assoc :cluster data)))
         (hosts (cdr (assoc :hosts cluster))))
    (make-instance 'cluster
                   :name (cdr (assoc :name cluster))
                   :hosts (mapcar (lambda (h)
                                    (make-instance 'host
                                                   :name (cdr (assoc :name h))
                                                   :role (cdr (assoc :role h))
                                                   :region (cdr (assoc :region h))
                                                   :zone (cdr (assoc :zone h))))
                                  hosts))))

;;; -------------------------------------------------------------------
;;; Génération des traits
;;; -------------------------------------------------------------------

(defun make-scaler-trait (min max)
  `(("type" . "scaler")
    ("properties" . (("min" . ,min)
                     ("max" . ,max)))))

(defun make-spread-trait (roles zones)
  (let ((spread '()))
    (dolist (r roles)
      (if zones
          (dolist (z zones)
            (push
             `(("name" . ,(format nil "~a-~a" r z))
               ("requirements" .
                               ((("key" . "role")("operator" . "In")("values" . (,r)))
                                (("key" . "zone")("operator" . "In")("values" . (,z))))))
             spread))
          (push
           `(("name" . ,r)
             ("requirements" .
                             ((("key" . "role")("operator" . "In")("values" . (,r))))))
           spread)))
    `(("type" . "spread")
      ("properties" . (("spread" . ,(nreverse spread)))))))

;;; -------------------------------------------------------------------
;;; Blocs de manifest
;;; -------------------------------------------------------------------

(defun component (name image min max roles zones)
  `(("name" . ,name)
    ("type" . "actor")
    ("properties" . (("image" . ,image)))
    ("traits" . ,(list (make-scaler-trait min max)
                       (make-spread-trait roles zones)))))

(defun provider (name image capid roles zones)
  `(("name" . ,name)
    ("type" . "capability")
    ("properties" . (("image" . ,image)
                     ("capabilityId" . ,capid)))
    ("traits" . ,(list (make-spread-trait roles zones)))))

(defun link (source target &optional values)
  `(("source" . ,source)
    ("target" . ,target)
    ,@(when values `(("values" . ,values)))))

;;; -------------------------------------------------------------------
;;; Manifest principal
;;; -------------------------------------------------------------------

(defun generate-manifest (cluster)
  (let* ((zones (remove-duplicates (mapcar #'host-zone (cluster-hosts cluster)) :test #'string=))
         (roles (remove-duplicates (mapcar #'host-role (cluster-hosts cluster)) :test #'string=)))
    `(("apiVersion" . "core.oam.dev/v1alpha2")
      ("kind" . "Application")
      ("metadata" . (("name" . ,(cluster-name cluster))))
      ("spec" .
              (("components" .
                             ,(list
                               (component "http-api" "ghcr.io/acme/http-api:1.0" 2 5 roles zones)
                               (component "ingestor" "ghcr.io/acme/ingestor:0.9" 1 4 '("leaf") zones)))
               ("capabilityProviders" .
                                      ,(list
                                        (provider "redis" "ghcr.io/wasmcloud/keyvalue-redis:0.23"
                                                  "wasmcloud:keyvalue" roles zones)
                                        (provider "httpserver" "ghcr.io/wasmcloud/http-server:0.19"
                                                  "wasmcloud:httpserver" '("leaf") zones)))
               ("links" .
                        ,(list
                          (link "http-api" "httpserver" '(("address" . "0.0.0.0:8080")))
                          (link "http-api" "redis" '(("URL" . "redis://redis:6379")))
                          (link "ingestor" "redis"))))))))

;;; -------------------------------------------------------------------
;;; Écriture JSON
;;; -------------------------------------------------------------------

(defun write-json (data path)
  (with-open-file (s path :direction :output :if-exists :supersede)
    (write-string (encode-json-to-string data) s))
  (format t "✅ Manifest JSON écrit dans ~a~%" path))

;;; -------------------------------------------------------------------
;;; Point d’entrée
;;; -------------------------------------------------------------------

(defun main (&key (cluster-file "cluster.json") (output "manifest.json"))
  (let* ((cluster (load-cluster cluster-file))
         (manifest (generate-manifest cluster)))
    (write-json manifest output)
    (format t "Cluster: ~a (~d hôtes)~%"
            (cluster-name cluster)
            (length (cluster-hosts cluster)))))
