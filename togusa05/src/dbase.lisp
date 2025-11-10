(defpackage :dbase
  (:use :cl :wasmcloud)
  (:export :init-db :*manifest-db*
           :with-db :save-node
           :save-cluster))

(in-package :dbase)

(defparameter *manifest-db* nil)

(defmacro with-db ((db-path) &body body)
  `(let ((*manifest-db* (sqlite::connect ,db-path)))
     (unwind-protect
          (progn ,@body)
       (sqlite::disconnect *manifest-db*))))

;; ================================================
;;  S C H E M A
;; ================================================

(defun init-db ()
  (sqlite:execute-non-query
   *manifest-db*
   "CREATE TABLE IF NOT EXISTS nodes (
         id TEXT PRIMARY KEY,
         type TEXT NOT NULL,
         zone TEXT NOT NULL
      );")
  (sqlite:execute-non-query
   *manifest-db*
   "CREATE TABLE IF NOT EXISTS node_labels (
         node_id TEXT NOT NULL,
         label TEXT NOT NULL,
         PRIMARY KEY (node_id, label),
         FOREIGN KEY (node_id) REFERENCES nodes(id)
      );")
  (sqlite:execute-non-query
   *manifest-db*
   "CREATE TABLE IF NOT EXISTS clusters (
         name TEXT PRIMARY KEY,
         nats_url TEXT NOT NULL
      );")
  (sqlite:execute-non-query
   *manifest-db*
   "CREATE TABLE IF NOT EXISTS cluster_nodes (
         cluster_name TEXT NOT NULL,
         node_id TEXT NOT NULL,
         PRIMARY KEY (cluster_name, node_id),
         FOREIGN KEY (cluster_name) REFERENCES clusters(name),
         FOREIGN KEY (node_id) REFERENCES nodes(id)
      );")
  (sqlite:execute-non-query
   *manifest-db*
   "CREATE TABLE IF NOT EXISTS manifest (
         name TEXT PRIMARY KEY
      );")
  (sqlite:execute-non-query
   *manifest-db*
   "CREATE TABLE IF NOT EXISTS cluster_manifest (
         cluster_name TEXT NOT NULL,
         manifest_name TEXT NOT NULL,
         PRIMARY KEY (cluster_name, manifest_name),
         FOREIGN KEY (cluster_name) REFERENCES clusters(name),
         FOREIGN KEY (manifest_name) REFERENCES manifest(name)
      );"))

;; ================================================
;;  C L U S T E R
;; ================================================

(defmethod save-node ((node node))
  (let ((type (if (typep node  'worker) "worker" "leaf")))
    (sqlite:execute-non-query
     *manifest-db*
     "INSERT OR REPLACE INTO nodes (id, type, zone) VALUES (?, ?, ?)"
     (node-id node) type (node-zone node))
    (sqlite:execute-non-query
     *manifest-db*
     "DELETE FROM node_labels WHERE node_id = ?" (node-id node))
    (dolist (label (node-label node))
      (sqlite:execute-non-query
       *manifest-db*
       "INSERT INTO node_labels (node_id, label) VALUES (?, ?)"
       (node-id node) label))))

(defun load-node (id)
  (let* ((row (sqlite:execute-to-list
               *manifest-db*
               "SELECT id, type, zone FROM nodes WHERE id = ?"
               id))
         (labels (mapcar #'first
                         (sqlite:execute-to-list
                             *manifest-db*
                           "SELECT label FROM node_labels WHERE node_id = ?"
                           id))))
    (when (first row)
      (let* ((row (first row))
             (node (if (string= (second row) "worker")
                       (make-instance 'worker)
                       (make-instance 'leaf))))
        (setf (node-id node) (first row)
              (node-zone node) (third row)
              (node-label node) labels)
        node))))

(defmethod save-cluster ((cluster cluster))
  (let ((cluster-name (cluster-name cluster)))
    (sqlite:execute-non-query
     *manifest-db*
     "INSERT OR REPLACE INTO clusters (name, nats_url) VALUES (?, ?)"
     cluster-name (cluster-nats-url cluster))
    (sqlite:execute-non-query
     *manifest-db*
     "DELETE FROM cluster_nodes WHERE cluster_name = ?" cluster-name)
    (dolist (node (append (cluster-workers cluster) (cluster-leafs cluster)))
      (sqlite:execute-non-query
       *manifest-db*
       "INSERT INTO cluster_nodes (cluster_name, node_id) VALUES (?, ?)"
       cluster-name (node-id node)))))

(defun load-cluster (name)
  (let* ((row (sqlite:execute-to-list
               *manifest-db*
               "SELECT name, nats_url FROM clusters WHERE name = ?"
               name))
         (node-ids (sqlite:execute-to-list
                    *manifest-db*
                    "SELECT node_id FROM cluster_nodes WHERE cluster_name = ?"
                    name))
         (nodes (mapcar (lambda (id) (load-node (first id))) node-ids)))
    (when (first row)
      (let ((row (first row)))
        (make-cluster :name (first row)
                      :nats-url (second row)
                      :workers (remove-if-not (lambda (node) (if (typep node 'WORKER) node)) nodes)
                      :leafs (remove nil (mapcar (lambda (node) (if (typep node 'LEAF) node)) nodes)))))))

;; ================================================
;;  M A N I F E S T
;; ================================================

(defmethod save-manifest ((m manifest))
  (let* ((manifest-name (manifest-name m))
         (cluster (manifest-cluster m))
         (cluster-name (cluster-name cluster)))
    (sqlite:execute-non-query
     *manifest-db*
     "INSERT OR REPLACE INTO manifest (name) VALUES (?)"
     manifest-name)
    (sqlite:execute-non-query
     *manifest-db*
     "DELETE FROM cluster_manifest WHERE manifest_name = ?" manifest-name)
    (sqlite:execute-non-query
     *manifest-db*
     "INSERT INTO cluster_manifest (manifest_name, cluster_name) VALUES (?, ?)"
     manifest-name cluster-name)))

(defun load-manifest (name)
  (let* ((row (sqlite:execute-single
               *manifest-db*
               "SELECT cluster_name FROM cluster_manifest WHERE manifest_name = ?"
               name))
         (cluster (sqlite:execute-to-list
                   *manifest-db*
                   "SELECT name, nats_url FROM clusters WHERE name = ?"
                   row)))
    (format t "~a " (first (first cluster)))
    (when row
      (let* ((cluster-name (first(first cluster)))
             (cluster (load-cluster cluster-name)))
        (make-manifest :name name
                       :cluster cluster)))
    ))

;; ================================================
;; E X A M P L E  
;; ================================================

(defun test ()
  (with-db ("wasmcloud.db")
    (init-db)
    (let* ((worker1 (wasmcloud:make-worker :id "w1" :zone "zone-a" :label '("fast" "gpu")))
           (worker2 (wasmcloud:make-worker :id "w2" :zone "zone-a" :label '("fast" "gpu")))
           (worker3 (wasmcloud:make-worker :id "w3" :zone "zone-a" :label '("fast" "gpu")))
           (leaf1 (wasmcloud:make-leaf :id "l1" :zone "zone-b" :label '("slow" "cpu")))
           (leaf2 (wasmcloud:make-leaf :id "l2" :zone "zone-b" :label '("slow" "cpu")))
           (cluster (wasmcloud:make-cluster
                     :name "prod"
                     :nats-url "nats://prod:4222"
                     :leafs (list leaf1 leaf2)
                     :workers (list worker1 worker2 worker3)))
           (manifest (make-manifest :name "wasm" :cluster cluster)))
      (save-node worker1)
      (save-node worker2)
      (save-node worker3)
      (save-node leaf1)
      (save-node leaf2)
      (save-cluster cluster)
      (save-manifest manifest)))

  (with-db ("wasmcloud.db")
    (let ((cluster (load-cluster "prod"))
          (manifest (load-manifest "wasm")))
      ;; (format t "Cluster: ~a~%" (wasmcloud:cluster-name cluster))
      ;; (format t "Workers: ~a~%" (cluster-workers cluster))
      ;; (format t "Leafs: ~a~%" (cluster-leafs cluster))
      ;; (format t "Manifest: ~a~%" (manifest-cluster manifest))
      manifest
      )))

