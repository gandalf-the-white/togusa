(defpackage :wasmcloud
  (:use :cl)
  (:export :make-cluster :make-node :make-leaf :make-provider :make-component :make-link :make-manifest
   :manifest-add-provider :manifest-add-component :manifest-add-link :save-manifest-json :load-manifest-json
           :save-manifest-to-db :load-manifest-from-db :manifest :manifest-components
           :manifest-name :manifest-version :manifest-description :component-config :manifest->json-string))

(in-package :wasmcloud)


;; ============================
;; Generic tools
;; ============================

(defun ensure-string (x)
  (etypecase x
    (string x)
    (symbol (string-downcase (symbol-name x)))
    (t (princ-to-string x))))

(defun plist-merge (&rest plists)
  (let ((out (copy-list (or (first plists) '()))))
    (dolist (pl (rest plists))
      (loop for (k v) on pl by #'cddr do
        (setf (getf out k) v)))
    out))

(defun make-json-object (&rest pairs)
  "Create a hash-table adapted to cl-json from a plist"
  (let ((ht (make-hash-table :test 'equal)))
    (loop for (k v) on pairs by #'cddr do
      (setf (gethash (string-downcase (symbol-name k)) ht)
            (cond
              ((null v) '())
              ((hash-table-p v) v)
              ((and (listp v) (every #'consp v))
               (mapcar #'(lambda (x)
                           (apply #'make-json-object (apply #'append x)))
                       v))
              ((and (listp v) (keywordp (first v)))
               (apply #'make-json-object v))
              ((and (listp v) (null (first v))) '())
              (t v))))
    ht))

;; ============================
;; Workers and leafs 
;; ============================

(defclass node ()
  ((id :initarg :id :accessor node-id :initform (gensym "node-") :type t)
   (zone :initarg :zone :accessor node-zone :initform "zone-a" :type string)
   (labels :initarg :labels :accessor node-labels :initform nil :type list))
  (:documentation "Wasmcloud node execution (host)"))

(defclass leaf (node) ()
  (:documentation "Leaf node connected on NATS/wasmcloud cluster."))

(defclass cluster ()
  ((name :initarg :name :accessor cluster-name :initform "lattice-1")
   (nats-url :initarg :nats-url :accessor cluster-nats-url :initform "nats://localhost:4222")
   (nodes :initarg :nodes :accessor cluster-nodes :initform (list) :type list)
   (leaves :initarg :leaves :accessor cluster-leaves :initform (list) :type list))
  (:documentation "Logical topology about a wasmcloud cluster with nodes and leafs."))

(defun make-node (&key id zone (labels nil))
  (make-instance 'node :id (or id (string (gensym "node-"))) :zone (or zone "zone-a") :labels labels))

(defun make-leaf (&key id zone (labels nil))
  (make-instance 'leaf :id (or id (string (gensym "leaf-"))) :zone (or zone "zone-b") :labels labels))

(defun make-cluster (&key (name "lattice-1") (nats-url "nats://localhost:4222") nodes leaves)
  (make-instance 'cluster :name name :nats-url nats-url :nodes (or nodes (list)) :leaves (or leaves (list))))

(defmethod to-plist ((n node))
  (list :id (ensure-string (node-id n))
        :zone (node-zone n)
        :labels (or (node-labels n) '())))

(defmethod to-plist ((l leaf))
  (plist-merge (call-next-method) (list :kind "leaf")))

(defmethod to-plist ((c cluster))
  (list :name (cluster-name c)
        :nats-url (cluster-nats-url c)
        :nodes (mapcar #'to-plist (cluster-nodes c))
        :leaves (mapcar #'to-plist (cluster-leaves c))))

(defmethod to-json ((n node))
  (make-json-object :id (node-id n) :zone (node-zone n) :labels (node-labels n)))

(defmethod to-json ((l leaf))
  (make-json-object :id (node-id l) :zone (node-zone l) :labels (node-labels l) :kind "leaf"))

(defmethod to-json ((c cluster))
  (make-json-object
   :name (cluster-name c)
   :nats_url (cluster-nats-url c)
   :nodes (mapcar #'to-json (or (cluster-nodes c) '()))
   :leaves (mapcar #'to-json (or (cluster-leaves c) '()))
   ))

;; ============================
;; Component, Provider and link 
;; ============================

(defclass component ()
  ((name :initarg :name :accessor component-name :type string)
   (image :initarg :image :accessor component-image :type string)
   (replicas :initarg :replicas :accessor component-replicas :initform 1 :type integer)
   (zone :initarg :zone :accessor component-zone :initform "zone-a" :type string)
   (config :initarg :config :accessor component-config :initform nil :type list)
   (traits :initarg :traits :accessor component-traits :initform nil :type list))
  (:documentation "A deploy wasm component (OCI artifact)."))

(defclass provider ()
  ((name :initarg :name :accessor provider-name :type string)
   (image :initarg :image :accessor provider-image :initform nil :type (or null string))
   (capability :initarg :capability :accessor provider-capability :type string)
   (zone :initarg :zone :accessor provider-zone :initform "zone-a" :type string)
   (config :initarg :config :accessor provider-config :initform nil :type list))
  (:documentation "Capability provider (HTTP, KV, SQL, etc.)."))

(defclass link ()
  ((name :initarg :name :accessor link-name :type string)
   (source :initarg :source :accessor link-source :type string)
   (source-interface :initarg :source-interface :accessor link-source-interface :type string)
   (target :initarg :target :accessor link-target :type string)
   (target-interface :initarg :target-interface :accessor link-target-interface :type string)
   (config :initarg :config :accessor link-config :initform nil :type list))
  (:documentation "Unidirectional link (WIT) between source and target"))

(defun make-component (&key name image (replicas 1) (zone "zone-a") config traits)
  (make-instance 'component :name name :image image :replicas replicas :zone zone :config config :traits traits))

(defun make-provider (&key name image capability (zone "zone-a") config)
  (make-instance 'provider :name name :image image :capability capability :zone zone :config config))

(defun make-link (&key name source source-interface target target-interface config)
  (make-instance 'link :name name :source source :source-interface source-interface :target target :target-interface target-interface :config config))

(defun make-spreadscaler-trait (&key instances spread)
  "Crée un trait 'spreadscaler' avec vérification des poids totaux (doivent faire 100)."
  (let* ((total-weight (reduce #'+ (mapcar (lambda (s) (getf s :weight)) spread)))
         (valid (= total-weight 100)))
    (unless valid
      (error "Le total des poids doit être 100, mais vaut ~A" total-weight))
    ;; Construire le trait sous forme hash-table
    (make-json-object
     :type "spreadscaler"
     :properties
     (make-json-object
      :instances instances
      :spread (mapcar
               (lambda (s)
                 (make-json-object
                  :name (getf s :name)
                  :weight (getf s :weight)
                  :requirements (make-json-object
                                 :zone (getf (getf s :requirements) :zone))))
               spread)))))

(defmethod to-plist ((c component))
  (let ((traits (append (when (component-replicas c)
                          (list (list :type "scaler" :properties (list :replicas (component-replicas c)))))
                        (when (component-zone c)
                          (list (list :type "placement" :properties (list :zone (component-zone c)))))
                        (component-traits c))))
    (list :name (component-name c)
          :type "component"
          :properties (plist-merge (list :image (component-image c)) (or (component-config c) '()))
          :traits traits)))

(defmethod to-plist ((p provider))
  (list :name (provider-name p)
        :type "capability"
        :properties (plist-merge (list :capability (provider-capability p))
                                 (when (provider-image p) (list :image (provider-image p)))
                                 (or (provider-config p) '()))
        :traits (list (list :type "placement" :properties (list :zone (provider-zone p))))))


(defmethod to-plist ((l link))
  (list :name (link-name l)
        :type "link"
        :properties (plist-merge (list :source (list :name (link-source l)
                                                     :interface (link-source-interface l))
                                       :target (list :name (link-target l)
                                                     :interface (link-target-interface l)))
                                 (or (link-config l) '()))))

;; (defmethod to-json ((c component))
;;   (make-json-object
;;    :name (component-name c)
;;    :type "component"
;;    :properties (make-json-object :image (component-image c))
;;    :traits (list (make-json-object :type "spreadscaler" :properties (make-json-object :replicas (component-replicas c)))
;;                  (make-json-object :type "placement" :properties (make-json-object :zone (component-zone c))))))

(defmethod to-json ((c component))
  (let ((traits (list
                 ;; Trait scaler de base
                 (make-json-object
                  :type "scaler"
                  :properties (make-json-object :replicas (component-replicas c)))
                 ;; Trait placement de base
                 (make-json-object
                  :type "placement"
                  :properties (make-json-object :zone (component-zone c))))))
    ;; Add personal behaviors eventually
    (dolist (ct (component-traits c))
      (push (apply #'make-json-object ct) traits))
    ;; If spreadcaler is defined
    (when (getf (component-config c) :spread)
      (push (make-spreadscaler-trait
             :instances (getf (component-config c) :instances 100)
             :spread (getf (component-config c) :spread))
            traits))
    (make-json-object
     :name (component-name c)
     :type "component"
     :properties (make-json-object :image (component-image c))
     :traits (reverse traits))))

(defmethod to-json ((p provider))
  (make-json-object
   :name (provider-name p)
   :type "capability"
   :properties (make-json-object :capability (provider-capability p) :image (provider-image p))
   :traits (list (make-json-object :type "placement" :properties (make-json-object :zone (provider-zone p))))))

(defmethod to-json ((l link))
  (make-json-object
   :name (link-name l)
   :type "link"
   :properties (make-json-object
                :source (make-json-object :name (link-source l) :interface (link-source-interface l))
                :target (make-json-object :name (link-target l) :interface (link-target-interface l)))))

;; ============================
;; Manifest applicatif
;; ============================

(defclass manifest ()
  ((name :initarg :name :accessor manifest-name :type string)
   (version :initarg :version :accessor manifest-version :initform "0.1.0")
   (description :initarg :description :accessor manifest-description :initform "")
   (cluster :initarg :cluster :accessor manifest-cluster :initform (make-cluster))
   (components :initarg :components :accessor manifest-components :initform (list) :type list)
   (providers :initarg :providers :accessor manifest-providers :initform (list) :type list)
   (links :initarg :links :accessor manifest-links :initform (list) :type list))
  (:documentation "JSON manifest for wasmcloud"))

(defun make-manifest (&key name (version "0.1.0") (description "") cluster components providers links)
  (make-instance 'manifest :name name :version version :description description :cluster (or cluster (make-cluster))
                           :components (or components (list)) :providers (or providers (list)) :links (or links (list))))

(defun manifest-add-component (m c)
  (push c (manifest-components m)) m)

(defun manifest-add-provider (m p)
  (push p (manifest-providers m)) m)

(defun manifest-add-link (m l)
  (push l (manifest-links m)) m)

(defun update-component-replicas (m component-name new-replicas)
  (dolist (c (manifest-components m) m)
    (when (string= (component-name c) component-name)
      (setf (component-replicas c) new-replicas))))

(defun update-component-zone (m component-name new-zone)
  (dolist (c (manifest-components m) m)
    (when (string= (component-name c) component-name)
      (setf (component-zone c) new-zone))))

(defmethod to-plist ((m manifest))
  (list
   :api_version "oam.wasmcloud.dev/v1"
   :kind "Application"
   :metadata (list :name (manifest-name m)
                   :version (manifest-version m)
                   :description (manifest-description m))
   :cluster (to-plist (manifest-cluster m))
   :spec (list
          :components (mapcar #'to-plist (reverse (manifest-components m)))
          :providers (mapcar #'to-plist (reverse (manifest-providers m)))
          :links (mapcar #'to-plist (reverse (manifest-links m))))))

(defmethod to-json ((m manifest))
  (make-json-object
   :api_version "oam.wasmcloud.dev/v1"
   :kind "Application"
   :metadata (make-json-object :name (manifest-name m) :version (manifest-version m) :description (manifest-description m))
   :cluster (to-json (manifest-cluster m))
   :spec (make-json-object
          :components (mapcar #'to-json (reverse (manifest-components m)))
          :providers (mapcar #'to-json (reverse (manifest-providers m)))
          :links (mapcar #'to-json (reverse (manifest-links m))
                         ))))

;; ============================
;; Manifest applicatif
;; ============================

(defun manifest->json-string (m)
  (let ((obj (to-json m)))
    ;; (format t "~a" obj)
    (cl-json:encode-json-to-string obj)))

(defun save-manifest-json (m pathname)
  (with-open-file (out pathname :direction :output :if-exists :supersede :if-does-not-exist :create)
    (write-string (manifest->json-string m) out))
  pathname)

;; (defun load-manifest-json (pathname)
;;   " Loads a manifest from a JSON file and recomposes instances
;;     Note: Unrecognized extensions are stored in the :config/traits slots."
;;   (let* ((data (with-open-file (in pathname :direction :input)
;;                  (let ((content (make-string (file-length in))))
;;                    (read-sequence content in)
;;                    (cl-json:decode-json-from-string content))))
;;          (meta (getf data :metadata))
;;          (cluster (getf data :cluster))
;;          (spec (getf data :spec)))
;;     (labels ((->component (x)
;;                (make-component :name (getf x :name)
;;                                :image (getf (getf x :properties) :image)
;;                                :replicas (getf (getf (find-if (lambda (tp) (string= (getf tp :type) "scaler"))
;;                                                               (getf x :traits))
;;                                                      :properties) :replicas 1)
;;                                :zone (getf (getf (find-if (lambda (tp) (string= (getf tp :type) "placement"))
;;                                                           (getf x :traits))
;;                                                  :properties) :zone "zone-a")
;;                                :config (getf x :properties)
;;                                :traits (remove-if (lambda (tp) (member (getf tp :type) '("scaler" "placement") :test #'string=))
;;                                                   (getf x :traits))))
;;              (->provider (x)
;;                (make-provider :name (getf x :name)
;;                               :capability (getf (getf x :properties) :capability)
;;                               :image (getf (getf x :properties) :image)
;;                               :zone (getf (getf (find-if (lambda (tp) (string= (getf tp :type) "placement"))
;;                                                          (getf x :traits))
;;                                                 :properties) :zone "zone-a")
;;                               :config (getf x :properties)))
;;              (->link (x)
;;                (make-link :name (getf x :name)
;;                           :source (getf (getf (getf x :properties) :source) :name)
;;                           :source-interface (getf (getf (getf x :properties) :source) :interface)
;;                           :target (getf (getf (getf x :properties) :target) :name)
;;                           :target-interface (getf (getf (getf x :properties) :target) :interface)
;;                           :config (getf x :properties))))
;;       (make-manifest
;;        :name (getf meta :name)
;;        :version (getf meta :version)
;;        :description (getf meta :description)
;;        :cluster (make-cluster :name (getf cluster :name)
;;                               :nats-url (getf cluster :nats_url)
;;                               :nodes (mapcar (lambda (n) (make-node :id (getf n :id) :zone (getf n :zone) :labels (getf n :labels)))
;;                                              (or (getf cluster :nodes) '()))
;;                               :leaves (mapcar (lambda (n) (make-leaf :id (getf n :id) :zone (getf n :zone) :labels (getf n :labels)))
;;                                               (or (getf cluster :leaves) '())))
;;        :components (mapcar #'->component (or (getf spec :components) '()))
;;        :providers (mapcar #'->provider (or (getf spec :providers) '()))
;;        :links (mapcar #'->link (or (getf spec :links) '()))))))

(defun load-manifest-json (pathname)
  "Charge un manifest JSON depuis le disque et reconstruit des objets Lisp.
   Gère les traits 'spreadscaler', 'scaler' et 'placement'."
  (let* ((data (with-open-file (in pathname :direction :input)
                 (let ((content (make-string (file-length in))))
                   (read-sequence content in)
                   (cl-json:decode-json-from-string content))))
         (meta (gethash "metadata" data))
         (cluster-data (gethash "cluster" data))
         (spec (gethash "spec" data)))

    (labels ((->component (x)
               (let* ((props (gethash "properties" x))
                      (traits (gethash "traits" x))
                      (replicas 1)
                      (zone "zone-a")
                      (config nil))
                 ;; extraire scalabilité, placement et spreadscaler
                 (dolist (tt traits)
                   (let ((ttype (gethash "type" tt))
                         (tprops (gethash "properties" tt)))
                     (cond
                       ((string= ttype "scaler")
                        (setf replicas (gethash "replicas" (gethash "properties" tt))))
                       ((string= ttype "placement")
                        (setf zone (gethash "zone" (gethash "properties" tt))))
                       ((string= ttype "spreadscaler")
                        (setf config
                              (list
                               :instances (gethash "instances" tprops)
                               :spread
                               (mapcar (lambda (s)
                                         (list
                                          :name (gethash "name" s)
                                          :weight (gethash "weight" s)
                                          :requirements (list :zone (gethash "zone" (gethash "requirements" s)))))
                                       (gethash "spread" tprops))))))))
                 (make-component
                  :name (gethash "name" x)
                  :image (gethash "image" props)
                  :replicas replicas
                  :zone zone
                  :config config))))
      (->provider (x)
                  (make-provider
                   :name (gethash "name" x)
                   :capability (gethash "capability" (gethash "properties" x))
                   :image (gethash "image" (gethash "properties" x))
                   :zone (gethash "zone" (gethash "properties" (aref (gethash "traits" x) 0)))))
      (->link (x)
              (let ((props (gethash "properties" x)))
                (make-link
                 :name (gethash "name" x)
                 :source (gethash "name" (gethash "source" props))
                 :source-interface (gethash "interface" (gethash "source" props))
                 :target (gethash "name" (gethash "target" props))
                 :target-interface (gethash "interface" (gethash "target" props))))))

    ;; Reconstruction du cluster
    (let ((cluster (make-cluster
                    :name (gethash "name" cluster-data)
                    :nats-url (gethash "nats_url" cluster-data)
                    :nodes (mapcar (lambda (n)
                                     (make-node :id (gethash "id" n)
                                                :zone (gethash "zone" n)
                                                :labels (gethash "labels" n)))
                                   (gethash "nodes" cluster-data))
                    :leaves (mapcar (lambda (l)
                                      (make-leaf :id (gethash "id" l)
                                                 :zone (gethash "zone" l)
                                                 :labels (gethash "labels" l)))
                                    (gethash "leaves" cluster-data)))))

      ;; Reconstruction du manifest complet
      (make-manifest
       :name (gethash "name" meta)
       :version (gethash "version" meta)
       :description (gethash "description" meta)
       :cluster cluster
       :components (mapcar #'->component (gethash "components" spec))
       :providers (mapcar #'->provider (gethash "providers" spec))
       :links (mapcar #'->link (gethash "links" spec))))))

;;;; =====================
;;;; Redeploy helper
;;;; =====================

;; (defun redeploy-with (m &key replicas zone)
;;   "Create a new manifest instance with accuraty"
;;   (let ((copy (make-manifest :name (manifest-name m)
;;                              :version (manifest-version m)
;;                              :description (manifest-description m)
;;                              :cluster (manifest-cluster m)
;;                              :components (mapcar #'(lambda (c)
;;                                                      (make-component :name (component-name c)
;;                                                                      :image (component-image c)
;;                                                                      :replicas (or replicas (component-replicas c))
;;                                                                      :zone (or zone (component-zone c))
;;                                                                      :config (component-config c)
;;                                                                      :traits (component-traits c)))
;;                                                  (manifest-components m))
;;                              :providers (copy-list (manifest-providers m))
;;                              :links (copy-list (manifest-links m)))))
;;     copy))

(defun save-manifest-to-db (m &optional (path db:*manifest-db*))
  (sqlite:with-open-database (db path)
    (apply #'sqlite:execute-non-query
           db
           "INSERT INTO manifest (name, version, description) VALUES (?, ?, ?);"
           (list (manifest-name m) (manifest-version m) (manifest-description m)))
    (let* ((manifest-id (sqlite:last-insert-rowid db))
           (cluster (manifest-cluster m)))
      ;; cluster
      (apply #'sqlite:execute-non-query
             db
             "INSERT INTO cluster (name, nats_url, manifest_id) VALUES (?, ?, ?);"
             (list (cluster-name cluster) (cluster-nats-url cluster) manifest-id))
      ;; component
      (dolist (c (manifest-components m))
        (apply #'sqlite:execute-non-query
               db
               "INSERT INTO component (name, image, replicas, zone, manifest_id) VALUES (?, ?, ?, ?, ?);"
               (list (component-name c) (component-image c)
                     (component-replicas c) (component-zone c) manifest-id)
               )
        (let ((component-id (sqlite:last-insert-rowid db))
              (conf (component-config c)))
          ;; spread present
          (when (getf conf :spread)
            (dolist (s (getf conf :spread))
              (apply #'sqlite:execute-non-query
                     db
                     "INSERT INTO spread (component_id, name, weight, zone) VALUES (?, ?, ?, ?);"
                     (list component-id (getf s :name)
                           (getf s :weight)
                           (getf (getf s :requirements) :zone)))))))
      ;; provider
      (dolist (p (manifest-providers m))
        (apply #'sqlite:execute-non-query
               db
               "INSERT INTO provider (name, capability, image, zone, manifest_id) VALUES (?, ?, ?, ?, ?);"
               (list (provider-name p) (provider-capability p)
                     (provider-image p) (provider-zone p) manifest-id))) 
      ;; link
      (dolist (l (manifest-links m))
        (apply #'sqlite:execute-non-query
               db
               "INSERT INTO link (name, source, source_interface, target, target_interface, manifest_id) VALUES (?, ?, ?, ?, ?, ?);"
               (list (link-name l) (link-source l)
                     (link-source-interface l)
                     (link-target l)
                     (link-target-interface l)
                     manifest-id))))
    (format t "Manifest '~A' saved.~%" (manifest-name m))))

(defun load-manifest-from-db (manifest-name &optional (path db:*manifest-db*))
  "Recrée un objet manifest depuis la base SQLite à partir de son nom."
  (sqlite:with-open-database (db path)
    (let* ((row (car (apply #'sqlite:execute-to-list
                            db
                            "SELECT id, name, version, description FROM manifest WHERE name = ?;"
                            (list manifest-name))))
           (manifest-id (first row))
           (cluster-row (car (apply #'sqlite:execute-to-list
                                    db
                                    "SELECT name, nats_url FROM cluster WHERE manifest_id = ?;"
                                    (list manifest-id))))
           (comp-rows (apply #'sqlite:execute-to-list
                             db
                             "SELECT id, name, image, replicas, zone FROM component WHERE manifest_id = ?;"
                             (list manifest-id)))
           (prov-rows (apply #'sqlite:execute-to-list
                             db
                             "SELECT name, capability, image, zone FROM provider WHERE manifest_id = ?;"
                             (list manifest-id)))
           (link-rows (apply #'sqlite:execute-to-list
                             db
                             "SELECT name, source, source_interface, target, target_interface FROM link WHERE manifest_id = ?;"
                             (list manifest-id)))
           (components
             (mapcar (lambda (r)
                       (let* ((cid (first r))
                              (spread (apply #'sqlite:execute-to-list
                                             db
                                             "SELECT name, weight, zone FROM spread WHERE component_id = ?;"
                                             (list cid)))
                              (spread-conf (mapcar (lambda (s)
                                                     (list :name (first s)
                                                           :weight (second s)
                                                           :requirements (list :zone (third s))))
                                                   spread)))
                         (make-component
                          :name (second r)
                          :image (third r)
                          :replicas (fourth r)
                          :zone (fifth r)
                          :config (when spread-conf
                                    (list :instances (reduce #'+ (mapcar (lambda (s) (getf s :weight)) spread-conf))
                                          :spread spread-conf)))))
                     comp-rows))
           (providers
             (mapcar (lambda (r)
                       (make-provider :name (first r)
                                      :capability (second r)
                                      :image (third r)
                                      :zone (fourth r)))
                     prov-rows))
           (links
             (mapcar (lambda (r)
                       (make-link :name (first r)
                                  :source (second r)
                                  :source-interface (third r)
                                  :target (fourth r)
                                  :target-interface (fifth r)))
                     link-rows)))
      (make-manifest
       :name (second row)
       :version (third row)
       :description (fourth row)
       :cluster (make-cluster :name (first cluster-row)
                              :nats-url (second cluster-row))
       :components components
       :providers providers
       :links links))))
