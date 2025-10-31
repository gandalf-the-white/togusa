(defpackage :wasmcloud
  (:use :cl))

(in-package :wasmcloud)

;; ================================================
;; C O M M O N  
;; ================================================

(defun make-json-object (&rest pairs)
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

;; ================================================
;;  W O R K E R S  L E A F E S  C L U S T E R S
;; ================================================

(defclass node ()
  ((id :initarg :id
       :accessor node-id
       :initform nil
       :type t)
   (zone :initarg :zone
         :accessor node-zone
         :initform "zone-a"
         :type string)
   (label :initarg :label
          :accessor node-label
          :initform (list)
          :type list))
  (:documentation "Common node base of worker and leaf"))

(defclass worker (node)
  ())

(defclass leaf (node)
  ())

(defclass cluster ()
  ((name :initarg :name
         :accessor cluster-name
         :initform "wasmcloud")
   (nats-url :initarg :nats-url
             :accessor cluster-nats-url
             :initform "nats://localhost:4222")
   (workers :initarg :workers
            :accessor cluster-workers
            :initform (list)
            :type list)
   (leafs :initarg :leafs
          :accessor cluster-leafs
          :initform (list)
          :type list))
  (:documentation "Cluster composed of workers"))


(defmethod make-worker (&key id zone label)
  (make-instance 'worker :id (or id (gensym "worker-"))
                         :zone (or zone "zone-a")
                         :label (or label (list))))

(defmethod make-leaf (&key id zone label)
  (make-instance 'leaf :id (or id (gensym "leaf-"))
                       :zone (or zone "zone-a")
                       :label (or label (list))))

(defmethod make-cluster (&key name nats-url workers leafs)
  (make-instance 'cluster :name (or name "wasmcloud")
                          :nats-url (or nats-url "nats://localhost:4222")
                          :workers (or  workers (list))
                          :leafs (or leafs (list))))

(defmethod to-json ((w worker))
  (make-json-object :id (node-id w)
                    :zone (node-zone w)
                    :labels (node-label w)))

(defmethod to-json ((l leaf))
  (make-json-object :id (node-id l)
                    :zone (node-zone l)
                    :labels (node-label l)))

(defmethod to-json ((c cluster))
  (make-json-object :name (cluster-name c)
                    :nats-url (cluster-nats-url c)
                    :workers (mapcar #'to-json (cluster-workers c))
                    :leafs (mapcan #'to-json (cluster-leafs c))))

;; ================================================
;;  C O M P O N E N T S  P R O V I D E R S
;; ================================================

(defclass instance ()
  ((name :initarg :name
         :accessor name
         :initform nil
         :type t)
   (image :initarg :image
          :accessor image
          :type string)
   (traits :initarg :traits
           :accessor traits
           :initform (list)
           :type list))
  (:documentation "Base to the component or capability"))

(defclass component (instance)
  ((replicas :initarg :replicas
             :accessor replicas
             :initform 1
             :type integer))
  (:documentation "Component definition"))

(defclass capability (instance)
  ())

(defmethod make-component (&key name image replicas traits)
  (make-instance 'component :name name
                            :image image
                            :replicas replicas
                            :traits (or traits (list))))

(defmethod make-capability (&key name image traits)
  (make-instance 'capability :name name
                             :image image
                             :traits (or traits (list))))

(defmethod to-json ((c component))
  (make-json-object :name (name c)
                    :type "component"
                    :image (image c)
                    :replicas (replicas c)
                    :traits (mapcar #'to-json (traits c))
                    ))

(defmethod to-json ((p capability))
  (make-json-object :name (name p)
                    :type "capability"
                    :image (image p)
                    :traits (mapcar #'to-json (traits p))))

;; ================================================
;;  S P R E A D   A N D   L I N K
;; ================================================

(defclass spreadscaler ()
  ((instances :initarg :instances
              :accessor instances
              :type integer)
   (spread :initarg :spread
           :accessor spread
           :type SPREAD))
  (:documentation "Spreadscaler set spreads balance"))

(defclass spread ()
  ((name :initarg :name
         :accessor name
         :type string)
   (weight :initarg :weight
           :accessor weight
           :type integer)
   (zone :initarg :zone
         :accessor zone
         :type string))
  (:documentation "Define the weight per instance"))

(defclass link ()
  ((target :initarg :target
           :accessor target
           :type string)
   (pack :initarg :pack
         :accessor pack
         :type string)
   (namespace :initarg :namespace
              :accessor namespace
              :type string)
   (interfaces :initarg :interfaces
               :accessor interfaces
               :type list))
  (:documentation "Define the target connection"))

(defmethod make-spreadscaler (&key instances spread)
  (make-instance 'spreadscaler :instances instances
                               :spread spread))

(defmethod make-spread (&key name weight zone)
  (make-instance 'spread :name name
                         :weight weight
                         :zone zone))

(defmethod make-link (&key target pack namespace interfaces)
  (make-instance 'link :target target
                       :pack pack
                       :namespace namespace
                       :interfaces interfaces))

(defmethod to-json ((s spreadscaler))
  (make-json-object :type "spreadscaler"
                    :instances (instances s)
                    :properties (list  :instances (instances s)
                                       :spread (mapcar #'to-json (spread s)))))

(defmethod to-json ((s spread))
  (make-json-object :name (name s)
                    :weight (weight s)
                    :requirement (list :zone (zone s))))

(defmethod to-json ((l link))
  (make-json-object :type "link"
                    :properties (list :target (target l)
                                      :namespace (namespace l)
                                      :package (pack l)
                                      :interfaces (interfaces l))))

;; ================================================
;;  M A N I F E S T
;; ================================================

(defclass manifest ()
  ((name :initarg :name
         :accessor name
         :type string)
   (cluster :initarg :cluster
            :accessor cluster
            :type CLUSTER)
   (components :initarg :components
               :accessor components
               :type list))
  (:documentation "Manifest object"))

(defmethod make-manifest (&key name cluster components)
  (make-instance 'manifest :name name
                           :cluster cluster
                           :components components))

(defmethod to-json ((m manifest))
  (make-json-object :api_version "oam.wasmcloud.dev/v1"
                    :kind "Application"
                    :metadata (make-json-object :name (name m))
                    :cluster (to-json (cluster m))
                    :components (mapcar #'to-json (components m))))

;; ================================================
;;  T O O L S
;; ================================================

(defun test-spread (spreads)
  (let* ((total-weight (apply #'+ (mapcar #'weight spreads)))
         (valid (= 100 total-weight)))
    valid))

(defmethod json-to-string-manifest ((m manifest))
  (let ((data (to-json m)))
    (cl-json:encode-json-to-string data)))

(defmethod save-manifest ((m manifest) pathname)
  (with-open-file (out pathname :direction :output
                                :if-exists :supersede
                                :if-does-not-exist :create)
    (write-sequence (json-to-string-manifest m) out)))

;; ================================================
;;  E X A M P L E
;; ================================================

(defun test ()
  (let* ((cluster (make-cluster :name "wasmcloud"
                                :workers (list (make-worker :label (list "cpu") :zone "zone-west"))
                                :leafs (list (make-leaf :label (list "home") :zone "zone-west"))))
         (spreads (list (make-spread :name "eastcost" :weight 80 :zone "us-eastcost")
                        (make-spread :name "westcost" :weight 20 :zone "us-westcost")))
         (spreadscaler (make-spreadscaler :instances 100 :spread spreads))
         (link (make-link :target "Http-component" :pack "http" :namespace "wasi" :interfaces (list "incoming-handler")))
         (manifest (make-manifest :cluster cluster :name "wasmcloud"
                                  :components (list (make-component :name "Http-component"
                                                                    :replicas 4
                                                                    :image "ghcr.io/wasmcloud/components/dog-fetcher-rust:0.1.1"
                                                                    :traits (list spreadscaler))
                                                    (make-capability :name "Httpserver"
                                                                     :image "ghcr.io/wasmcloud/http-server:0.27.0"
                                                                     :traits (list link))))))
    (format t "manifest: ~a" manifest)
    (save-manifest manifest "../datas/save.json") 
    manifest))
