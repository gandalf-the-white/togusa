(defpackage :wasmcloud
  (:use :cl)
  (:export :make-provider :make-worker :make-leaf
   :make-component :make-link :make-spread
           :make-cluster :make-manifest
           :make-json-object :to-json
           :json-to-string-manifest :save-manifest))

(in-package :wasmcloud)

;; ================================================
;; T O O L S 
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
          :initform nil
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
            :type list)
   (leafs :initarg :leafs
          :accessor cluster-leafs
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
  (make-json-object :id (node-id w) :zone (node-zone w) :label (node-label w)))

(defmethod to-json ((l leaf))
  (make-json-object :id (node-id l) :zone (node-zone l) :label (node-label l)))

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
          :type string))
  (:documentation "Base to the component or provider"))

(defclass component (instance)
  ((replicas :initarg :replicas
             :accessor replicas
             :initform 1
             :type integer)
   (traits :initarg :traits
           :accessor traits
           :type list))
  (:documentation "Component definition"))

(defclass provider (instance)
  ())

(defclass spread ()
  ((name :initarg :name
         :accessor name
         :type string)
   (weight :initarg :weight
           :accessor weight
           :type integer)
   (requirement :initarg :requirement
                :accessor requirement))
  (:documentation "Define the balance with zone areas"))

(defclass link ()
  ((name :initarg :name
         :accessor link-name)
   (source :initarg :source
           :accessor link-source
           :type string)
   (target :initarg :target
           :accessor link-target
           :type string)))

(defmethod make-component (&key name image replicas traits)
  (make-instance 'component :name name
                            :image image
                            :replicas replicas
                            :traits (or traits (list))))

(defmethod make-provider (&key name image)
  (make-instance 'provider :name name
                           :image image))

(defmethod make-spread (&key name weight requirement)
  (make-instance 'spread :name name :weight weight :requirement requirement))

(defmethod make-link (&key name source target)
  (make-instance 'link :name name
                       :source source
                       :target target))

(defmethod to-json ((c component))
  (make-json-object :name (name c)
                    :type "component"
                    :image (image c)
                    :replicas (replicas c)
                    :traits (mapcar #'to-json (traits c))))

(defmethod to-json ((p provider))
  (make-json-object :name (name p)
                    :image (image p)))

(defmethod to-json ((s spread))
  (make-json-object :name (name s)
                    :weight (weight s)
                    :requirement (make-json-object :zone (requirement s))))

(defmethod to-json ((l link))
  (make-json-object :name (name l)
                    :source (source l)
                    :target (target l)))

;; ================================================
;;  M A N I F E S T
;; ================================================

(defclass manifest ()
  ((name :initarg :name
         :accessor manifest-name
         :type string)
   (cluster :initarg :cluster
            :accessor cluster)
   (components :initarg :components
               :accessor components
               :type list)
   (providers :initarg :providers
              :accessor providers
              :type list)
   (links :initarg :links
          :accessor links
          :type list))
  (:documentation "Manifest"))

(defmethod make-manifest (&key name cluster components providers links)
  (make-instance 'manifest :name name
                           :cluster cluster
                           :components components
                           :providers providers
                           :links links))

(defmethod to-json ((m manifest))
  (make-json-object :api_version "oam.wasmcloud.dev/v1"
                    :kind "Application"
                    :metadata (make-json-object :name (manifest-name m))
                    :cluster (to-json (cluster m))
                    :components (mapcar #'to-json (components m))
                    :providers (mapcar #'to-json (providers m))))

;; ================================================
;;  T O O L S
;; ================================================

(defmethod json-to-string-manifest (m)
  (let ((data (to-json m)))
    (cl-json:encode-json-to-string data)))

(defmethod save-manifest (m pathname)
  (with-open-file (out pathname :direction :output
                                :if-exists :supersede
                                :if-does-not-exist :create)
    (write-sequence (json-to-string-manifest m) out)))
