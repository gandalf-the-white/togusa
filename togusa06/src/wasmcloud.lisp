(in-package :togusa)


;; ================================================
;;  W O R K E R S  /  L E A F S  /  C L U S T E R S
;; ================================================

(defclass node ()
  ((id :initarg :id
       :accessor node-id
       :initform nil
       :type t)
   (zone :initarg :zone
         :accessor node-zone
         :type string)
   (label :initarg :label
          :accessor node-label
          :type list))
  (:documentation "Représente un nœud de cluster (worker ou leaf)."))

(defclass worker (node) ()
  (:documentation "Worker node."))

(defclass leaf (node) ()
  (:documentation "Leaf node."))

(defclass cluster ()
  ((name :initarg :name
         :accessor cluster-name
         :initform "wasmcloud"
         :type string)
   (nats-url :initarg :nats-url
             :accessor cluster-nats-url
             :initform "nats://localhost:4222"
             :type string)
   (workers :initarg :workers
            :accessor cluster-workers
            :initform '()
            :type list)
   (leafs :initarg :leafs
          :accessor cluster-leafs
          :initform '()
          :type list))              
  (:documentation "Représente un cluster wasmCloud + NATS."))

(defun make-worker (&key id zone label)
  "Construire un worker."
  (make-instance 'worker
                 :id id
                 :zone zone
                 :label (or label '())))

(defun make-leaf (&key id zone label)
  "Construire un leaf."
  (make-instance 'leaf
                 :id id
                 :zone zone
                 :label (or label '())))

(defun make-cluster (&key name nats-url workers leafs)
  "Construire un cluster. Les valeurs par défaut sont définies dans les INITFORM."
  (make-instance 'cluster
                 :name name
                 :nats-url nats-url
                 :workers (or workers '())
                 :leafs (or leafs '())))

(defgeneric to-json (object)
  (:documentation "Encoder OBJECT en représentation JSON (hash-table)."))

(defmethod to-json ((w worker))
  (make-json-object :id (node-id w)
                    :zone (node-zone w)
                    :labels (node-label w)))

(defmethod to-json ((l leaf))
  (make-json-object :id (node-id l)
                    :zone (node-zone l)
                    :labels (node-label l)))

(defmethod to-json ((c cluster))
  (make-json-object
   :name (cluster-name c)
   :nats-url (cluster-nats-url c)
   :workers (mapcar #'to-json (cluster-workers c)) ;; MODIF: MAPCAR au lieu de MAPCAN
   :leafs (mapcar #'to-json (cluster-leafs c))))    ;; MODIF: idem

;; ================================================
;;  S P R E A D   /   L I N K   /   C O N F I G
;; ================================================

(defclass spread ()
  ((name :initarg :name
         :accessor spread-name
         :type string)
   (weight :initarg :weight
           :accessor spread-weight
           :type integer)
   (zone :initarg :zone
         :accessor spread-zone
         :type string))
  (:documentation "Décrit un poids de scheduling pour une zone donnée."))

(defclass spreadscaler ()
  ((instances :initarg :instances
              :accessor spreadscaler-instances
              :type integer)
   ;; MODIF: le slot SPREAD contient désormais une LISTE de SPREADs
   ;;        (et pas un seul), avec INITFORM = '().
   (spread :initarg :spread
           :accessor spreadscaler-spread
           :initform '()
           :type list))
  (:documentation "Trait spreadscaler pour répartir les replicas par zone."))

(defclass config ()
  ((name :initarg :name
         :accessor config-name
         :type string)
   (address :initarg :address
            :accessor config-address
            :type string))
  (:documentation "Configuration d'un provider (ex: HTTP address)."))

(defclass link ()
  ((target :initarg :target
           :accessor link-target
           :type string)
   (pack :initarg :pack
         :accessor link-pack
         :type string)
   (namespace :initarg :namespace
              :accessor link-namespace
              :type string)
   (interfaces :initarg :interfaces
               :accessor link-interfaces
               :type list)
   ;; MODIF: source optionnel, avec INITFORM NIL et type (OR NULL CONFIG)
   (source :initarg :source
           :accessor link-source
           :initform nil
           :type (or null config)))
  (:documentation "Trait link entre un composant et une capability."))

(defun make-spread (&key name weight zone)
  "Construire un objet SPREAD."
  (make-instance 'spread
                 :name name
                 :weight weight
                 :zone zone))

(defun make-spreadscaler (&key instances spread)
  "Construire un trait SPREADSCALER.
SPREAD est une liste de SPREADs."
  (make-instance 'spreadscaler
                 :instances instances
                 :spread (or spread '())))

(defun make-config (&key name address)
  "Construire une CONFIG."
  (make-instance 'config
                 :name name
                 :address address))

(defun make-link (&key target pack namespace interfaces source)
  "Construire un trait LINK."
  (make-instance 'link
                 :target target
                 :pack pack
                 :namespace namespace
                 :interfaces interfaces
                 :source source))

;; MODIF: respect du schéma wasmCloud OAM pour SPREAD / SPREADSCALER.
(defmethod to-json ((s spreadscaler))
  (make-json-object
   :type "spreadscaler"
   :properties (make-json-object
                :instances (spreadscaler-instances s)
                :spread (mapcar #'to-json (spreadscaler-spread s)))))

(defmethod to-json ((s spread))
  (make-json-object
   :name (spread-name s)
   :weight (spread-weight s)
   ;; MODIF: clé `requirements` (et pas `requirement`),
   ;;        et objet JSON pour les labels de scheduling.
   :requirements (make-json-object :zone (spread-zone s))))

;; MODIF: CONFIG encode maintenant un objet `{ name, properties{...} }`
;;        directement (sans wrapper :config).
(defmethod to-json ((c config))
  (make-json-object
   :name (config-name c)
   :properties (make-json-object
                :address (config-address c))))

;; MODIF: LINK utilise `source_config` (liste) conformément à la doc wasmCloud.
(defmethod to-json ((l link))
  (let* ((base-props (list :target (link-target l)
                           :namespace (link-namespace l)
                           :package (link-pack l)
                           :interfaces (link-interfaces l)))
         (props (if (link-source l)
                    (append base-props
                            (list :source_config
                                  (list (to-json (link-source l)))))
                    base-props)))
    (make-json-object
     :type "link"
     :properties (apply #'make-json-object props))))

;; ================================================
;;  I N S T A N C E S  /  C O M P O N E N T S
;; ================================================

(defclass instance ()
  ((name :initarg :name
         :accessor instance-name
         :type string)
   (image :initarg :image
          :accessor instance-image
          :type string)
   (traits :initarg :traits
           :accessor instance-traits
           :initform '()
           :type list))
  (:documentation "Instance wasmCloud (composant ou capability)."))

(defclass component (instance)
  ((replicas :initarg :replicas
             :accessor component-replicas
             :initform 1
             :type integer))
  (:documentation "Component OAM."))

(defclass capability (instance)
  ()
  (:documentation "Capability provider (HTTP server, NATS, etc.)."))

(defun make-component (&key name image replicas traits)
  "Construire un composant."
  (make-instance 'component
                 :name name
                 :image image
                 :replicas (or replicas 1)
                 :traits (or traits '())))

(defun make-capability (&key name image traits)
  "Construire une capability."
  (make-instance 'capability
                 :name name
                 :image image
                 :traits (or traits '())))

(defmethod to-json ((c component))
  (make-json-object
   :name (instance-name c)
   :type "component"
   :image (instance-image c)
   :replicas (component-replicas c)
   :traits (mapcar #'to-json (instance-traits c))))

(defmethod to-json ((p capability))
  (make-json-object
   :name (instance-name p)
   :type "capability"
   :image (instance-image p)
   :traits (mapcar #'to-json (instance-traits p))))

;; ================================================
;;  M A N I F E S T
;; ================================================

(defclass manifest ()
  ((name :initarg :name
         :accessor manifest-name
         :type string)
   (cluster :initarg :cluster
            :accessor manifest-cluster
            :type cluster)
   (components :initarg :components
               :accessor manifest-components
               :initform '()
               :type list))
  (:documentation "Objet manifest pour wadm / wasmCloud."))

(defun make-manifest (&key name cluster components)
  "Construire un manifest."
  (make-instance 'manifest
                 :name name
                 :cluster cluster
                 :components (or components '())))

;; MODIF: to-json aligné avec le schéma OAM/wasmCloud :
;;   - apiVersion (camelCase)
;;   - spec.{components, cluster}
(defmethod to-json ((m manifest))
  (make-json-object
   :apiVersion "core.oam.dev/v1beta1"
   :kind "Application"
   :metadata (make-json-object :name (manifest-name m))
   :spec (make-json-object
          :cluster (to-json (manifest-cluster m))
          :components (mapcar #'to-json (manifest-components m)))))

;; ================================================
;;  C H E C K S
;; ================================================

;; MODIF: test-spread -> valid-spread-p, usage de REDUCE et du slot correct.
(defun valid-spread-p (spreads)
  "Vérifie que la somme des WEIGHT d'une liste de SPREAD vaut 100."
  (let ((total-weight (reduce #'+ spreads
                              :key #'spread-weight
                              :initial-value 0)))
    (= 100 total-weight)))

(defmethod json-to-string-manifest ((m manifest))
  "Sérialiser un manifest en chaîne JSON via cl-json."
  (cl-json:encode-json-to-string (to-json m)))

;; (defun manifest-to-json-string (manifest)
;; "Convertit un manifest complet en JSON sous forme de string."
;; (json-to-string-manifest (to-json manifest)))

(defmethod save-manifest ((m manifest) pathname)
  "Sauvegarder le manifest en JSON dans PATHNAME."
  (with-open-file (out pathname
                       :direction :output
                       :if-exists :supersede
                       :if-does-not-exist :create)
    (write-sequence (json-to-string-manifest m) out)))
