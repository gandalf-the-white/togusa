(in-package :togusa)

;; ================================================
;;  C O N N E X I O N   &   U T I L I T A I R E S
;; ================================================

;; MODIF: DEFVAR au lieu de DEFPARAMETER pour variable spéciale globale
(defvar *manifest-db* nil
  "Connexion SQLite courante pour la persistance des manifests wasmCloud.")

;; MODIF: connexion locale + UNWIND-PROTECT, sans dépendre de la valeur finale de *manifest-db*
(defmacro with-db (db-path &body body)
  "Établit une connexion SQLite sur DB-PATH, liée dynamiquement à *MANIFEST-DB*."
  `(let ((conn (connect ,db-path)))
     (let ((*manifest-db* conn))
       (unwind-protect
            (progn ,@body)
         (disconnect conn)))))

;; Pour tester la macro
;; (macroexpand-1 '(with-db "./datas/wasmcloud.db" (init-db)))


;; MODIF: petit helper transactionnel
(defmacro with-transaction (&body body)
  "Exécute BODY dans une transaction SQLite."
  `(progn
     (execute-non-query *manifest-db* "BEGIN TRANSACTION")
     (unwind-protect
          (progn
            ,@body
            (execute-non-query *manifest-db* "COMMIT"))
       ;; En cas d'erreur, on rollback
       (ignore-errors
        (execute-non-query *manifest-db* "ROLLBACK")))))

;; ================================================
;;  S C H É M A   S Q L I T E
;; ================================================

(defun init-db ()
  "Initialise le schéma SQLite. Idempotent grâce à IF NOT EXISTS."
  (execute-non-query
   *manifest-db*
   "PRAGMA foreign_keys = ON;")

  ;; Manifest
  (execute-non-query
   *manifest-db*
   "CREATE TABLE IF NOT EXISTS manifest (
      name TEXT PRIMARY KEY
    );")

  ;; Cluster
  (execute-non-query
   *manifest-db*
   "CREATE TABLE IF NOT EXISTS clusters (
      name TEXT PRIMARY KEY,
      nats_url TEXT NOT NULL
    );")

  ;; Association manifest <-> cluster
  (execute-non-query
   *manifest-db*
   "CREATE TABLE IF NOT EXISTS cluster_manifest (
      manifest_name TEXT NOT NULL,
      cluster_name TEXT NOT NULL,
      PRIMARY KEY (manifest_name, cluster_name),
      FOREIGN KEY (manifest_name) REFERENCES manifest(name),
      FOREIGN KEY (cluster_name) REFERENCES clusters(name)
    );")

  ;; Nœuds (workers / leafs)
  (execute-non-query
   *manifest-db*
   "CREATE TABLE IF NOT EXISTS nodes (
      id TEXT PRIMARY KEY,
      type TEXT NOT NULL,
      zone TEXT NOT NULL
    );")

  (execute-non-query
   *manifest-db*
   "CREATE TABLE IF NOT EXISTS node_labels (
      node_id TEXT NOT NULL,
      label TEXT NOT NULL,
      PRIMARY KEY (node_id, label),
      FOREIGN KEY (node_id) REFERENCES nodes(id)
    );")

  ;; Association cluster <-> nodes
  (execute-non-query
   *manifest-db*
   "CREATE TABLE IF NOT EXISTS cluster_nodes (
      cluster_name TEXT NOT NULL,
      node_id TEXT NOT NULL,
      PRIMARY KEY (cluster_name, node_id),
      FOREIGN KEY (cluster_name) REFERENCES clusters(name),
      FOREIGN KEY (node_id) REFERENCES nodes(id)
    );")

  ;; Components (CLOS: component)
  ;; MODIF: PK composite (name, manifest_name)
  (execute-non-query
   *manifest-db*
   "CREATE TABLE IF NOT EXISTS component (
      name TEXT NOT NULL,
      image TEXT NOT NULL,
      replicas INTEGER NOT NULL,
      manifest_name TEXT NOT NULL,
      PRIMARY KEY (name, manifest_name),
      FOREIGN KEY (manifest_name) REFERENCES manifest(name)
    );")

  ;; Capabilities (optionnel, symétrique au component)
  (execute-non-query
   *manifest-db*
   "CREATE TABLE IF NOT EXISTS capability (
      name TEXT NOT NULL,
      image TEXT NOT NULL,
      manifest_name TEXT NOT NULL,
      PRIMARY KEY (name, manifest_name),
      FOREIGN KEY (manifest_name) REFERENCES manifest(name)
    );")

  ;; Spreadscaler
  ;; MODIF: référence complète sur (component_name, manifest_name)
  (execute-non-query
   *manifest-db*
   "CREATE TABLE IF NOT EXISTS spreadscaler (
      component_name TEXT NOT NULL,
      manifest_name TEXT NOT NULL,
      instances INTEGER NOT NULL,
      PRIMARY KEY (component_name, manifest_name),
      FOREIGN KEY (component_name, manifest_name)
        REFERENCES component(name, manifest_name)
    );")

  ;; Spread
  ;; MODIF: ajout de manifest_name pour FKs cohérentes
  (execute-non-query
   *manifest-db*
   "CREATE TABLE IF NOT EXISTS spread (
      name TEXT NOT NULL,
      weight INTEGER NOT NULL,
      zone TEXT NOT NULL,
      component_name TEXT NOT NULL,
      manifest_name TEXT NOT NULL,
      PRIMARY KEY (name, component_name, manifest_name),
      FOREIGN KEY (component_name, manifest_name)
        REFERENCES component(name, manifest_name)
    );")

  ;; Link (traits link) avec namespace / package / interfaces
  ;; MODIF: extension du schéma pour coller au modèle CLOS
  (execute-non-query
   *manifest-db*
   "CREATE TABLE IF NOT EXISTS link (
      target TEXT NOT NULL,
      component_name TEXT NOT NULL,
      manifest_name TEXT NOT NULL,
      namespace TEXT,
      package TEXT,
      interfaces TEXT,         -- liste sérialisée (ex: printed Lisp)
      config_name TEXT,        -- config liée (optionnelle)
      PRIMARY KEY (target, component_name, manifest_name),
      FOREIGN KEY (component_name, manifest_name)
        REFERENCES component(name, manifest_name)
    );")

  ;; Config (source_config pour link)
  ;; MODIF: manifest_name ajouté et FK composite
  (execute-non-query
   *manifest-db*
   "CREATE TABLE IF NOT EXISTS config (
      name TEXT NOT NULL,
      address TEXT NOT NULL,
      component_name TEXT NOT NULL,
      manifest_name TEXT NOT NULL,
      PRIMARY KEY (name, component_name, manifest_name),
      FOREIGN KEY (component_name, manifest_name)
        REFERENCES component(name, manifest_name)
    );")

  t)

;; ================================================
;;  N O D E S   /   C L U S T E R S
;; ================================================

(defmethod save-node-db ((node node))
  "Enregistre un worker ou leaf en base."
  (let ((type (cond
                ((typep node 'worker) "worker")
                ((typep node 'leaf) "leaf")
                (t (error "Unknown node type: ~S" node)))))
    (execute-non-query
     *manifest-db*
     "INSERT OR REPLACE INTO nodes (id, type, zone) VALUES (?, ?, ?)"
     (node-id node)
     type
     (node-zone node))
    ;; labels
    (execute-non-query
     *manifest-db*
     "DELETE FROM node_labels WHERE node_id = ?"
     (node-id node))
    (dolist (label (node-label node))
      (execute-non-query
       *manifest-db*
       "INSERT INTO node_labels (node_id, label) VALUES (?, ?)"
       (node-id node)
       label))))

(defun load-node-db (id)
  (let ((rows (sqlite:execute-to-list
               *manifest-db*
               "SELECT id, type, zone FROM nodes WHERE id = ?"
               id)))
    (when rows
      (destructuring-bind (nid type zone) (first rows)
        (let* ((labels (mapcar #'first
                               (sqlite:execute-to-list
                                   *manifest-db*
                                 "SELECT label FROM node_labels WHERE node_id = ?"
                                 nid)))
               (node (cond
                       ((string= type "worker")
                        (make-instance 'worker))
                       ((string= type "leaf")
                        (make-instance 'leaf))
                       (t
                        (error "Unknown node type ~S in nodes table" type)))))
          (setf (node-id node) nid
                (node-zone node) zone
                (node-label node) labels)
          node)))))


(defmethod save-cluster-db ((cluster cluster))
  "Enregistre un cluster (sans le manifest)."
  (let ((name (cluster-name cluster)))
    (execute-non-query
     *manifest-db*
     "INSERT OR REPLACE INTO clusters (name, nats_url) VALUES (?, ?)"
     name
     (cluster-nats-url cluster))
    (execute-non-query
     *manifest-db*
     "DELETE FROM cluster_nodes WHERE cluster_name = ?"
     name)
    (dolist (node (append (cluster-workers cluster)
                          (cluster-leafs cluster)))
      ;; Assure-toi que le node existe
      (save-node-db node)
      (execute-non-query
       *manifest-db*
       "INSERT OR REPLACE INTO cluster_nodes (cluster_name, node_id)
          VALUES (?, ?)"
       name
       (node-id node)))))

(defun load-cluster-db (name)
  "Charge un cluster par son nom."
  (let ((row (execute-to-list
              *manifest-db*
              "SELECT name, nats_url FROM clusters WHERE name = ?"
              name)))
    (when row
      (destructuring-bind (cname nats-url) (first row)
        (let* ((node-ids (mapcar #'second
                                 (execute-to-list
                                  *manifest-db*
                                  "SELECT cluster_name, node_id FROM cluster_nodes WHERE cluster_name = ?"
                                  cname)))
               (nodes (mapcar #'load-node-db node-ids)))
          (make-instance 'cluster
                         :name cname
                         :nats-url nats-url
                         ;; MODIF: filtrage simplifié
                         :workers (remove-if-not (lambda (n) (typep n 'worker))
                                                 nodes)
                         :leafs   (remove-if-not (lambda (n) (typep n 'leaf))
                                                 nodes)))))))

;; ================================================
;;  S P R E A D S C A L E R   /   S P R E A D
;; ================================================

(defmethod save-spread-db ((s spread) component-name manifest-name)
  "Enregistre un spread pour un composant dans un manifest."
  (execute-non-query
   *manifest-db*
   "INSERT OR REPLACE INTO spread
      (name, weight, zone, component_name, manifest_name)
    VALUES (?, ?, ?, ?, ?)"
   (spread-name s)
   (spread-weight s)
   (spread-zone s)
   component-name
   manifest-name))

(defmethod load-spread-db (component-name manifest-name)
  "Charge tous les spreads d'un composant/manifest."
  (let ((rows (execute-to-list
               *manifest-db*
               "SELECT name, weight, zone FROM spread
                WHERE component_name = ? AND manifest_name = ?"
               component-name manifest-name)))
    (mapcar (lambda (r)
              (destructuring-bind (name weight zone) r
                (make-spread :name name
                             :weight weight
                             :zone zone)))
            rows)))


(defmethod save-spreadscaler-db ((s spreadscaler)
                                 component-name manifest-name)
  "Enregistre un spreadscaler et ses spreads."
  (execute-non-query
   *manifest-db*
   "INSERT OR REPLACE INTO spreadscaler
        (component_name, manifest_name, instances)
      VALUES (?, ?, ?)"
   component-name manifest-name
   (spreadscaler-instances s))
  (execute-non-query
   *manifest-db*
   "DELETE FROM spread
      WHERE component_name = ? AND manifest_name = ?"
   component-name manifest-name)
  (dolist (sp (spreadscaler-spread s))
    (save-spread-db sp component-name manifest-name)))

(defun load-spreadscaler-db (component-name manifest-name)
  "Charge un spreadscaler complet (instances + spreads)."
  (let ((row (execute-to-list
              *manifest-db*
              "SELECT instances FROM spreadscaler
               WHERE component_name = ? AND manifest_name = ?"
              component-name manifest-name)))
    (when row
      (let ((instances (first (first row)))
            (spreads (load-spread-db component-name manifest-name)))
        (make-spreadscaler
         :instances instances
         :spread spreads)))))

(defun delete-spreadscaler-db (component-name manifest-name)
  "Supprime le spreadscaler et tous ses spreads pour un composant/manifest."
  ;; D'abord les spreads (dépendants)
  (sqlite:execute-non-query
   *manifest-db*
   "DELETE FROM spread WHERE component_name = ? AND manifest_name = ?"
   component-name manifest-name)
  ;; Puis le spreadscaler
  (sqlite:execute-non-query
   *manifest-db*
   "DELETE FROM spreadscaler WHERE component_name = ? AND manifest_name = ?"
   component-name manifest-name))

(defun delete-link-db (component-name manifest-name &optional target)
  "Supprime les links (et leurs configs associées) pour un component/manifest.
Si TARGET est non-nil, ne supprime que les links ayant ce target."
  ;; On récupère d'abord les éventuels config_name associés
  (let* ((rows (if target
                   (sqlite:execute-to-list
                    *manifest-db*
                    "SELECT config_name FROM link
                       WHERE component_name = ? AND manifest_name = ? AND target = ?"
                    component-name manifest-name target)
                   (sqlite:execute-to-list
                    *manifest-db*
                    "SELECT config_name FROM link
                       WHERE component_name = ? AND manifest_name = ?"
                    component-name manifest-name)))
         (config-names (remove nil (mapcar #'first rows))))
    ;; Supprimer les links
    (if target
        (sqlite:execute-non-query
         *manifest-db*
         "DELETE FROM link
            WHERE component_name = ? AND manifest_name = ? AND target = ?"
         component-name manifest-name target)
        (sqlite:execute-non-query
         *manifest-db*
         "DELETE FROM link
            WHERE component_name = ? AND manifest_name = ?"
         component-name manifest-name))
    ;; Supprimer les configs associées (si tu veux éviter les orphelins)
    (dolist (cfg-name config-names)
      (sqlite:execute-non-query
       *manifest-db*
       "DELETE FROM config
          WHERE name = ? AND component_name = ? AND manifest_name = ?"
       cfg-name component-name manifest-name))))

(defun delete-manifest-db (manifest-name)
  "Supprime un manifest et toutes les données associées (components, traits...).
Ne touche pas aux clusters/nodes directement (ils peuvent être partagés)."
  (with-transaction
    ;; 1) Récupérer les components du manifest
    (let ((components (mapcar #'first
                              (sqlite:execute-to-list
                               *manifest-db*
                               "SELECT name FROM component
                                  WHERE manifest_name = ?"
                               manifest-name))))
      (dolist (cname components)
        ;; a) supprimer links (et configs associées)
        (delete-link-db cname manifest-name nil)
        ;; b) supprimer spreads + spreadscaler
        (sqlite:execute-non-query
         *manifest-db*
         "DELETE FROM spread
            WHERE component_name = ? AND manifest_name = ?"
         cname manifest-name)
        (sqlite:execute-non-query
         *manifest-db*
         "DELETE FROM spreadscaler
            WHERE component_name = ? AND manifest_name = ?"
         cname manifest-name)
        ;; c) supprimer le component lui-même
        (sqlite:execute-non-query
         *manifest-db*
         "DELETE FROM component
            WHERE name = ? AND manifest_name = ?"
         cname manifest-name)))

    ;; 2) Supprimer les capabilities liées au manifest
    (sqlite:execute-non-query
     *manifest-db*
     "DELETE FROM capability
        WHERE manifest_name = ?"
     manifest-name)

    ;; 3) Supprimer les associations manifest <-> cluster
    (sqlite:execute-non-query
     *manifest-db*
     "DELETE FROM cluster_manifest
        WHERE manifest_name = ?"
     manifest-name)

    ;; 4) Enfin, supprimer le manifest lui-même
    (sqlite:execute-non-query
     *manifest-db*
     "DELETE FROM manifest WHERE name = ?"
     manifest-name)))

;; ================================================
;;  L I N K   /   C O N F I G
;; ================================================

(defmethod save-config-db ((c config) component-name manifest-name)
  "Enregistre une config associée à un composant dans un manifest."
  (execute-non-query
   *manifest-db*
   "INSERT OR REPLACE INTO config
      (name, address, component_name, manifest_name)
    VALUES (?, ?, ?, ?)"
   (config-name c)
   (config-address c)
   component-name
   manifest-name))

(defun load-config-db (name component-name manifest-name)
  "Charge une config précise."
  (let ((row (execute-to-list
              *manifest-db*
              "SELECT name, address FROM config
               WHERE name = ? AND component_name = ? AND manifest_name = ?"
              name component-name manifest-name)))
    (when row
      (destructuring-bind (cname address) (first row)
        (make-config :name cname :address address)))))

(defmethod save-link-db ((l link) component-name manifest-name)
  "Enregistre un trait link pour un composant/manifest.
   Sérialise la liste d'interfaces en string (prin1)."
  (when (link-source l)
    (save-config-db (link-source l) component-name manifest-name))
  (execute-non-query
   *manifest-db*
   "INSERT OR REPLACE INTO link
        (target, component_name, manifest_name,
         namespace, package, interfaces, config_name)
      VALUES (?, ?, ?, ?, ?, ?, ?)"
   (link-target l)
   component-name
   manifest-name
   (link-namespace l)
   (link-pack l)
   (prin1-to-string (link-interfaces l))
   (and (link-source l)
        (config-name (link-source l)))))

(defun load-link-db (component-name manifest-name)
  "Charge tous les links pour un composant/manifest.
   Ne recharge qu'un seul link si besoin, à adapter selon ton usage."
  (let ((rows (execute-to-list
               *manifest-db*
               "SELECT target, namespace, package, interfaces, config_name
                FROM link
                WHERE component_name = ? AND manifest_name = ?"
               component-name manifest-name)))
    (mapcar (lambda (r)
              (destructuring-bind (target ns pack ifaces-str cfg-name) r
                (let* ((interfaces (read-from-string ifaces-str))
                       (cfg (when cfg-name
                              (load-config-db cfg-name component-name manifest-name))))
                  (make-link
                   :target target
                   :namespace ns
                   :pack pack
                   :interfaces interfaces
                   :source cfg))))
            rows)))

;; ================================================
;;  M A N I F E S T   /   C O M P O N E N T S
;; ================================================

(defmethod save-component-db ((c component) manifest-name)
  "Enregistre un composant pour un manifest donné."
  (execute-non-query
   *manifest-db*
   "INSERT OR REPLACE INTO component
        (name, image, replicas, manifest_name)
      VALUES (?, ?, ?, ?)"
   (instance-name c)
   (instance-image c)
   (component-replicas c)
   manifest-name))

(defun load-component-db (name manifest-name)
  "Charge un composant par son nom et le nom de son manifest."
  (let ((rows (execute-to-list
               *manifest-db*
               "SELECT name, image, replicas FROM component
                WHERE name = ? AND manifest_name = ?"
               name manifest-name)))
    (when rows
      (destructuring-bind (cname image replicas) (first rows)
        ;; Les traits (spreadscaler, link, etc.) sont à recharger à part
        (make-instance 'component
                       :name cname
                       :image image
                       :replicas replicas
                       :traits '())))))

(defmethod save-capability-db ((c capability) manifest-name)
  "Enregistre un composant pour un manifest donné."
  (execute-non-query
   *manifest-db*
   "INSERT OR REPLACE INTO capability
        (name, image, manifest_name)
      VALUES (?, ?, ?)"
   (instance-name c)
   (instance-image c)
   manifest-name))

(defun load-capability-db (name manifest-name)
  "Charge un capability par son nom et le nom de son manifest."
  (let ((rows (execute-to-list
               *manifest-db*
               "SELECT name, image FROM capability
                WHERE name = ? AND manifest_name = ?"
               name manifest-name)))
    (when rows
      (destructuring-bind (cname image ) (first rows)
        ;; Les traits (spreadscaler, link, etc.) sont à recharger à part
        (make-instance 'capability
                       :name cname
                       :image image
                       :traits '())))))

(defmethod save-manifest-db ((m manifest))
  "Enregistre un manifest complet (manifest + cluster + components).
   On suppose que les traits des components sont enregistrés à part."
  (let ((name (manifest-name m))
        (cluster (manifest-cluster m)))
    (with-transaction
      ;; manifest
      (execute-non-query
       *manifest-db*
       "INSERT OR REPLACE INTO manifest (name) VALUES (?)"
       name)
      ;; cluster
      (save-cluster-db cluster)
      ;; association manifest <-> cluster
      (execute-non-query
       *manifest-db*
       "INSERT OR REPLACE INTO cluster_manifest (manifest_name, cluster_name)
        VALUES (?, ?)"
       name
       (cluster-name cluster))
      ;; components
      (dolist (comp (manifest-components m))
        (save-component-db comp name)))))

(defun load-manifest-db (name)
  "Charge un manifest et son cluster.
   Ne recharge pas ici tous les components/traits pour garder la fonction simple."
  (let ((cluster-name (execute-single
                       *manifest-db*
                       "SELECT cluster_name FROM cluster_manifest
                        WHERE manifest_name = ?"
                       name)))
    (when cluster-name
      (let ((cluster (load-cluster-db cluster-name)))
        (make-instance 'manifest
                       :name name
                       :cluster cluster
                       :components '())))))

;; ================================================
;;  R E C U R E N C Y
;; ================================================

(defvar *current-manifest-name* nil
  "Nom du manifest actuellement persisté, utilisé pour les FKs.")

(defmethod persist ((m manifest) &key component-name)
  (declare (ignore component-name))
  (let ((*current-manifest-name* (manifest-name m)))
    ;; 1) manifest
    (execute-non-query
     *manifest-db*
     "INSERT OR REPLACE INTO manifest (name) VALUES (?)"
     *current-manifest-name*)

    ;; 2) cluster (et nodes)
    (persist (manifest-cluster m))

    ;; 3) association manifest <-> cluster
    (execute-non-query
     *manifest-db*
     "INSERT OR REPLACE INTO cluster_manifest (manifest_name, cluster_name)
        VALUES (?, ?)"
     *current-manifest-name*
     (cluster-name (manifest-cluster m)))

    ;; 4) components et capabilities
    (dolist (comp (manifest-components m))
      (persist comp))))

(defmethod persist ((c cluster) &key component-name)
  (declare (ignore component-name))
  (save-cluster-db c))

(defmethod persist ((c component) &key component-name)
  (declare (ignore component-name))
  (save-component-db c *current-manifest-name*)
  (dolist (trait (instance-traits c))
    (persist trait :component-name (instance-name c))))

(defmethod persist ((c capability) &key component-name)
  (declare (ignore component-name))
  (save-capability-db c *current-manifest-name*)

  (dolist (trait (instance-traits c))
    (persist trait :component-name (instance-name c))))

(defmethod persist ((s spreadscaler) &key component-name)
  (save-spreadscaler-db s component-name *current-manifest-name*))

(defmethod persist ((l link) &key component-name)
  (save-link-db l component-name *current-manifest-name*))

(defun save-full-manifest (manifest)
  "Persiste récursivement MANIFEST (cluster, nodes, components, traits...)."
  (with-transaction
    (persist manifest)))

;; ================================================
;;  L O A D   F R O M   D B 
;; ================================================

(defun load-components-for-manifest (manifest-name)
  "Charge tous les components d'un manifest, avec leurs traits (spreadscaler, link...)."
  (let ((rows (execute-to-list
               *manifest-db*
               "SELECT name, image, replicas
                  FROM component
                 WHERE manifest_name = ?"
               manifest-name)))
    (mapcar (lambda (row)
              (destructuring-bind (name image replicas) row
                ;; traits
                (let* ((spreadscaler (load-spreadscaler-db name manifest-name))
                       (links (load-link-db name manifest-name))
                       (traits (append (when spreadscaler (list spreadscaler))
                                       links)))
                  (make-component :name name
                                  :image image
                                  :replicas replicas
                                  :traits traits))))
            rows)))

(defun load-capabilities-for-manifest (manifest-name)
  "Charge toutes les capabilities d'un manifest, avec leurs traits (par ex. link)."
  (let ((rows (execute-to-list
               *manifest-db*
               "SELECT name, image
                  FROM capability
                 WHERE manifest_name = ?"
               manifest-name)))
    (mapcar (lambda (row)
              (destructuring-bind (name image) row
                (let* ((links (load-link-db name manifest-name)) 
                       (traits links))
                  (make-capability :name name
                                   :image image
                                   :traits traits))))
            rows)))

(defun load-full-manifest-from-db (manifest-name)
  "Reconstruit un manifest complet (cluster + components + traits) à partir de SQLite."
  (let ((m (load-manifest-db manifest-name)))
    (unless m
      (error "Aucun manifest nommé ~S trouvé en base." manifest-name))
    (let* ((components   (load-components-for-manifest manifest-name))
           (capabilities (load-capabilities-for-manifest manifest-name)) 
           (all (append components capabilities)))
      (setf (manifest-components m) all)
      m)))

(defun export-manifest-from-db (manifest-name pathname)
  "Recharge le manifest MANIFEST-NAME depuis SQLite et l'enregistre en JSON dans PATHNAME."
  (with-db "./datas/wasmcloud.db"
    (let ((m (load-full-manifest-from-db manifest-name)))
      (save-manifest m pathname)
      m)))
