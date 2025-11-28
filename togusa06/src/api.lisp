(in-package :togusa)


;; ================================================
;;  S E R V E R
;; ================================================

;; (defun start-api ()
;;   "Démarre le serveur REST sur *API-PORT*."
;;   (tbnl:start
;;    (make-instance 'tbnl:easy-acceptor :port *api-port*)))

;; (defun stop-api ()
;;   "Arrête le serveur REST."
;;   (tbnl:stop
;;    (first tbnl:*acceptor*)))

(defun start-server (&key (port *api-port*))
  (format t "~&Starting API on port ~a~&" port)
  (force-output)
  (setf *server*
        (tbnl:start
         (make-instance 'tbnl:easy-acceptor :port port))))

(defun stop-server ()
  (when *server*
    (tbnl:stop *server*)
    (setf *server* nil)))

(defun parse-json-body ()
  "Lit le corps JSON de la requête et retourne un hash-table (cl-json)."
  (let* ((raw (tbnl:raw-post-data :force-text t)))
    (decode-json-from-string raw)))

;; ================================================
;;  T O O L
;; ================================================

(defun json-response (data &key (code 200))
  (setf (tbnl:content-type*) "application/json"
        (tbnl:return-code*) code)
  (encode-json-to-string data))

;; ================================================
;;  H E L P E R
;; ================================================

(defun find-component-in-manifest (manifest component-name)
  (find component-name
        (manifest-components manifest)
        :key #'instance-name
        :test #'string=))

(defun find-link-in-component (component)
  (find-if (lambda (l) (typep l 'link))
           (instance-traits component)))

(defun remove-spreadscaler-trait (component)
  "Retire le trait spreadscaler (s'il existe) des traits d'un component."
  (setf (instance-traits component)
        (remove-if (lambda (tt) (typep tt 'spreadscaler))
                   (instance-traits component))))

(defun find-spreadscaler-trait (component)
  "Retourne le premier trait de type SPREADSCALER dans COMPONENT, ou NIL."
  (find-if (lambda (tt) (typep tt 'spreadscaler))
           (instance-traits component)))

(defun find-links-in-component (component)
  "Retourne la liste des traits de type LINK dans COMPONENT."
  (remove-if-not (lambda (tt) (typep tt 'link))
                 (instance-traits component)))

(defun remove-links-from-component (component &optional target)
  "Retire les traits LINK du composant.
Si TARGET est fourni, ne supprime que les links ayant ce target."
  (setf (instance-traits component)
        (remove-if (lambda (tt)
                     (and (typep tt 'link)
                          (or (null target)
                              (string= (link-target tt) target))))
                   (instance-traits component))))

(defun find-link-in-component-by-target (component target)
  "Retourne le premier trait LINK dans COMPONENT dont le TARGET correspond."
  (find-if (lambda (tt)
             (and (typep tt 'link)
                  (string= (link-target tt) target)))
           (instance-traits component)))

;; ================================================
;;  H A N D L E R
;; ================================================

(define-easy-handler (create-manifest-api :uri "/api/manifest") ()
  (let* ((body (parse-json-body))
         ;; body est une alist du type ((:NAME . "...") (:CLUSTER . <alist>))
         (name (cdr (assoc :name body)))
         (cluster-hash (cdr (assoc :cluster body)))
         (cluster-name (or (and cluster-hash (cdr (assoc :name cluster-hash)))
                           "wasmcloud"))
         (nats-url (or (and cluster-hash (cdr (assoc :nats-url cluster-hash)))
                       "nats://localhost:4222")))
    (with-db "./datas/wasmcloud.db"
      (init-db)
      (let* ((cluster (make-cluster :name cluster-name
                                    :nats-url nats-url
                                    :workers '()
                                    :leafs '()))
             (manifest (make-manifest :name name
                                      :cluster cluster
                                      :components '())))
        (save-full-manifest manifest)
        (json-response (list :status "ok"
                             :manifest name)
                       :code 201)))))

(define-easy-handler (add-component-api :uri "/api/component") ()
  (let* ((body (parse-json-body))
         (manifest-name (cdr(assoc :manifest body)))
         (cname (cdr(assoc :name body)))
         (image (cdr(assoc :image body)))
         (replicas (or (cdr(assoc :replicas body)) 1)))
    (with-db "./datas/wasmcloud.db"
      (let ((manifest (load-full-manifest-from-db manifest-name)))
        (unless manifest
          (return-from add-component-api
            (json-response (list :error "manifest not found")
                           :code 404)))
        ;; construire le component
        (let ((comp (make-component :name cname
                                    :image image
                                    :replicas replicas
                                    :traits '())))
          ;; on l’ajoute en mémoire
          (push comp (manifest-components manifest))
          ;; on repersiste tout
          (save-full-manifest manifest)
          (json-response (list :status "ok"
                               :manifest manifest-name
                               :component cname)
                         :code 201))))))

(define-easy-handler (add-link-api :uri "/api/link") ()
  (let* ((body (parse-json-body))
         (manifest-name (cdr (assoc :manifest body)))
         (component-name (cdr(assoc :component body)))
         (target (cdr(assoc :target body)))
         (namespace (cdr(assoc :namespace body)))
         (pack (cdr(assoc :package body)))
         (interfaces (cdr(assoc :interface body))))
    (with-db "./datas/wasmcloud.db"
      (let ((manifest (load-full-manifest-from-db manifest-name)))
        (unless manifest
          (return-from add-link-api
            (json-response (list :error "manifest not found")
                           :code 404)))
        (let ((comp (find-component-in-manifest manifest component-name)))
          (unless comp
            (return-from add-link-api
              (json-response (list :error "component not found")
                             :code 404)))
          ;; créer le link (sans config pour l'instant)
          (let ((link (make-link :target target
                                 :pack pack
                                 :namespace namespace
                                 :interfaces interfaces
                                 :source nil)))
            ;; ajouter le trait
            (push link (instance-traits comp))
            ;; repersister
            (save-full-manifest manifest)
            (json-response (list :status "ok"
                                 :manifest manifest-name
                                 :component component-name)
                           :code 201)))))))

(define-easy-handler (add-config-api :uri "/api/config") ()
  (let* ((body (parse-json-body))
         (manifest-name (cdr(assoc :manifest body)))
         (component-name (cdr(assoc :component body)))
         (cfg-name (cdr(assoc :name body)))
         (address (cdr(assoc :address body))))
    (with-db "./datas/wasmcloud.db"
      (let ((manifest (load-full-manifest-from-db manifest-name)))
        (unless manifest
          (return-from add-config-api
            (json-response (list :error "manifest not found")
                           :code 404)))
        (let ((comp (find-component-in-manifest manifest component-name)))
          (unless comp
            (return-from add-config-api
              (json-response (list :error "component not found")
                             :code 404)))
          (let ((link (find-link-in-component comp)))
            (unless link
              (return-from add-config-api
                (json-response (list :error "no link found for component")
                               :code 400)))
            ;; créer la config et l’attacher au link
            (let ((cfg (make-config :name cfg-name
                                    :address address)))
              (setf (link-source link) cfg)
              ;; repersister tout
              (save-full-manifest manifest)
              (json-response (list :status "ok"
                                   :manifest manifest-name
                                   :component component-name
                                   :config cfg-name)
                             :code 201))))))))

(define-easy-handler (get-manifest-api :uri "/api/manifest") (name)
  "Retourne le manifest complet sous forme JSON."
  (with-db "./datas/wasmcloud.db"
    (let ((manifest (load-full-manifest-from-db name)))
      (if manifest
          (progn
            (setf (hunchentoot:content-type*) "application/json; charset=utf-8")
            (json-to-string-manifest manifest))  ;; <== ici : on lui passe le manifest
          (json-response 
           (list :error (format nil "Manifest ~A not found" name))
           :code 404)))))

(define-easy-handler
    (update-component-api
     :uri "/api/component"
     :default-request-type :PUT) ()   ;; important : méthode HTTP PUT
  ;; Met à jour un component existant dans un manifest.
  (let* ((body (parse-json-body))
         ;; body est une alist : ((:MANIFEST . "...") (:NAME . "...") ...)
         (manifest-name (cdr (assoc :manifest body)))
         (cname         (cdr (assoc :name body)))
         (new-image     (cdr (assoc :image body)))     ;; optionnel
         (new-replicas  (cdr (assoc :replicas body)))) ;; optionnel
    (with-db "./datas/wasmcloud.db"
      (let ((manifest (load-full-manifest-from-db manifest-name)))
        (unless manifest
          (return-from update-component-api
            (json-response
             (list :error (format nil "Manifest ~A not found" manifest-name))
             :code 404)))
        (let ((comp (find-component-in-manifest manifest cname)))
          (unless comp
            (return-from update-component-api
              (json-response
               (list :error (format nil "Component ~A not found in manifest ~A"
                                    cname manifest-name))
               :code 404)))
          ;; On applique uniquement les champs présents
          (when new-image
            (setf (instance-image comp) new-image))
          (when new-replicas
            (setf (component-replicas comp) new-replicas))

          ;; On repersiste tout le manifest modifié
          (save-full-manifest manifest)

          ;; Réponse JSON
          (json-response
           (list :status "ok"
                 :manifest manifest-name
                 :component cname
                 :image (instance-image comp)
                 :replicas (component-replicas comp))
           :code 200))))))

(define-easy-handler
    (delete-spreadscaler-api
     :uri "/api/spreadscaler"
     :default-request-type :DELETE) ()
  "Supprime le spreadscaler d'un component dans un manifest."
  (let* ((body (parse-json-body))
         ;; body = ((:MANIFEST . "...") (:COMPONENT . "..."))
         (manifest-name  (cdr (assoc :manifest body)))
         (component-name (cdr (assoc :component body))))
    (with-db "./datas/wasmcloud.db"
      (let ((manifest (load-full-manifest-from-db manifest-name)))
        (unless manifest
          (return-from delete-spreadscaler-api
            (json-response
             (list :error (format nil "Manifest ~A not found" manifest-name))
             :code 404)))
        (let ((comp (find-component-in-manifest manifest component-name)))
          (unless comp
            (return-from delete-spreadscaler-api
              (json-response
               (list :error (format nil "Component ~A not found in manifest ~A"
                                    component-name manifest-name))
               :code 404)))
          ;; 1) enlever le trait spreadscaler en mémoire
          (remove-spreadscaler-trait comp)
          ;; 2) supprimer en base (spreadscaler + spreads)
          (delete-spreadscaler-db component-name manifest-name)
          ;; 3) (optionnel mais propre) re-persister le manifest sans ce trait
          (save-full-manifest manifest)
          ;; 4) réponse
          (json-response
           (list :status "ok"
                 :manifest manifest-name
                 :component component-name
                 :deleted "spreadscaler")
           :code 200))))))

(define-easy-handler
    (add-spreadscaler-api
     :uri "/api/spreadscaler"
     :default-request-type :POST) ()
  "Ajoute (ou remplace) un spreadscaler + ses spreads sur un component."
  (let* ((body (parse-json-body))
         ;; body est une alist: ((:MANIFEST . "...") (:COMPONENT . "...") ...)
         (manifest-name  (cdr (assoc :manifest body)))
         (component-name (cdr (assoc :component body)))
         (instances      (cdr (assoc :instances body)))
         (spread-raw     (cdr (assoc :spread body))))  ;; liste d'alists
    (with-db "./datas/wasmcloud.db"
      ;; 1) Charger le manifest complet
      (let ((manifest (load-full-manifest-from-db manifest-name)))
        (unless manifest
          (return-from add-spreadscaler-api
            (json-response
             (list :error (format nil "Manifest ~A not found" manifest-name))
             :code 404)))
        ;; 2) Trouver le component
        (let ((comp (find-component-in-manifest manifest component-name)))
          (unless comp
            (return-from add-spreadscaler-api
              (json-response
               (list :error (format nil "Component ~A not found in manifest ~A"
                                    component-name manifest-name))
               :code 404)))
          ;; 3) Construire les SPREADs à partir du JSON
          (let* ((spreads
                   (mapcar (lambda (s)
                             (make-spread
                              :name   (cdr (assoc :name s))
                              :weight (cdr (assoc :weight s))
                              :zone   (cdr (assoc :zone s))))
                           spread-raw)))
            ;; 4) Validation de la somme des weights (optionnel mais utile)
            (unless (valid-spread-p spreads)
              (return-from add-spreadscaler-api
                (json-response
                 (list :error "Spread weights must sum to 100")
                 :code 400)))
            ;; 5) Construire le SPREADSCALER
            (let ((scaler (make-spreadscaler
                           :instances instances
                           :spread spreads)))
              ;; 6) Enlever un spreadscaler existant éventuel
              (remove-spreadscaler-trait comp)
              ;; 7) Ajouter le nouveau trait
              (push scaler (instance-traits comp))
              ;; 8) Repersister tout le manifest (save-full-manifest s'occupera
              ;;    d'appeler les fonctions DB nécessaires pour spreadscaler+spread)
              (save-full-manifest manifest)
              ;; 9) Réponse JSON
              (json-response
               (list :status "ok"
                     :manifest manifest-name
                     :component component-name
                     :instances instances
                     :spread
                     (mapcar (lambda (s)
                               (list :name   (spread-name s)
                                     :weight (spread-weight s)
                                     :zone   (spread-zone s)))
                             spreads))
               :code 201))))))))

(define-easy-handler
    (get-spreadscaler-api :uri "/api/spreadscaler") (manifest component)
  "Retourne la config du spreadscaler pour un component donné dans un manifest.
   Exemple: GET /api/spreadscaler?manifest=example-app&component=http-component"
  ;; manifest et component sont récupérés automatiquement depuis la query string
  (with-db "./datas/wasmcloud.db"
    (let ((m (load-full-manifest-from-db manifest)))
      (unless m
        (return-from get-spreadscaler-api
          (json-response
           (list :error (format nil "Manifest ~A not found" manifest))
           :code 404)))
      (let ((comp (find-component-in-manifest m component)))
        (unless comp
          (return-from get-spreadscaler-api
            (json-response
             (list :error (format nil "Component ~A not found in manifest ~A"
                                  component manifest))
             :code 404)))
        (let ((sc (find-spreadscaler-trait comp)))
          (unless sc
            (return-from get-spreadscaler-api
              (json-response
               (list :error (format nil "No spreadscaler found for component ~A in manifest ~A"
                                    component manifest))
               :code 404)))
          ;; Succès : on renvoie la config du spreadscaler
          (json-response
           (list :manifest manifest
                 :component component
                 :instances (spreadscaler-instances sc)
                 :spread (mapcar (lambda (s)
                                   (list :name   (spread-name s)
                                         :weight (spread-weight s)
                                         :zone   (spread-zone s)))
                                 (spreadscaler-spread sc)))
           :code 200))))))

(define-easy-handler
    (get-link-api :uri "/api/link") (manifest component)
  "Retourne la liste des links pour un component donné dans un manifest.
   Exemple: GET /api/link?manifest=example-app&component=http-component"
  ;; manifest et component sont récupérés automatiquement depuis la query string
  (with-db "./datas/wasmcloud.db"
    (let ((m (load-full-manifest-from-db manifest)))
      (unless m
        (return-from get-link-api
          (json-response
           (list :error (format nil "Manifest ~A not found" manifest))
           :code 404)))
      (let ((comp (find-component-in-manifest m component)))
        (unless comp
          (return-from get-link-api
            (json-response
             (list :error (format nil "Component ~A not found in manifest ~A"
                                  component manifest))
             :code 404)))
        (let ((links (find-links-in-component comp)))
          (unless links
            (return-from get-link-api
              (json-response
               (list :error (format nil "No links found for component ~A in manifest ~A"
                                    component manifest))
               :code 404)))
          ;; Succès : on renvoie une liste de links
          (json-response
           (list
            :manifest manifest
            :component component
            :links
            (mapcar (lambda (l)
                      (let ((cfg (link-source l)))
                        (list
                         :target     (link-target l)
                         :namespace  (link-namespace l)
                         :package    (link-pack l)
                         :interfaces (link-interfaces l)
                         :config (when cfg
                                   (list :name    (config-name cfg)
                                         :address (config-address cfg))))))
                    links))
           :code 200))))))

(define-easy-handler
    (add-link-api
     :uri "/api/link"
     :default-request-type :POST) ()
  "Ajoute un link à un component dans un manifest."
  (let* ((body (parse-json-body))
         (manifest-name  (cdr (assoc :manifest body)))
         (component-name (cdr (assoc :component body)))
         (target         (cdr (assoc :target body)))
         (namespace      (cdr (assoc :namespace body)))
         (pack           (cdr (assoc :package body)))
         (interfaces     (cdr (assoc :interfaces body))))  ;; liste de strings
    (with-db "./datas/wasmcloud.db"
      (let ((manifest (load-full-manifest-from-db manifest-name)))
        (unless manifest
          (return-from add-link-api
            (json-response
             (list :error (format nil "Manifest ~A not found" manifest-name))
             :code 404)))
        (let ((comp (find-component-in-manifest manifest component-name)))
          (unless comp
            (return-from add-link-api
              (json-response
               (list :error (format nil "Component ~A not found in manifest ~A"
                                    component-name manifest-name))
               :code 404)))
          ;; créer le link (sans config pour l'instant)
          (let ((link (make-link :target target
                                 :pack pack
                                 :namespace namespace
                                 :interfaces interfaces
                                 :source nil)))
            ;; ajouter le trait au composant
            (push link (instance-traits comp))
            ;; persister tout
            (save-full-manifest manifest)
            ;; réponse
            (json-response
             (list :status "ok"
                   :manifest manifest-name
                   :component component-name
                   :link (list :target     (link-target link)
                               :namespace  (link-namespace link)
                               :package    (link-pack link)
                               :interfaces (link-interfaces link)))
             :code 201)))))))

(define-easy-handler
    (delete-link-api
     :uri "/api/link/delete"
     :default-request-type :DELETE) ()
  "Supprime un ou plusieurs links pour un component dans un manifest."
  (let* ((body (parse-json-body))
         (manifest-name  (cdr (assoc :manifest body)))
         (component-name (cdr (assoc :component body)))
         (target         (cdr (assoc :target body))))   ;; optionnel
    (with-db "./datas/wasmcloud.db"
      (let ((manifest (load-full-manifest-from-db manifest-name)))
        (unless manifest
          (return-from delete-link-api
            (json-response
             (list :error (format nil "Manifest ~A not found" manifest-name))
             :code 404)))
        (let ((comp (find-component-in-manifest manifest component-name)))
          (unless comp
            (return-from delete-link-api
              (json-response
               (list :error (format nil "Component ~A not found in manifest ~A"
                                    component-name manifest-name))
               :code 404)))
          (remove-links-from-component comp target)
          (delete-link-db component-name manifest-name target)
          (save-full-manifest manifest)
          (json-response
           (list :status "ok"
                 :manifest manifest-name
                 :component component-name
                 :deleted (if target
                              (format nil "links with target ~A" target)
                              "all links"))
           :code 200))))))

(define-easy-handler
    (update-link-api
     :uri "/api/link/update"
     :default-request-type :PUT) ()
  "Met à jour un link existant."
  (let* ((body (parse-json-body))
         (manifest-name  (cdr (assoc :manifest body)))
         (component-name (cdr (assoc :component body)))
         (target         (cdr (assoc :target body)))        ;; ancien target
         (new-target     (cdr (assoc :new-target body)))    ;; optionnel
         (namespace      (cdr (assoc :namespace body)))     ;; optionnel
         (pack           (cdr (assoc :package body)))       ;; optionnel
         (interfaces     (cdr (assoc :interfaces body)))    ;; optionnel
         (cfg-hash       (cdr (assoc :config body))))       ;; optionnel
    (with-db "./datas/wasmcloud.db"
      (let ((manifest (load-full-manifest-from-db manifest-name)))
        (unless manifest
          (return-from update-link-api
            (json-response
             (list :error (format nil "Manifest ~A not found" manifest-name))
             :code 404)))
        (let ((comp (find-component-in-manifest manifest component-name)))
          (unless comp
            (return-from update-link-api
              (json-response
               (list :error (format nil "Component ~A not found in manifest ~A"
                                    component-name manifest-name))
               :code 404)))
          (let ((link (find-link-in-component-by-target comp target)))
            (unless link
              (return-from update-link-api
                (json-response
                 (list :error (format nil "Link with target ~A not found for component ~A in manifest ~A"
                                      target component-name manifest-name))
                 :code 404)))
            ;; mises à jour
            (when new-target
              (setf (link-target link) new-target))
            (when namespace
              (setf (link-namespace link) namespace))
            (when pack
              (setf (link-pack link) pack))
            (when interfaces
              (setf (link-interfaces link) interfaces))
            (when cfg-hash
              (let* ((cfg-name    (cdr (assoc :name cfg-hash)))
                     (cfg-address (cdr (assoc :address cfg-hash))))
                (if (link-source link)
                    (progn
                      (when cfg-name
                        (setf (config-name (link-source link)) cfg-name))
                      (when cfg-address
                        (setf (config-address (link-source link)) cfg-address)))
                    (setf (link-source link)
                          (make-config :name cfg-name
                                       :address cfg-address)))))
            (when new-target
              (delete-link-db component-name manifest-name target))
            (save-full-manifest manifest)
            (let ((cfg (link-source link)))
              (json-response
               (list
                :status "ok"
                :manifest manifest-name
                :component component-name
                :link (list
                       :target     (link-target link)
                       :namespace  (link-namespace link)
                       :package    (link-pack link)
                       :interfaces (link-interfaces link)
                       :config (when cfg
                                 (list :name    (config-name cfg)
                                       :address (config-address cfg)))))
               :code 200))))))))

(define-easy-handler
    (list-manifests-api :uri "/api/manifests") ()
  "Liste tous les manifests connus en base."
  (with-db "./datas/wasmcloud.db"
    (let ((rows (sqlite:execute-to-list
                 *manifest-db*
                 "SELECT name FROM manifest ORDER BY name")))
      (json-response
       (list :status "ok"
             :manifests (mapcar #'first rows))
       :code 200))))

(define-easy-handler
    (delete-manifest-api
     :uri "/api/manifest"
     :default-request-type :DELETE) ()
  "Supprime un manifest (et ses components/traits) à partir de son nom.
Body JSON: { \"name\": \"example-app\" }"
  (let* ((body (parse-json-body))
         (name (cdr (assoc :name body))))
    (with-db "./datas/wasmcloud.db"
      ;; Vérifier que le manifest existe
      (let ((exists (sqlite:execute-single
                     *manifest-db*
                     "SELECT 1 FROM manifest WHERE name = ?"
                     name)))
        (unless exists
          (return-from delete-manifest-api
            (json-response
             (list :status "error"
                   :message (format nil "Manifest ~A not found" name))
             :code 404)))
        ;; Supprimer en DB
        (delete-manifest-db name)
        ;; Réponse OK
        (json-response
         (list :status "ok"
               :deleted name)
         :code 200)))))
