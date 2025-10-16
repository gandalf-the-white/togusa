(uiop:define-package togusa
  (:use #:cl #:wasmcloud))

(in-package #:togusa)

(defun main ()
  (let* ((cluster (make-cluster :name "prod-eu" :nats-url "nats://nats.prod:4222"
                                :nodes (list (make-node :id "host-eu-1" :zone "eu-west-1a" :labels '("x86_64"))
                                             (make-node :id "host-eu-2" :zone "eu-west-1b"))
                                :leaves (list (make-leaf :id "edge-paris" :zone "eu-west-1-edge"))))
         (http (make-provider :name "httpserver" :capability "wasi:http/proxy" :image "ghcr.io/wasmcloud/http-server:0.21.0" :zone "eu-west-1a"))
         (kv   (make-provider :name "kv" :capability "wasi:keyvalue/store" :image "ghcr.io/wasmcloud/keyvalue-redis:0.11.0" :zone "eu-west-1b"))
         (spread (list
                  (list :name "eastcoast" :weight 80 :requirements (list :zone "us-east-1"))
                  (list :name "westcoast" :weight 20 :requirements (list :zone "us-west-1"))))
         ;; (comp (make-component :name "hello"
         ;;                       :image "ghcr.io/wasmcloud/components/http-hello-world-rust:0.1.0"
         ;;                       :replicas 3 :zone "eu-west-1a"))
         (comp (make-component
                :name "http-component"
                :image "file://./build/http_hello_world_s.wasm"
                :config (list :instances 100 :spread spread)))
         (lnk1 (make-link :name "http->hello"
                          :source "httpserver" :source-interface "wasi:http/outgoing-handler"
                          :target "hello" :target-interface "wasi:http/incoming-handler"))
         (lnk2 (make-link :name "hello->kv"
                          :source "hello" :source-interface "wasi:keyvalue/store"
                          :target "kv" :target-interface "wasi:keyvalue/store"))
         (m (make-manifest :name "demo-app" :version "1.0.0" :description "Demo wasmCloud EU"
                           :cluster cluster)))
    (manifest-add-provider m http)
    (manifest-add-provider m kv)
    (manifest-add-component m comp)
    (manifest-add-link m lnk1)
    (manifest-add-link m lnk2)
    (save-manifest-json m #P"./demo-app.json")
    ))

(defun reload ()
  (let ((m2 (load-manifest-json #P"./demo-app.json")))
    ;; Exemple : on change la distribution du spreadscaler
    (setf (component-config (first (manifest-components m2)))
          (list :instances 100
                :spread (list
                         (list :name "eastcoast" :weight 60 :requirements (list :zone "us-east-1"))
                         (list :name "westcoast" :weight 40 :requirements (list :zone "us-west-1")))))
    ;; Resauvegarde avec nouveau nom
    (save-manifest-json m2 #P"./spread-app-redeploy.json")))

(defun create ()
  (let* ((cluster (make-cluster :name "prod-eu" :nats-url "nats://nats.prod:4222"
                                :nodes (list (make-node :id "host-eu-1" :zone "eu-west-1a" :labels '("x86_64"))
                                             (make-node :id "host-eu-2" :zone "eu-west-1b"))
                                :leaves (list (make-leaf :id "edge-paris" :zone "eu-west-1-edge"))))
         (http (make-provider :name "httpserver" :capability "wasi:http/proxy" :image "ghcr.io/wasmcloud/http-server:0.21.0" :zone "eu-west-1a"))
         (kv   (make-provider :name "kv" :capability "wasi:keyvalue/store" :image "ghcr.io/wasmcloud/keyvalue-redis:0.11.0" :zone "eu-west-1b"))
         (spread (list
                  (list :name "eastcoast" :weight 80 :requirements (list :zone "us-east-1"))
                  (list :name "westcoast" :weight 20 :requirements (list :zone "us-west-1"))))
         ;; (comp (make-component :name "hello"
         ;;                       :image "ghcr.io/wasmcloud/components/http-hello-world-rust:0.1.0"
         ;;                       :replicas 3 :zone "eu-west-1a"))
         (comp (make-component
                :name "http-component"
                :image "file://./build/http_hello_world_s.wasm"
                :config (list :instances 100 :spread spread)))
         (lnk1 (make-link :name "http->hello"
                          :source "httpserver" :source-interface "wasi:http/outgoing-handler"
                          :target "hello" :target-interface "wasi:http/incoming-handler"))
         (lnk2 (make-link :name "hello->kv"
                          :source "hello" :source-interface "wasi:keyvalue/store"
                          :target "kv" :target-interface "wasi:keyvalue/store"))
         (m (make-manifest :name "demo-app" :version "1.0.0" :description "Demo wasmCloud EU"
                           :cluster cluster)))
    (manifest-add-provider m http)
    (manifest-add-provider m kv)
    (manifest-add-component m comp)
    (manifest-add-link m lnk1)
    (manifest-add-link m lnk2)
    m))

(defun maindb ()
  (let ((m (create)))
    (db:init-db)
    (format t "~a~%" (list (manifest-name m) (manifest-version m) (manifest-description m)))
    (wasmcloud:save-manifest-to-db m)
    (defparameter *m2* (wasmcloud:load-manifest-from-db "demo-app"))
    (setf (wasmcloud:component-config (first (wasmcloud:manifest-components *m2*)))
          (list :instances 100
                :spread (list
                         (list :name "eastcoast" :weight 60 :requirements (list :zone "us-east-1"))
                         (list :name "westcoast" :weight 40 :requirements (list :zone "us-west-1")))))
    (wasmcloud:save-manifest-to-db *m2*)
    (save-manifest-json *m2* #P"./spread-app-redeploy.json")))
