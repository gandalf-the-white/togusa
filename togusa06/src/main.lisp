(in-package :togusa)

(defun create-manifest ()
  "Exemple de construction de manifest complet."
  (let* ((worker1 (make-worker :id "worker-1" :zone "zone-a" :label (list "cpu")))
         (worker2 (make-worker :id "worker-2" :zone "zone-b" :label (list "mem")))
         (leaf1 (make-leaf :id "leaf-1" :zone "zone-a" :label (list "cpu" "mem")))
         (cluster (make-cluster :name "wasmcloud"
                                :nats-url "nats://localhost:4222"
                                :workers (list worker1 worker2)
                                :leafs (list leaf1)))
         (spreads (list (make-spread :name "zone-a" :weight 60 :zone "zone-a")
                        (make-spread :name "zone-b" :weight 40 :zone "zone-b")))
         (spreadscaler (make-spreadscaler :instances 4 :spread spreads))
         (cfg (make-config :name "default-http"
                           :address "0.0.0.0:8080"))
         (link (make-link :target "http-component"
                          :pack "http"
                          :namespace "wasi"
                          :interfaces (list "incoming-handler")
                          :source cfg))
         (component (make-component :name "http-component"
                                    :image "ghcr.io/wasmcloud/components/dog-fetcher-rust:0.1.1"
                                    :replicas 4
                                    :traits (list spreadscaler link)))
         (capability (make-capability :name "httpserver"
                                      :image "ghcr.io/wasmcloud/http-server:0.27.0"
                                      :traits nil))
         (manifest (make-manifest :name "example-app"
                                  :cluster cluster
                                  :components (list component capability))))
    (with-db "./datas/wasmcloud.db"
      (init-db)
      (save-manifest manifest "./datas/save.json")
      (save-full-manifest manifest))
    manifest))
