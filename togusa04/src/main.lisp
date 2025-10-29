(defpackage :togusa
  (:use :cl :wasmcloud))

(in-package :togusa)

;; blah blah blah.

(defun main()
  (let* ((c (make-cluster :name "clolo"
                          :workers (list (make-worker))
                          :leafs (list (make-leaf))))
         (m (make-manifest :name "mlolo"
                           :cluster c
                           :components (list (make-component :name "Http-component"
                                                             :image "ghcr.io/wasmcloud/components/dog-fetcher-rust:0.1.1"
                                                             :replicas 1
                                                             :traits (list (make-spread :name "eastcost"
                                                                                        :weight 80
                                                                                        :requirement "us-east-1")
                                                                           (make-spread :name "westcost"
                                                                                        :weight 20
                                                                                        :requirement "us-west-1"))))
                           :providers (list (make-provider :name "Httpserver"
                                                           :image "ghcr.io/wasmcloud/http-server:0.27.0")))))
    ;; (format t "cluster: ~a~%" (to-json c))
    ;; (format t "manifest: ~a~%" (to-json m))
    ;; (inspect m)
    
    ;; (json-to-string-manifest m)
    (save-manifest m "./save.json")
    ))
