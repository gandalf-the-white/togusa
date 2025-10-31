(defpackage :togusa
  (:use :cl :wasmcloud))

(in-package :togusa)

;; blah blah blah.

(defun main()
  (let* ((c (make-cluster :name "clolo"
                          :workers (list (make-worker))
                          :leafs (list (make-leaf))))
         (spreads (list (make-spread :name "eastcost" :weight 80 :requirement "us-eastcost")
                        (make-spread :name "westcost" :weight 20 :requirement "us-westcost")))
         (linkcompo (make-link :target "Httpserver"))
         (linkprov (make-link :target "Http-component"))
         (compo (make-component :name "Http-component"
                                :image "ghcr.io/wasmcloud/components/dog-fetcher-rust:0.1.1"
                                :replicas 1
                                :traits (list (make-trait :cat "spreadscaler" :properties spreads)
                                              linkcompo)))
         (prov (make-capability :name "Httpserver"
                                :image "ghcr.io/wasmcloud/http-server:0.27.0"
                                :traits (list linkprov)))
         (m (make-manifest :name "mlolo"
                           :cluster c
                           :components (list compo)
                           :capabilitys (list prov))))
    ;; (when (wasmcloud::test-spread spreads)
    ;; (add-spreads :name "Http-component" 
    ;; :components (wasmcloud::components m)
    ;; :spreads spreads))
    (save-manifest m "./save.json")
    m))


;; (defun add_spreads (m name spreads)
;;   (mapcar (lambda (s)
;;             (if (string= (wasmcloud::name s) name)
;;                 (setf (wasmcloud::traits s) spreads)))
;;           (wasmcloud::components m)))

;; (defun test-spread (spreads)
;;   (let* ((total-weight (apply #'+ (mapcar #'wasmcloud::weight spreads)))
;;          (valid (= 100 total-weight)))
;;     valid))
