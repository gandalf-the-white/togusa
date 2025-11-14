(defpackage :togusa
  (:use :cl :wasmcloud :dbase))

(in-package :togusa)

;; Création et sauvegarde
;; (with-db ("wasmcloud.db")
;;   (init-db)
;;   (let* ((worker (wasmcloud:make-worker :id "w1" :zone "zone-a" :label '("fast" "gpu")))
;;          (leaf (wasmcloud:make-leaf :id "l1" :zone "zone-b" :label '("slow" "cpu")))
;;          (cluster (wasmcloud:make-cluster
;;                    :name "prod"
;;                    :nats-url "nats://prod:4222"
;;                    :leafs (list leaf)
;;                    :workers (list worker))))
;;     (save-node worker)
;;     (save-node leaf)
;;     (save-cluster cluster)))

;; Chargement
;; (with-db ("wasmcloud.db")
;;   (let ((cluster (dbase::load-cluster "prod")))
;;     (format t "Cluster: ~a~%" (cluster-name cluster))
;;     (format t "Workers: ~a~%" (cluster-workers cluster))
;;     (format t "Leafs: ~a~%" (cluster-leafs cluster))))

;; Recurcive
(defun factorial (n)
  (if (< n 2)
      n
      (* n (factorial (- n 1)))))

;; 1 * 2 * 3 * 4
;; n=5
;; (factorial 5)
;; 5  * (factorial 4)
;;      4 * (factorial 3)
;;          3 * (factorial 2)
;;              2 * (factorial 1)
;;                  1
