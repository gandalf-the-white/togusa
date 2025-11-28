(defpackage :togusa
  (:use :cl :wasmcloud :dbase))

(in-package :togusa)

;; Recurcive
(defun factorial (n)
  (if (< n 2)
      n
      (* n (factorial (- n 1)))))

(defun fac (n)
  (loop for i from 0 to n
        do (format t "~A! = ~A~%" i (factorial i)) ))

;; 1 * 2 * 3 * 4
;; n=5
;; (factorial 5)
;; 5  * (factorial 4)
;;      4 * (factorial 3)
;;          3 * (factorial 2)
;;              2 * (factorial 1)
;;                  1
