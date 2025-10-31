(defpackage :wasmcloud
  (:use :cl))

(in-package :wasmcloud)

;; ================================================
;; C O M M O N  
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


;; ================================================
;;  C O M P O N E N T S  P R O V I D E R S
;; ================================================

;; ================================================
;;  M A N I F E S T
;; ================================================


;; ================================================
;;  S P R E A D   A N D   L I N K
;; ================================================



;; ================================================
;;  T O O L S
;; ================================================


;; ================================================
;;  E X A M P L E
;; ================================================



