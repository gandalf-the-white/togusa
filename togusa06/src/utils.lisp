(in-package :togusa)

;; MODIF: légèrement simplifié et documenté la "magie" de make-json-object.
;; - Pairs = (&key k1 v1 k2 v2 ...)
;; - Les valeurs peuvent être :
;;   * un hash-table -> inséré tel quel
;;   * une liste de hash-tables -> insérée telle quelle
;;   * NIL -> encodé comme liste vide (utile pour les champs optionnels)
(defun make-json-object (&rest pairs)
  "Construire un objet JSON sous forme de hash-table à partir de PAIRS.
PAIRS est une liste de la forme :k1 v1 :k2 v2 ..."
  (let ((ht (make-hash-table :test 'equal)))
    (loop for (k v) on pairs by #'cddr
          when k
            do (setf (gethash (string-downcase (symbol-name k)) ht)
                     (cond
                       ((null v) '())
                       ((hash-table-p v) v)
                       ((and (listp v)
                             (every #'hash-table-p v))
                        v)
                       (t v))))
    ht))
