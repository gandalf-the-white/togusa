(defpackage togusa/tests/main
  (:use :cl
        :togusa
        :rove))
(in-package :togusa/tests/main)

;; NOTE: To run this test file, execute `(asdf:test-system :togusa)' in your Lisp.

(deftest test-target-1
  (testing "should (= 1 1) to be true"
    (ok (= 1 1))))
