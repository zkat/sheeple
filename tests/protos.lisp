;; This file is part of Sheeple.

;; tests/protos.lisp
;;
;; Unit tests for src/protos.lisp
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :sheeple)

(def-suite protos :in cloning)
(in-suite protos)

(test find-proto ;; (setf find-proto) goes here, too.
  (is (sheep-p (find-proto 't)))
  (let ((find-proto-test (defproto find-proto-test () ())))
    (is (eql find-proto-test (find-proto 'find-proto-test)))))

(test remove-proto
  (let ((test-proto (defproto test-proto () ())))
    (is (sheep-p test-proto))
    (is (sheep-p (find-proto 'test-proto)))
    (is (remove-proto 'test-proto))
    (signals error (find-proto 'test-proto))
    (is (not (remove-proto 'test-proto)))))

(test defproto)
(test spawn-or-reinitialize-sheep)
(test find-proto-transformer)
(test find-proto-reader-macro)