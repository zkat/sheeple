;;;; -*- Mode: lisp; indent-tabs-mode: nil -*-
;;;;
;;;; This file is part of Sheeple.

;;;; tests/parse-lambda-list.lisp
;;;;
;;;; Unit tests for src/parse-lambda-list.lisp
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :sheeple)

(def-suite parse-lambda-list :in sheeple)
(in-suite parse-lambda-list)

(test parse-lambda-list-like-thing)
(test parse-lambda-list)

(test count-required-parameters
  (is (= 0 (count-required-parameters nil)))
  (is (= 3 (count-required-parameters '(a b c))))
  (is (= 0 (count-required-parameters '(&optional d))))
  (is (= 0 (count-required-parameters '(&rest f))))
  (is (= 0 (count-required-parameters '(&key g))))
  (is (= 0 (count-required-parameters '(&aux h))))
  (is (= 2 (count-required-parameters '(i j &key k)))))

(test parse-specialized-lambda-list)
(test analyze-lambda-list)
