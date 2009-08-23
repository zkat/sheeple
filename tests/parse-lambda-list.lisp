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
(test parse-specialized-lambda-list)
