;;;; -*- Mode: lisp; indent-tabs-mode: nil -*-
;;;;
;;;; This file is part of Sheeple

;;;; backend/ccl.lisp
;;;;
;;;; Clozure CL version of backend-customizable bits
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :sheeple)

(define-compiler-macro safe-fdefinition (name)
  `(fboundp ,name))

(define-compiler-macro copy-simple-vector (vector)
  `(ccl::copy-uvector ,vector))

(defun record-message-source (name)
  (ccl:record-source-file name 'message))

(defun record-message-arglist (name arglist)
  (ccl::record-arglist name arglist))
