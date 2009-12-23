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
