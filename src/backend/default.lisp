;;;; -*- Mode: lisp; indent-tabs-mode: nil -*-
;;;;
;;;; This file is part of Sheeple

;;;; backend/default.lisp
;;;;
;;;; Portable versions of backend-customizable bits
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :sheeple)

(declaim (inline safe-fdefinition))
(defun safe-fdefinition (name)
  (when (fboundp name) (fdefinition name)))

(declaim (inline copy-simple-vector))
(defun copy-simple-vector (vector)
  (declare (simple-vector vector) (optimize speed))
  (make-array (length vector) :initial-contents vector))

(defun vector-cons (x vector)
  (declare (simple-vector vector) (optimize speed))
  (aprog1 (make-array (1+ (length vector)))
    (loop for elt across vector and i from 1
       do (setf (svref it i) elt)
       finally (setf (svref it 0) x))))
