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
  (declare (optimize speed (safety 0) (debug 0))) ; The irony kills me
  (when (fboundp name) (fdefinition name)))

;;; I thought about Alexandria's COPY-ARRAY, but that's too general.
;;; Use with caution.
(declaim (inline copy-simple-vector))
(defun copy-simple-vector (vector)
  "Creates a new simple-vector with the same elements as VECTOR."
  (declare (simple-vector vector) (optimize speed (safety 0) (debug 0)))
  (make-array (length vector) :initial-contents vector))

(macrolet ((fixnum+ (&rest values) `(the fixnum (+ ,@values))))
  (defun vector-cons (x vector)
    (declare (simple-vector vector)
             (optimize speed (safety 0) (debug 0))) ; --omg-optimized
    (let* ((index (fixnum+ 1 (length vector)))
           (result (make-array index)))
      (declare (fixnum index) (simple-vector result))
      (tagbody (go test)
       loop (setf (svref result index) (svref vector (fixnum+ -1 index)))
       test (setf index (fixnum+ -1 index)) (unless (zerop index) (go loop)))
      (setf (svref result 0) x)
      result)))
