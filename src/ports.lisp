;;;; -*- Mode: lisp; indent-tabs-mode: nil -*-
;;;;
;;;; This file is part of Sheeple

;;;; ports.lisp
;;;;
;;;; Bits which can be customized for various platforms
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :sheeple)

(declaim (inline safe-fdefinition))
(defun safe-fdefinition (name)
  (feature-case
      (when (fboundp name)
        (fdefinition name))
    #+ccl (fboundp name)))

(declaim (inline copy-simple-vector))
(defun copy-simple-vector (vector)
  (declare (simple-vector vector) (optimize speed))
  (feature-case
      (make-array (length vector) :initial-contents vector)
    #+ccl (ccl::copy-uvector vector)))

(defun vector-cons (x vector)
  (declare (simple-vector vector) (optimize speed))
  (aprog1 (make-array (1+ (length vector)))
    (loop for elt across vector and i from 1
       do (setf (svref it i) elt)
       finally (setf (svref it 0) x))))

(defun record-message-compilation (name lambda-list env)
  ;; What should be the default way to note a message at compile time?
  (declare (ignore lambda-list env))
  (feature-case
      `(proclaim `(ftype function ,',name))))

(defun record-message-source (name)
  (declare (ignorable name))
  (feature-case nil
    #+ccl (ccl:record-source-file name 'message)))

(defun record-message-arglist (name arglist)
  (declare (ignorable name arglist))
  (feature-case nil
    #+ccl (ccl::record-arglist name arglist)))
