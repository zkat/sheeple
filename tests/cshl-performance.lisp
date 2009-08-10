;;;; -*- Mode: lisp; indent-tabs-mode: nil -*-
;;;
;; This file is part of Sheeple.

;; tests/sheeple.lisp
;;
;; Unit tests for src/sheeple.lisp
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :sheeple)

(defun has-cycle-p (constraints)
  (loop :while constraints
     :for prev := constraints
     :do (setf constraints
              (remove-if (lambda (cdr) (not (find cdr constraints :key #'car)))
                         constraints :key #'cadr))
     :when (equal prev constraints) :return constraints))

(defun gen-constraints (&key (num-elements 20) (max-constraints num-elements)
                        (max-tries (* 1.5 max-constraints)))
  (lambda ()
    (loop :repeat max-tries :finally (return constraints)
       :for constraint = (list (random num-elements) (random num-elements))
       :unless (has-cycle-p (cons constraint constraints))
       :collect constraint :into constraints
       :until (= (length constraints) max-constraints))))
