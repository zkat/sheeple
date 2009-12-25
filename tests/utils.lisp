;;;; -*- Mode: lisp; indent-tabs-mode: nil -*-
;;;;
;;;; This file is part of Sheeple

;;;; utils.lisp
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :sheeple)

(def-suite utils :in sheeple)
(in-suite sheeple)

(test ensure-list
  (is (null (ensure-list nil)))
  (is (equal '(1) (ensure-list 1)))
  (is (equal '(1) (ensure-list '(1)))))

(test fun
  (is (functionp (fun (* 2 _))))
  (is (= 5 (funcall (fun (+ 3 _)) 2))))

(test collect)
(test memq)
(test maybe-weak-pointer-value)
