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

(test make-vector
  (for-all ((size (gen-integer :min 0 :max 1023))
            (value  (gen-integer)))
    (is (equalp (apply #'vector (make-list size :initial-element value))
                (make-vector size :initial-element value)))))

(test fun
  (is (functionp (fun (* 2 _))))
  (is (= 5 (funcall (fun (+ 3 _)) 2))))

(test collect-normal-expander)
(test collect-list-expander)
(test collect)
(test memq)
(test maybe-weak-pointer-value)
