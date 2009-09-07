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

(test proper-list-of-length-p
  (for-all ((min (gen-integer :min 0 :max 1023))
            (max (gen-integer :min 0 :max 1023) (>= max min))
            (list (gen-list :length (gen-integer :min 0 :max 1023))
                  (<= min (length list) max)))
    (is (not (null (proper-list-of-length-p list min max)))))
  (for-all ((min (gen-integer :min 0 :max 1023))
            (max (gen-integer :min 0 :max 1023) (>= max min))
            (list (gen-list :length (gen-integer :min 0 :max 1023))
                  (or (< (length list) min) (> (length list) max))))
    (is (null (proper-list-of-length-p list min max))))
  (signals type-error (proper-list-of-length-p 5 0))
  (is (null (proper-list-of-length-p #1='(:P . #1#) 0 2))))

(test fun
  (is (functionp (fun (* 2 _))))
  (is (= 5 (funcall (fun (+ 3 _)) 2))))

(test collect-normal-expander)
(test collect-list-expander)
(test collect)
(test once-only)
(test memq)
(test maybe-weak-pointer-value)
