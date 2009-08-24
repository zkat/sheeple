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
  "Might fail if your Lisp does no vector optimization"
  (for-all ((size (gen-integer :min 0 :max 1023))
            (value  (gen-integer)))
    (is (typep  (make-vector size) 'simple-vector))
    (is (equalp (apply #'vector (make-list size :initial-element value))
                (make-vector size value)))))

(test flatten
  (for-all ((tree (gen-tree :elements (constantly nil))))
    (is (null (flatten tree))))
  (for-all ((list (gen-list)))
    (is (equal list (flatten list))))
  (for-all ((tree (gen-tree)))
    (is (every #'atom (flatten tree))))
  (is (equal '(a b c) (flatten '((a b c)))))
  (is (equal '(a b c) (flatten '((a) (b) (c)))))
  (is (equal '(a b c) (flatten '((a . b) . c)))))

(test proper-list-of-length-p
  (signals type-error (proper-list-of-length-p 5 0))
  (is (not (null (proper-list-of-length-p () 0))))
  (is (null (proper-list-of-length-p '() 1)))
  (is (null (proper-list-of-length-p '(1) 0)))
  (is (not (null (proper-list-of-length-p '(1 2 3) 0 4))))
  (is (null (proper-list-of-length-p '(1 2 3) 1 2)))
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
