;;;; -*- Mode: lisp; indent-tabs-mode: nil -*-
;;;;
;;;; This file is part of Sheeple.

;;;; tests/builtins.lisp
;;;;
;;;; Unit tests for src/builtins.lisp
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :sheeple)

;;;
;;; Built-in sheeple objects
;;;
(def-suite builtins :in sheeple)

(def-suite autoboxing :in builtins)
(in-suite autoboxing)

(test box-type-of
  "Tests that the box-type-of function returns the right fleeced-wolf for each lisp type"
  ;; TODO - This could be a *lot* more thorough
  (is (eq =null= (box-type-of nil)))
  (is (eq =symbol= (box-type-of 'foo)))
  (is (eq =complex= (box-type-of #C (10 10))))
  (is (eq =integer= (box-type-of 1)))
  (is (eq =float= (box-type-of 1.0)))
  (is (eq =cons= (box-type-of (cons 1 2))))
  (is (eq =character= (box-type-of #\a)))
  (is (eq =hash-table= (box-type-of (make-hash-table))))
  (is (eq =package= (box-type-of (find-package :sheeple))))
  (is (eq =pathname= (box-type-of #P"compatibility.lisp")))
  (is (eq =readtable= (box-type-of *readtable*)))
  (is (eq =stream= (box-type-of *standard-output*)))
  (is (eq =number= (box-type-of 1/2)))
  (is (eq =string= (box-type-of "foo")))
  (is (eq =bit-vector= (box-type-of #*)))
  (is (eq =vector= (box-type-of (vector 1 2 3))))
  (is (eq =array= (box-type-of (make-array '(1 2)))))
  (is (eq =function= (box-type-of (lambda () 1)))))

(test find-boxed-object)
(test box-object)
(test remove-boxed-object)
(test sheepify)
(test sheepify-list)

;; TODO - implement CLOS autoboxing
;; (test clos-boxing)

