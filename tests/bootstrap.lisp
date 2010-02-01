;;;; -*- Mode: lisp; indent-tabs-mode: nil -*-
;;;;
;;;; This file is part of Sheeple.

;;;; tests/bootstrap.lisp
;;;;
;;;; Unit tests for src/bootstrap.lisp
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :sheeple)

(test shared-init-object)

(test init-object
  (let ((parent (object :properties '((prop1 NIL)))))
    (defreply init-object :before ((object parent) &key)
      (is (property-owner object 'prop1))
      (is (eq NIL (property-value object 'prop1)))
      (is (not (property-owner object 'prop2))))
    (defreply init-object :after ((object parent) &key)
      (is (property-owner object 'prop1))
      (is (eq 'val1 (property-value object 'prop1)))
      (is (property-owner object 'prop2))
      (is (eq 'val2 (property-value object 'prop2))))
    (defreply init-object :around ((object parent) &key)
      (is (eq NIL (property-value object 'prop1)))
      (prog1 (call-next-reply)
        (is (eq 'val1 (property-value object 'prop1)))
        (is (eq 'val2 (property-value object 'prop2)))))
    (let ((test-object (object :parents (list parent) :properties '((prop1 val1) (prop2 val2)))))
      (is (eq 'val1 (property-value test-object 'prop1)))
      (is (eq 'val2 (property-value test-object 'prop2))))))

(test reinit-object
  ;; TODO
  (let ((test-object (object)))
    (is (string= "value" (setf (property-value test-object 'var) "value")))
    (is (direct-property-p test-object 'var))))
