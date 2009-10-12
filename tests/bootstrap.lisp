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
  (let ((parent (make-object () :properties '((prop1 NIL)))))
    (defreply init-object :before ((object parent) &key)
      (is (has-property-p object 'prop1))
      (is (eq NIL (property-value object 'prop1)))
      (is (not (has-property-p object 'prop2))))
    (defreply init-object :after ((object parent) &key)
      (is (has-property-p object 'prop1))
      (is (eq 'val1 (property-value object 'prop1)))
      (is (has-property-p object 'prop2))
      (is (eq 'val2 (property-value object 'prop2))))
    (defreply init-object :around ((object parent) &key)
      (is (eq NIL (property-value object 'prop1)))
      (prog1 (call-next-reply)
        (is (eq 'val1 (property-value object 'prop1)))
        (is (eq 'val2 (property-value object 'prop2)))))
    (let ((test-object (make-object (list parent) :properties '((prop1 val1) (prop2 val2)))))
      (is (eq 'val1 (property-value test-object 'prop1)))
      (is (eq 'val2 (property-value test-object 'prop2))))))

(test reinit-object
  (let ((test-object (spawn))
        (another (spawn)))
    (is (eql test-object (add-property test-object 'var "value" :accessor t)))
    (is (has-direct-property-p test-object 'var))
    (remove-parent =standard-object= test-object) ; Avoids circular precedence graphs
    (is (eql test-object (add-parent another test-object)))
    (is (parentp another test-object))
    (is (eql test-object (reinit-object test-object)))
    (is (parentp =standard-object= test-object))
    (is (not (has-direct-property-p test-object 'var)))
    (is (not (parentp another test-object)))
    (is (eql test-object (reinit-object test-object :parents (list another))))
    (is (parentp another test-object))))
