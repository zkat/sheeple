;;;; -*- Mode: lisp; indent-tabs-mode: nil -*-
;;;;
;;;; This file is part of Sheeple.

;;;; tests/bootstrap.lisp
;;;;
;;;; Unit tests for src/bootstrap.lisp
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :sheeple)

(test shared-init-sheep)

(test init-sheep
  (let ((parent (make-sheep () :properties '((prop1 NIL)))))
    (defreply init-sheep :before ((sheep parent) &key)
      (is (has-property-p sheep 'prop1))
      (is (eq NIL (property-value sheep 'prop1)))
      (is (not (has-property-p sheep 'prop2))))
    (defreply init-sheep :after ((sheep parent) &key)
      (is (has-property-p sheep 'prop1))
      (is (eq 'val1 (property-value sheep 'prop1)))
      (is (has-property-p sheep 'prop2))
      (is (eq 'val2 (property-value sheep 'prop2))))
    (defreply init-sheep :around ((sheep parent) &key)
      (is (eq NIL (property-value sheep 'prop1)))
      (prog1 (call-next-reply)
        (is (eq 'val1 (property-value sheep 'prop1)))
        (is (eq 'val2 (property-value sheep 'prop2)))))
    (let ((test-sheep (make-sheep (list parent) :properties '((prop1 val1) (prop2 val2)))))
      (is (eq 'val1 (property-value test-sheep 'prop1)))
      (is (eq 'val2 (property-value test-sheep 'prop2))))))

(test reinit-sheep
  (let ((test-sheep (spawn))
        (another (spawn)))
    (is (eql test-sheep (add-property test-sheep 'var "value" :accessor t)))
    (is (has-direct-property-p test-sheep 'var))
    (is (eql test-sheep (add-parent another test-sheep)))
    (is (parentp another test-sheep))
    (is (eql test-sheep (reinit-sheep test-sheep)))
    (is (parentp =standard-sheep= test-sheep))
    (is (not (has-direct-property-p test-sheep 'var)))
    (is (not (parentp another test-sheep)))
    (is (eql test-sheep (reinit-sheep test-sheep :new-parents (list another))))
    (is (parentp another test-sheep))))
