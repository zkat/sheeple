;;;; -*- Mode: lisp; indent-tabs-mode: nil -*-
;;;;
;;;; This file is part of Sheeple.

;;;; tests/properties.lisp
;;;;
;;;; Unit tests for src/properties.lisp
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :sheeple)

;;;
;;; Properties
;;;
(def-suite properties :in sheeple)

(def-suite properties-basic :in properties)
(in-suite properties-basic)

(postboot-test add-property
  ;; TODO - errors could be more specific?
  (let ((sheep (clone)))
    (is (eql sheep (add-property sheep 'var "value")))
    (is (has-direct-property-p sheep 'var))
    (is (equal "value" (direct-property-value sheep 'var)))
    (is (participantp sheep 'var))
    (is (participantp sheep '(setf var)))
    (is (equal "value" (var sheep)))
    (is (equal "new-value" (setf (var sheep) "new-value")))
    (is (equal "new-value" (var sheep)))
    (is (eql sheep (add-property sheep 'new-var "new-value" :make-accessor-p nil)))
    (is (equal "new-value" (direct-property-value sheep 'new-var)))
    (is (not (participantp sheep 'new-var)))
    (is (not (participantp sheep '(setf new-var))))
    (is (eql sheep (add-property sheep 'writ "writer!" :writers '(set-writ (setf writ)))))
    (is (string= "foo" (set-writ sheep "foo")))
    (is (string= "foo" (direct-property-value sheep 'writ)))
    (signals error (writ sheep)) ;if :readers or :writers is provider, no accessor is made.
    (is (eql sheep (add-property sheep 'read "I read it" :readers '(read-it))))
    (is (string= "I read it" (read-it sheep)))
    (signals error (setf (read-it sheep) "hurr durr"))))

(postboot-test remove-property
  ;; TODO - more specific errors?
  (let ((sheep (defclone () ((var "value")))))
    (is (has-direct-property-p sheep 'var))
    (is (eql sheep (remove-property sheep 'var)))
    (is (not (has-direct-property-p sheep 'var)))
    (signals error (remove-property sheep 'var))
    (signals error (remove-property sheep 'something-else))))

(postboot-test remove-all-direct-properties
  (let ((sheep (defclone () ((var "value") (another-prop "another-value")))))
    (is (has-direct-property-p sheep 'var))
    (is (has-direct-property-p sheep 'another-prop))
    (is (eql sheep (remove-all-direct-properties sheep)))
    (is (eql nil (sheep-direct-properties sheep)))
    (signals unbound-property (direct-property-value sheep 'var))
    (signals unbound-property (direct-property-value sheep 'another-prop))))

(postboot-test direct-property-value
  (let* ((parent (defclone () ((parent-var "foo"))))
         (child (defclone (parent) ((child-var "bar")))))
    (is (equal "foo" (direct-property-value parent 'parent-var)))
    (is (equal "bar" (direct-property-value child 'child-var)))
    (signals unbound-direct-property (direct-property-value child 'something-else))
    ;; setf
    (is (equal "x" (setf (direct-property-value child 'child-var) "x")))
    (is (equal "x" (direct-property-value child 'child-var)))
    (signals unbound-property (setf (direct-property-value child 'something-else) "y"))))

(postboot-test property-value
  (let* ((parent (defclone () ((parent-var "foo"))))
         (child (defclone (parent) ((child-var "bar")))))
    (is (equal "foo" (property-value parent 'parent-var)))
    (is (equal "bar" (property-value child 'child-var)))
    (is (equal "foo" (property-value child 'parent-var)))
    (signals unbound-property (property-value parent 'something-else))
    ;; Test for re-allocation of parent properties in the child sheep
    (setf (property-value child 'parent-var) "zap")
    (is (equal "zap" (property-value child 'parent-var)))
    (is (equal "foo" (property-value parent 'parent-var)))))

(postboot-test has-direct-property-p
  (let* ((sheep (defclone () ((var "value"))))
         (child (clone sheep)))
    (is (has-direct-property-p sheep 'var))
    (is (not (has-direct-property-p sheep 'anything-else)))
    (is (not (has-direct-property-p child 'var)))))

(postboot-test has-property-p
  (let* ((parent (defclone () ((parent-var "foo"))))
         (child (defclone (parent) ((child-var "bar")))))
    (is (has-property-p parent 'parent-var))
    (is (has-property-p child 'child-var))
    (is (has-property-p child 'parent-var))
    (is (not (has-property-p parent 'something-else)))))

;; (def-suite property-mop :in properties)
;; (in-suite property-mop)

;; ;; MOP behavior
;; (defclass fancy-property-sheep (standard-sheep)
;;   ((fancy-property-table :initform (make-hash-table :test #'eq))))
;; (defclass fancy-property (standard-property) ())

;; #+nil(test add-property
;;   ;; TODO - check that property metaobjects are being created properly
;;   ;; TODO - check that different property metaobject class instances are created
;;   ;;        depending on the class of the sheep.
;;   (let ((fancy-clone (defclone () (())
;;                          ))))
;;   )
;; (test remove-property
;;   ;; TODO - check that property metaobjects are being disposed of properly
;;   )
;; (test remove-all-direct-properties
;;   ;; TODO - simple test to make sure basic behavior overriding works.
;;   )
;; ;; These two are only supposed to be dispatchable on SHEEP for now.
;; (test has-direct-property-p)
;; (test has-property-p)

;; ;; Check that property-spec and standard-sheep can both be subclassed, and behavior
;; ;; extended for both of these.
;; (test direct-property-value)
;; (test property-value) ; includes (setf property-value)

;; ;; Reflection stuff
;; (test property-owner
;;   (let* ((parent (defclone () ((var "value"))))
;;          (child (defclone (parent) ((child-var "child-value")))))
;;     (is (eql parent (property-owner parent 'var)))
;;     (is (eql parent (property-owner child 'var)))
;;     (is (eql child (property-owner child 'child-var)))
;;     ;; todo - make sure to check that error is signaled (or not signaled) as appropriate
;;     (is (not (property-owner parent 'some-other-property nil)))
;;     (signals unbound-property (property-owner parent 'some-other-property))))

;; (test available-properties
;;   (let ((sheep (defclone () ((var "value" :accessor var)))))
;;     (is (eql 1 (length (available-properties sheep))))
;;     (is (eql (find-class 'standard-property)
;;              (class-of (car (available-properties sheep)))))
;;     (is (eql sheep (add-property sheep 'new-var "new-value")))
;;     (is (eql 2 (length (available-properties sheep))))))

;; (test direct-property-metaobject
;;   ;; TODO - this should check that the standard property-spec's capabilities work properly.
;;   (let ((sheep (defclone () ((var "value" :accessor var)))))
;;     (is (eql (find-class 'standard-property)
;;              (class-of (direct-property-metaobject sheep 'var))))
;;     (signals unbound-direct-property (direct-property-metaobject sheep 'whoops))))

;; (test sheep-direct-properties
;;   ;; TODO - This one just needs to check that all the direct property spec metaobjects are returned.
;;   (let ((sheep (defclone () ((var "value" :accessor var) (another-var "another-value")))))
;;     (is (eql 2 (length (sheep-direct-properties sheep))))
;;     (is (eql (find-class 'standard-property)
;;              (class-of (car (sheep-direct-properties sheep)))))
;;     (is (eql (find-class 'standard-property)
;;              (class-of (cadr (sheep-direct-properties sheep)))))))

;; ;; ugh. I don't want to write tests for these right now. I probably need cl-ppcre :\
;; (test property-summary)
;; (test direct-property-summary)
