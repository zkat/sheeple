;; This file is part of Sheeple.

;; tests/properties.lisp
;;
;; Unit tests for src/properties.lisp
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :sheeple)

;;;
;;; Properties
;;;
(def-suite properties :in sheeple)
(in-suite properties)

(test add-property
  (let ((sheep (clone)))
    (is (eql sheep (add-property sheep 'var "value")))
    (is (has-direct-property-p sheep 'var))
    (is (equal "value" (direct-property-value sheep 'var)))
    (is (participant-p sheep 'var))
    (is (participant-p sheep '(setf var)))
    (is (equal "value" (var sheep)))
    (setf (var sheep) "new-value")
    (is (equal "new-value" (var sheep)))
    (is (eql sheep (add-property sheep 'new-var "new-value" :make-accessors-p nil)))
    (is (equal "new-value" (direct-property-value sheep 'new-var)))
    (is (not (participant-p sheep 'new-var)))
    (is (not (participant-p sheep '(setf new-var))))
    ;; TODO - :readers and :writers options for add-property should be tested.
    ))

(test remove-property
  (let ((sheep (defclone () ((var "value")))))
    (is (has-direct-property-p sheep 'var))
    (is (eql sheep (remove-property sheep 'var)))
    (is (not (has-direct-property-p sheep 'var)))
    (signals error (remove-property sheep 'something-else))))

(test remove-all-direct-properties
  (let ((sheep (defclone () ((var "value") (another-prop "another-value")))))
    (is (has-direct-property-p sheep 'var))
    (is (has-direct-property-p sheep 'another-prop))
    (is (eql sheep (remove-all-direct-properties sheep)))
    (is (eql nil (sheep-direct-properties sheep)))))

(test direct-property-value
  (let* ((parent (defclone () ((parent-var "foo"))))
         (child (defclone (parent) ((child-var "bar")))))
    (is (equal "foo" (direct-property-value parent 'parent-var)))
    (is (equal "bar" (direct-property-value child 'child-var)))
    (signals unbound-property (direct-property-value child 'something-else))))

(test property-value ;; this should have (setf property-value) in it, too.
  (let* ((parent (defclone () ((parent-var "foo"))))
         (child (defclone (parent) ((child-var "bar")))))
    (is (equal "foo" (property-value parent 'parent-var)))
    (is (equal "bar" (property-value child 'child-var)))
    (is (equal "foo" (property-value child 'parent-var)))
    (signals unbound-property (property-value parent 'something-else))))

(test has-direct-property-p
  (let ((sheep (defclone () ((var "value")))))
    (is (has-direct-property-p sheep 'var))
    (is (not (has-direct-property-p sheep 'anything-else)))))

(test has-property-p
  (let* ((parent (defclone () ((parent-var "foo"))))
         (child (defclone (parent) ((child-var "bar")))))
    (is (has-property-p parent 'parent-var))
    (is (has-property-p child 'child-var))
    (is (has-property-p child 'parent-var))
    (is (not (has-property-p parent 'something-else)))))

(def-suite property-reflection :in properties)
(in-suite property-reflection)

(test property-owner
  (let* ((parent (defclone () ((var "value"))))
         (child (defclone (parent) ((child-var "child-value")))))
    (is (eql parent (property-owner parent 'var)))
    (is (eql parent (property-owner child 'var)))
    (is (eql child (property-owner child 'child-var)))
    ;; todo - make sure to check that error is signaled (or not signaled) as appropriate
    (is (not (property-owner parent 'some-other-property nil)))
    (signals error (property-owner parent 'some-other-property))))

(test available-properties
  (let ((sheep (defclone () ((var "value" :accessor var)))))
    (is (eql 1 (length (available-properties sheep))))
    (is (eql (find-class 'property-spec)
             (class-of (car (available-properties sheep)))))
    (is (eql sheep (add-property sheep 'new-var "new-value")))
    (is (eql 2 (length (available-properties sheep))))))

(test direct-property-spec
  (let ((sheep (defclone () ((var "value" :accessor var)))))
    (is (eql (find-class 'property-spec)
             (class-of (direct-property-spec sheep 'var))))
    (signals unbound-property (direct-property-spec sheep 'some-other-property))))

(test sheep-direct-properties
  (let ((sheep (defclone () ((var "value" :accessor var) (another-var "another-value")))))
    (is (eql 2 (length (sheep-direct-properties sheep))))
    (is (eql (find-class 'property-spec)
             (class-of (car (sheep-direct-properties sheep)))))
    (is (eql (find-class 'property-spec)
             (class-of (cadr (sheep-direct-properties sheep)))))))

;; ugh. I don't want to write tests for these right now. I probably need cl-ppcre :\
(test property-summary)
(test direct-property-summary)
