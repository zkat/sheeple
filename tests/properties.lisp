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

(def-suite internals :in properties)
(in-suite internals)

(postboot-test %add-property-cons
  (let ((sheep (spawn)))
    (is (null (%sheep-direct-properties sheep)))
    (is (eq sheep (%add-property-cons sheep (spawn =standard-property=) nil)))
    (is (not (null (%sheep-direct-properties sheep))))
    (is (vectorp (%sheep-direct-properties sheep)))
    (is (find 'std-property (%sheep-direct-properties sheep)
              :key (fun (property-name (car _)))))))

(postboot-test %get-property-cons
  (let* ((sheep (spawn))
         (property (defsheep (=standard-property=) ((property-name 'test)))))
    (is (null (%get-property-cons sheep 'test)))
    (%add-property-cons sheep property 'value)
    (is (consp (%get-property-cons sheep 'test))))
    (is (null (%get-property-cons sheep 'something-else)))
    (is (eq property (car (%get-property-cons sheep 'test))))
    (is (eq 'value (cdr (%get-property-cons sheep 'test)))))

(postboot-test %remove-property-cons
  (let* ((sheep (spawn))
         (property (defsheep (=standard-property=) ((property-name 'test)))))
    (%add-property-cons sheep property 'value)
    (is (eq sheep (%remove-property-cons sheep 'test)))
    (is (null (%get-property-cons sheep 'tests)))))

(postboot-test %direct-property-value
  (let* ((sheep (spawn))
         (property (defsheep (=standard-property=) ((property-name 'test)))))
    (%add-property-cons sheep property 'value)
    (is (eq 'value (%direct-property-value sheep 'test)))))

(postboot-test %direct-property-metaobject
  (let* ((sheep (spawn))
         (property (defsheep (=standard-property=) ((property-name 'test)))))
    (%add-property-cons sheep property 'value)
    (is (eq 'new-value (setf (%direct-property-value sheep 'test) 'new-value)))
    (is (eq 'new-value (%direct-property-value sheep 'test)))))

(def-suite existential :in properties)
(in-suite existential)

(postboot-test has-direct-property-p
  (let* ((sheep (spawn))
         (property (defsheep (=standard-property=) ((property-name 'test)))))
    (%add-property-cons sheep property 'value)
    (is (has-direct-property-p sheep 'test))
    (is (not (has-direct-property-p sheep 'something-else))))
  (let* ((a (spawn))
         (b (spawn a)))
    (add-property a 'test 'value)
    (is (has-direct-property-p a 'test))
    (is (not (has-direct-property-p b 'test)))))

(postboot-test has-property-p
  (let* ((a (spawn))
         (b (spawn a)))
    (add-property a 'test 'value)
    (is (has-direct-property-p a 'test))
    (is (not (has-direct-property-p a 'something-else)))
    (is (not (has-direct-property-p b 'test)))
    (is (not (has-direct-property-p b 'something-else)))))

(postboot-test add-property
  (let ((sheep (spawn)))
    (is (eq sheep (add-property sheep 'test 'value)))
    (is (has-direct-property-p sheep 'test))
    (is (eq 'value (%direct-property-value sheep 'test)))
    (signals error (add-property sheep "foo" "uh oh"))
    (is (not (has-direct-property-p sheep "foo")))
    ;; todo - check that the restart works properly.
    ))

(postboot-test remove-property
  (let ((sheep (spawn)))
    (signals error (remove-property sheep 'something))
    (add-property sheep 'test 'value)
    (is (eq sheep (remove-property sheep 'test)))
    (is (not (has-direct-property-p sheep 'test)))
    (signals error (remove-property sheep 'test))))

(postboot-test remove-all-direct-properties
  (let ((sheep (spawn)))
    (add-property sheep 'test1 'value)
    (add-property sheep 'test2 'value)
    (add-property sheep 'test3 'value)
    (is (eq sheep (remove-all-direct-properties sheep)))
    (is (not (or (has-direct-property-p sheep 'test1)
                 (has-direct-property-p sheep 'test2)
                 (has-direct-property-p sheep 'test3))))))

(def-suite values :in properties)
(in-suite values)

(postboot-test direct-property-value
  (let* ((a (spawn))
         (b (spawn a)))
    (add-property a 'test 'value)
    (is (eq 'value (direct-property-value a 'test)))
    (signals unbound-property (direct-property-value a 'something-else))
    (signals unbound-property (direct-property-value b 'test))))

(postboot-test property-value
  (let* ((a (spawn))
         (b (spawn a))
         (c (spawn)))
    (add-property a 'test 'value)
    (is (eq 'value (property-value a 'test)))
    (is (eq 'value (property-value b 'test)))
    (signals unbound-property (property-value a 'something-else))
    (signals unbound-property (property-value c 'test))))

(postboot-test property-value-with-hierarchy-list
  (let* ((a (spawn))
         (b (spawn a))
         (c (spawn)))
    (add-property a 'test 'value)
    (is (eq 'value (property-value-with-hierarchy-list a 'test)))
    (is (eq 'value (property-value-with-hierarchy-list b 'test)))
    (signals unbound-property (property-value-with-hierarchy-list a 'something-else))
    (signals unbound-property (property-value-with-hierarchy-list c 'test))))

(postboot-test setf-property-value
  (let* ((a (spawn))
         (b (spawn a)))
    (signals unbound-property (setf (property-value a 'test) 'new-val))
    (add-property a 'test 'value)
    (is (eq 'new-value (setf (property-value a 'test) 'new-value)))
    (is (eq 'new-value (direct-property-value a 'test)))
    (is (eq 'new-value (property-value b 'test)))
    (is (eq 'foo (setf (property-value b 'test) 'foo)))
    (is (eq 'foo (property-value b 'test)))
    (is (eq 'new-value (property-value a 'test)))))

(def-suite reflection :in properties)
(in-suite reflection)

(postboot-test property-owner)
(postboot-test direct-property-metaobject)
(postboot-test sheep-direct-properties)
(postboot-test available-properties)
(postboot-test property-summary)
(postboot-test direct-property-summary)

;; ;; Reflection stuff
;; (test property-owner
;;   (let* ((parent (defsheep () ((var "value"))))
;;          (child (defsheep (parent) ((child-var "child-value")))))
;;     (is (eql parent (property-owner parent 'var)))
;;     (is (eql parent (property-owner child 'var)))
;;     (is (eql child (property-owner child 'child-var)))
;;     ;; todo - make sure to check that error is signaled (or not signaled) as appropriate
;;     (is (not (property-owner parent 'some-other-property nil)))
;;     (signals unbound-property (property-owner parent 'some-other-property))))

;; (test available-properties
;;   (let ((sheep (defsheep () ((var "value" :accessor var)))))
;;     (is (eql 1 (length (available-properties sheep))))
;;     (is (eql (find-class 'standard-property)
;;              (class-of (car (available-properties sheep)))))
;;     (is (eql sheep (add-property sheep 'new-var "new-value")))
;;     (is (eql 2 (length (available-properties sheep))))))

;; (test direct-property-metaobject
;;   ;; TODO - this should check that the standard property-spec's capabilities work properly.
;;   (let ((sheep (defsheep () ((var "value" :accessor var)))))
;;     (is (eql (find-class 'standard-property)
;;              (class-of (direct-property-metaobject sheep 'var))))
;;     (signals unbound-direct-property (direct-property-metaobject sheep 'whoops))))

;; (test sheep-direct-properties
;;   ;; TODO - This one just needs to check that all the direct property spec metaobjects are returned.
;;   (let ((sheep (defsheep () ((var "value" :accessor var) (another-var "another-value")))))
;;     (is (eql 2 (length (sheep-direct-properties sheep))))
;;     (is (eql (find-class 'standard-property)
;;              (class-of (car (sheep-direct-properties sheep)))))
;;     (is (eql (find-class 'standard-property)
;;              (class-of (cadr (sheep-direct-properties sheep)))))))

;; ;; ugh. I don't want to write tests for these right now. I probably need cl-ppcre :\
;; (test property-summary)
;; (test direct-property-summary)
