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

(test %add-property-cons
  (let ((object (spawn))
        (property
         #+sheeple3.1 (spawn =standard-property=)
         #-sheeple3.1 'test))
    (is (null (%object-properties object)))
    (is (eq object (%add-property-cons object property nil)))
    (signals error (%add-property-cons object property nil))
    (is (not (null (%object-properties object))))
    (is (vectorp (%object-properties object)))
    (is (find property (%object-properties object)
              :key #+sheeple3.1(fun (property-name (car _)))
              #-sheeple3.1 'car))))

(test %get-property-cons
  (let* ((object (spawn))
         (property
          #+sheeple3.1 (defobject (=standard-property=) ((property-name 'test)))
          #-sheeple3.1 'test)
         (other-property
          #+sheeple3.1 (defobject (=standard-property=) ((property-name 'other-prop)))
          #-sheeple3.1 'other-prop))
    (is (null (%get-property-cons object property)))
    (%add-property-cons object property 'value)
    (is (consp (%get-property-cons object property)))
    (is (null (%get-property-cons object other-property)))
    (is (eq property (car (%get-property-cons object property))))
    (is (eq 'value (cdr (%get-property-cons object property))))))

(test %remove-property-cons
  (let* ((object (spawn))
         (property
          #+sheeple3.1 (defobject (=standard-property=) ((property-name 'test)))
          #-sheeple3.1 'test))
    (%add-property-cons object property 'value)
    (is (eq object (%remove-property-cons object 'test)))
    (is (null (%get-property-cons object 'tests)))))

(test %direct-property-value
  (let* ((object (spawn))
         (property
          #+sheeple3.1 (defobject (=standard-property=) ((property-name 'test)))
          #-sheeple3.1 'test))
    (%add-property-cons object property 'value)
    (is (eq 'value (%direct-property-value object 'test)))))

#+sheeple3.1
(test %direct-property-metaobject
  (let* ((object (spawn))
         (property
          #+sheeple3.1 (defobject (=standard-property=) ((property-name 'test)))
          #-sheeple3.1 'test))
    (%add-property-cons object property 'value)
    (is (eq 'new-value (setf (%direct-property-value object 'test) 'new-value)))
    (is (eq 'new-value (%direct-property-value object 'test)))))

(def-suite existential :in properties)
(in-suite existential)

(test has-direct-property-p
  (let* ((object (spawn))
         (property
          #+sheeple3.1 (defobject (=standard-property=) ((property-name 'test)))
          #-sheeple3.1 'test))
    (%add-property-cons object property 'value)
    (is (has-direct-property-p object 'test))
    (is (not (has-direct-property-p object 'something-else))))
  (let* ((a (spawn))
         (b (spawn a)))
    (add-property a 'test 'value)
    (is (has-direct-property-p a 'test))
    (is (not (has-direct-property-p b 'test)))))

(test has-property-p
  (let* ((a (spawn))
         (b (spawn a)))
    (add-property a 'test 'value)
    (is (has-direct-property-p a 'test))
    (is (not (has-direct-property-p a 'something-else)))
    (is (not (has-direct-property-p b 'test)))
    (is (not (has-direct-property-p b 'something-else)))))

(test add-property
  (let ((object (spawn)))
    (is (eq object (add-property object 'test 'value)))
    (is (has-direct-property-p object 'test))
    (is (eq 'value (%direct-property-value object 'test)))
    (signals error (add-property object "foo" "uh oh"))
    (is (not (has-direct-property-p object "foo")))
    ;; todo - check that the restart works properly.
    ))

(test remove-property
  (let ((object (spawn)))
    (signals error (remove-property object 'something))
    (add-property object 'test 'value)
    (is (eq object (remove-property object 'test)))
    (is (not (has-direct-property-p object 'test)))
    (signals error (remove-property object 'test))))

(test remove-all-direct-properties
  (let ((object (spawn)))
    (add-property object 'test1 'value)
    (add-property object 'test2 'value)
    (add-property object 'test3 'value)
    (is (eq object (remove-all-direct-properties object)))
    (is (not (or (has-direct-property-p object 'test1)
                 (has-direct-property-p object 'test2)
                 (has-direct-property-p object 'test3))))))

(def-suite values :in properties)
(in-suite values)

(test direct-property-value
  (let* ((a (spawn))
         (b (spawn a)))
    (add-property a 'test 'value)
    (is (eq 'value (direct-property-value a 'test)))
    (signals unbound-property (direct-property-value a 'something-else))
    (signals unbound-property (direct-property-value b 'test))))

(test property-value
  (let* ((a (spawn))
         (b (spawn a))
         (c (spawn)))
    (add-property a 'test 'value)
    (is (eq 'value (property-value a 'test)))
    (is (eq 'value (property-value b 'test)))
    (signals unbound-property (property-value a 'something-else))
    (signals unbound-property (property-value c 'test))))

(test property-value-with-hierarchy-list
  (let* ((a (spawn))
         (b (spawn a))
         (c (spawn)))
    (add-property a 'test 'value)
    (is (eq 'value (property-value-with-hierarchy-list a 'test)))
    (is (eq 'value (property-value-with-hierarchy-list b 'test)))
    (signals unbound-property (property-value-with-hierarchy-list a 'something-else))
    (signals unbound-property (property-value-with-hierarchy-list c 'test))))

(test setf-property-value
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

(test property-owner
  (let* ((parent (defobject () ((var "value"))))
         (child (defobject (parent) ((child-var "child-value")))))
    (is (eq parent (property-owner parent 'var)))
    (is (eq parent (property-owner child 'var)))
    (is (eq child (property-owner child 'child-var)))
    (is (null (property-owner parent 'some-other-property nil)))
    (is (null (property-owner parent 'some-other-property)))
    (signals unbound-property (property-owner parent 'some-other-property t))))

#+sheeple3.1
(test property-metaobject-p
  (is (property-metaobject-p (spawn =standard-property=)))
  (is (not (property-metaobject-p (spawn)))))

#+sheeple3.1
(test direct-property-metaobject
  (let ((object (defobject () ((var 'value)))))
    (is (property-metaobject-p (direct-property-metaobject object 'var)))
    (signals unbound-property (direct-property-metaobject object 'durr))
    (is (null (direct-property-metaobject object 'durr nil)))
    (signals unbound-property (direct-property-metaobject object 'durr t))))

(test object-direct-properties
  (let ((object (defobject () ((var1 'val) (var2 'val) (var3 'val)))))
    (is (= 3 (length (object-direct-properties object))))
    (is (every #+sheeple3.1 #'property-metaobject-p
               #-sheeple3.1 #'symbolp
               (object-direct-properties object)))))

(test available-properties
  (let* ((a (defobject () ((var1 'val))))
         (b (defobject (a) ((var2 'val))))
         (c (defobject (b) ((var3 'val)))))
    (is (= 4 (length (available-properties c))))
    (is (every #+sheeple3.1 #'property-metaobject-p
               #-sheeple3.1 #'symbolp (available-properties c)))))

;; ugh. I don't want to write tests for these.
(test property-summary)
(test direct-property-summary)
