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

(test add-property)
(test has-direct-property-p)
(test remove-property)
(test remove-all-direct-properties)
(test direct-property-value)
(test property-value ;; this should have (setf property-value) in it, too.
  )
(test has-property-p)


(def-suite property-reflection :in properties)
(in-suite property-reflection)

(test property-owner)
(test available-properties)
(test property-summary)
(test direct-property-summary)
(test sheep-direct-properties)
(test direct-property-spec)
(test properties-basic
  "Basic property-setting and property-access tests. Ensures they follow spec."
  (let* ((main-sheep (clone () () (:nickname "main-sheep")))
	 (child-sheep (clone (main-sheep) () (:nickname "child-sheep"))))
    (is (eql nil (available-properties main-sheep)))
    (is (= 1 (length (available-properties (add-property (clone) 'var "value" :make-accessors-p nil)))))
    (signals unbound-property (property-value main-sheep 'foo))
    (is (equal "bar" 
	       (setf (property-value main-sheep 'foo) "bar")))
    (is (eql t
	     (has-direct-property-p main-sheep 'foo)))
    (is (eql t
	     (has-property-p main-sheep 'foo)))
    (is (eql nil
	     (has-direct-property-p child-sheep 'foo)))
    (is (eql t
	     (has-property-p child-sheep 'foo)))
    (is (equal "bar" (property-value main-sheep 'foo)))
    (is (eq 'foo (property-spec-name (car (available-properties main-sheep)))))
    (is (eql t (remove-property main-sheep 'foo)))
    (signals unbound-property (property-value main-sheep 'foo))
    (signals unbound-property (property-value child-sheep 'foo))
    (is (eql nil (remove-property main-sheep 'foo)))))

(test auto-generated-manipulators
  "Tests to confirm property-option functionality."
  (let ((test-sheep (clone () ((var "value" :manipulator get-var)))))
    (is (equal "value" (get-var test-sheep)))
    (is (equal "new-value" (setf (get-var test-sheep) "new-value")))
    (is (equal "new-value" (get-var test-sheep))))
  (let ((test-sheep (clone () ((var "value" :reader var :writer set-var)))))
    (is (equal "value" (var test-sheep)))
    (signals undefined-function (setf (var test-sheep) "new-value"))
    (is (equal "value" (var test-sheep)))
    (is (equal "new-value" (set-var "new-value" test-sheep)))
    (is (equal "new-value" (var test-sheep)))))

