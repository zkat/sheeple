;; This file is part of Sheeple.

;; tests/sheeple.lisp
;;
;; Unit tests for src/sheeple.lisp
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :sheeplette-tests)

(export 'sheeplette-tests)

(def-suite sheeplette)
(defun sheeplette-tests ()
  (run! 'sheeplette))

(def-suite sheep-cloning-tests :in sheeplette)
(def-suite sheep-properties-tests :in sheep-cloning-tests)
(def-suite clone-options :in sheep-cloning-tests)

(in-suite sheep-cloning-tests)
(test clone-basic
  "Basic cloning tests. Confirm the CLONE macro works correctly, and that cyclic hierarchy lists
properly signal SHEEP-HIERARCHY-ERROR."
  (is (eql =dolly= (car (sheep-direct-parents (clone () ())))))
  (is (= 1 (length (available-properties (clone () ((foo "bar")))))))
  (let ((obj1 (clone () ())))
    (is (eql obj1
	     (car (sheep-direct-parents (clone (obj1) ()))))))
  (let ((obj1 (clone () ()))
	(obj2 (clone () ())))
    (add-parent obj1 obj2)
    (is (eql obj1
	     (car (sheep-direct-parents obj2)))))
  (let ((obj (clone () ((foo "bar")))))
    (is (equal "bar" (property-value obj 'foo))))
  (let ((obj (clone () ((foo "bar") (baz "quux")))))
    (is (equal "quux" (property-value obj 'baz))))
  (let* ((obj1 (clone () ()))
	 (obj2 (clone () ())))
    (is (= 1 (- (sid obj2) (sid obj1)))))
  (signals sheep-hierarchy-error (let ((obj1 (clone () ()))
				       (obj2 (clone () ())))
				   (add-parent obj1 obj2)
				   (clone (obj1 obj2) ())))
  (signals sheep-hierarchy-error (let* ((obj1 (clone () ()))
					(obj2 (clone (obj1) ())))
				   (clone (obj1 obj2) ()))))

(test inheritance-checkers
  "Confirms proper function of direct-parent-p, direct-child-p, ancestor-p, and descendant-p"
  (let* ((grandpa (clone () ()))
	 (father (clone (grandpa) ()))
	 (child (clone (father) ())))
    (is (direct-parent-p father child))
    (is (not (direct-parent-p grandpa child)))
    (is (not (direct-parent-p child child)))
    (is (ancestor-p father child))
    (is (ancestor-p grandpa child))
    (is (not (ancestor-p child child)))
    (is (not (ancestor-p child grandpa)))
    (is (not (ancestor-p child father)))
    (is (descendant-p child father))
    (is (descendant-p child grandpa))
    (is (not (descendant-p father father)))
    (is (not (descendant-p father child)))
    (is (not (descendant-p grandpa child)))))

(in-suite sheep-properties-tests)
(test properties-basic
  "Basic property-setting and property-access tests. Ensures they follow spec."
  (let* ((main-sheep (clone () ()))
	 (child-sheep (clone (main-sheep) ())))
    (is (eql nil (available-properties main-sheep)))
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
    (is (equal '(foo) (available-properties main-sheep)))
    (is (eql main-sheep (who-sets main-sheep 'foo)))
    (is (eql main-sheep (who-sets child-sheep 'foo)))
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

(in-suite clone-options)
;; (test :copy-all-values
;;   "Tests the :copy-all-values clone option. It's supposed to pull in
;; all available property values from the sheep hierarchy and set them locally."
;;   (let* ((test-sheep (clone () ((var "value")) (:nickname "test-sheep")))
;; 	 (another-sheep (clone (test-sheep) ((other-var "other-value")) (:copy-all-values t))))
;;     (setf (property-value test-sheep 'var) "new-value")
;;     (is (equal "new-value" (property-value test-sheep 'var)))
;;     (is (equal "value" (property-value another-sheep 'var)))))

;; (test :copy-direct-values
;;   "Tests the :copy-direct-values clone option. It pulls in only the direct-slots
;; defined in the sheep that are being cloned.")

(test :nickname
  "Tests the :nickname clone option"
  (let* ((test-sheep (clone () ((var "value")) (:nickname "test-sheep")))
	 (another-sheep (clone (test-sheep) () (:copy-all-values t))))
    (is (equal "test-sheep" (sheep-nickname test-sheep)))
    (setf (sheep-nickname another-sheep) "Johnny Bravo")
    (is (equal "Johnny Bravo" (sheep-nickname another-sheep)))))

;; (test general-clone-options
;;   "Runs tests on the options feature of CLONE, and checks that existing options work."
;;   (signals invalid-option-error (clone () () (:anything-else)))
;;   (signals invalid-option-error (clone () () ()))
;;   (signals probably-meant-to-be-option (clone () (:copy-all-values t))))
