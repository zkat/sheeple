;; This file is part of Sheeple.

;; tests/sheeple.lisp
;;
;; Unit tests for src/sheeple.lisp
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :sheeple-tests)

(export 'sheeple-tests)

(def-suite sheeple)
(defun sheeple-tests ()
  (run! 'sheeple))

(def-suite sheep-cloning-tests :in sheeple)
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
    (is (equal "bar" (get-property obj 'foo))))
  (let ((obj (clone () ((foo "bar") (baz "quux")))))
    (is (equal "quux" (get-property obj 'baz))))
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
    (signals unbound-property (get-property main-sheep 'foo))
    (is (equal "bar" 
	       (setf (get-property main-sheep 'foo) "bar")))
    (is (eql t
	     (has-direct-property-p main-sheep 'foo)))
    (is (eql t
	     (has-property-p main-sheep 'foo)))
    (is (eql nil
	     (has-direct-property-p child-sheep 'foo)))
    (is (eql t
	     (has-property-p child-sheep 'foo)))
    (is (equal "bar" (get-property main-sheep 'foo)))
    (is (equal '(foo) (available-properties main-sheep)))
    (is (eql main-sheep (who-sets main-sheep 'foo)))
    (is (eql main-sheep (who-sets child-sheep 'foo)))
    (is (eql t (remove-property main-sheep 'foo)))
    (signals unbound-property (get-property main-sheep 'foo))
    (signals unbound-property (get-property child-sheep 'foo))
    (is (eql nil (remove-property main-sheep 'foo)))))

(test locked-properties
  "Tests proper behavior of the :lock property option"
  (let ((locked-sheep (clone () ((var "value" :manipulator get-var :lock t))))
	(unlocked-sheep (clone () ((other-var "value" :manipulator get-var :lock nil))))
	(unspecced-sheep (clone () ((other-var "value" :manipulator get-var)))))
    (is (equal "value" (get-var locked-sheep)))
    (is (equal "value" (get-var unlocked-sheep)))
    (is (equal "value" (get-var unspecced-sheep)))
    (signals sheeple::locked-property (setf (get-var locked-sheep) "new-value"))
    (is (equal "new-value" (setf (get-var unlocked-sheep) "new-value")))
    (is (equal "new-value" (setf (get-var unspecced-sheep) "new-value")))))


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
(test :copy-all-values
  "Tests the :copy-all-values clone option. It's supposed to pull in
all available property values from the sheep hierarchy and set them locally."
  (let* ((test-sheep (clone () ((var "value")) (:nickname "test-sheep")))
	 (another-sheep (clone (test-sheep) ((other-var "other-value")) (:copy-all-values t))))
    (setf (get-property test-sheep 'var) "new-value")
    (is (equal "new-value" (get-property test-sheep 'var)))
    (is (equal "value" (get-property another-sheep 'var)))))

(test :copy-direct-values
  "Tests the :copy-direct-values clone option. It pulls in only the direct-slots
defined in the sheep that are being cloned.")

(test :nickname
  "Tests the :nickname clone option"
  (let* ((test-sheep (clone () ((var "value")) (:nickname "test-sheep")))
	 (another-sheep (clone (test-sheep) () (:copy-all-values t))))
    (is (equal "test-sheep" (sheep-nickname test-sheep)))
    (setf (sheep-nickname another-sheep) "Johnny Bravo")
    (is (equal "Johnny Bravo" (sheep-nickname another-sheep)))))

(test :mitosis
  "Tests to somehow figure out whether the :mitosis option actually works. This evil little option
changes the behavior of the CLONE macro almost entirely. For starters, it makes it so that CLONE
only accepts one argument, the Model. Like mitosis, CLONE then executes this weird cell-splitting
process where everything about the model except for its nickname and sid is copied over into the new
sheep object. This is useful, but is certainly shoved in in a very strange way, mostly because of 
the 1-argument restriction. It signals a MITOSIS-ERROR if the list is longer than 1"
  (let* ((the-parent (clone () ((name "Dad")) (:nickname "The-parent")))
	 (the-bro (clone (the-parent) ((name "Bro")) (:nickname "The Bro")))
	 (the-younger-bro (clone (the-bro) () (:mitosis t) (:nickname "The lil' bro")))
	 (some-random-dude (clone (the-parent) () (:mitosis nil) (:nickname "The Guy"))))
    (is (equal (get-property the-bro 'name) (get-property the-younger-bro 'name)))
    (is (equal (sheep-direct-parents the-bro) (sheep-direct-parents the-younger-bro)))
    (is (equal (sheep-direct-parents the-bro) (sheep-direct-parents some-random-dude)))
    (signals sheeple::mitosis-error (clone (the-bro some-random-dude) () (:mitosis t)))))

(test general-clone-options
  "Runs tests on the options feature of CLONE, and checks that existing options work."
  (signals invalid-option-error (clone () () (:anything-else)))
  (signals invalid-option-error (clone () () ()))
  (signals probably-meant-to-be-option (clone () (:copy-all-values t))))
