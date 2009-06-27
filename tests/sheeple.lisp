;; This file is part of Sheeple.

;; tests/sheeple.lisp
;;
;; Unit tests for src/sheeple.lisp
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :sheeple)

(export 'sheeple-tests)

(def-suite sheeple)
(defun run-all-tests ()
  (run! 'sheeple))

(def-suite cloning :in sheeple)

(def-suite clone-general :in cloning)
(in-suite clone-general)
(test equitable-sheep
  "Tests that sheep are correctly identified as equalp.
WARNING: This tests blows the stack if some weird circularity pops up."
  (let ((sheep1 (clone))
	(sheep2 (clone)))
    ;; This should -not- blow the stack.
    (is (equalp sheep1 sheep2))
    (is (eql sheep1 sheep1))))

(test spawn-sheep)

(test clone
  "Basic cloning"
  (is (eql #@dolly (car (sheep-direct-parents (clone)))))
  (let ((obj1 (clone)))
    (is (eql obj1
	     (car (sheep-direct-parents (clone obj1))))))
  (let* ((obj1 (clone))
         (obj2 (clone obj1)))
    (is (eql obj1
	     (car (sheep-direct-parents obj2)))))
  (signals sheep-hierarchy-error (let ((obj1 (clone))
				       (obj2 (clone)))
				   (add-parent obj1 obj2)
				   (clone obj1 obj2)))
  (signals sheep-hierarchy-error (let* ((obj1 (clone))
					(obj2 (clone obj1)))
				   (clone obj1 obj2))))

(test sheep-nickname)
(test sheep-documentation)
(test sheep-parents)
(test sheep-direct-roles)
(test sheep-hierarchy-list)
(test sheep-id)

(test sheep-p)
(test copy-sheep)
(test finalize-sheep)
(test add-parent)
(test add-parents)
(test remove-parent)
(test allocate-sheep)
(test spawn-sheep)

(def-suite inheritance :in cloning)
(in-suite inheritance)
(test parentp
  (let* ((grandpa (clone))
         (father (clone grandpa))
         (child (clone father)))
    (is (parentp grandpa father))
    (is (parentp father child))
    (is (not (parentp child father)))
    (is (not (parentp grandpa child)))))

(test childp
  (let* ((grandpa (clone))
         (father (clone grandpa))
         (child (clone father)))
    (is (childp child father))
    (is (childp grandpa child))
    (is (not (childp grandpa father)))
    (is (not (childp father child)))))

(test ancestorp
  (let* ((grandpa (clone))
         (father (clone grandpa))
         (child (clone father)))
    (is (ancestorp grandpa father))
    (is (ancestorp grandpa child))
    (is (ancestorp father child))
    (is (not (ancestorp child grandpa)))
    (is (not (ancestorp child father)))
    (is (not (ancestorp father grandpa)))))

(test descendantp
  (let* ((grandpa (clone))
         (father (clone grandpa))
         (child (clone father)))
    (is (descendantp father grandpa))
    (is (descendantp child grandpa))
    (is (descendantp child father))
    (is (not (descendantp grandpa child)))
    (is (not (descendantp father child)))
    (is (not (descendantp grandpa father)))))

(test collect-parents)
(test compute-sheep-hierarchy-list)
(test finalize-sheep)
;;;
;;; DEFCLONE
;;;
(def-suite defclone :in cloning)
(in-suite defclone)

;;; macro processing
(test canonize-sheeple
  (is (equal '(list foo bar baz) (canonize-sheeple '(foo bar baz)))))

(test canonize-property
  (is (equal '(list (list 'VAR "value")) (canonize-property '(var "value"))))
  (is (equal '(list 'VAR "value" :readers '(var) :writers '((setf var)))
             (canonize-property '(var "value") t)))
  (is (equal '(list 'VAR "value" :writers '((setf var)))
             (canonize-property '(var "value" :reader nil) t)))
  (is (equal '(list 'VAR "value" :readers '(var))
             (canonize-property '(var "value" :writer nil) t)))
  (is (equal '(list 'VAR "value")
             (canonize-property '(var "value" :accessor nil) t))))

(test canonize-properties
  (is (equal '(list (list 'VAR "value")) (canonize-properties '((var "value"))))))

(test canonize-clone-options
  (is (equal '(:metaclass 'foo :other-option 'bar)
             (canonize-clone-options '((:metaclass 'foo) (:other-option 'bar)))))
  (is (equal '(metaclass 'foo other-option 'bar)
             (canonize-clone-options '((metaclass 'foo) (other-option 'bar))))))

(test defclone)

