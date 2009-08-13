;;;; -*- Mode: lisp; indent-tabs-mode: nil -*-
;;;;
;;;; This file is part of Sheeple.

;;;; tests/sheeple.lisp
;;;;
;;;; Unit tests for src/sheeple.lisp
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :sheeple)

;;;
;;; Allocation
;;;
(def-suite allocation :in sheeple)

(def-suite allocate-std-sheep :in allocation)
(in-suite allocate-std-sheep)

(def-fixture allocate-std-sheep ()
  (let ((sheep (allocate-std-sheep)))
    (&body)))

(test (std-sheep-basic-structure
       :fixture allocate-std-sheep)
  (is (consp sheep))
  (is (vectorp (cdr sheep)))
  (is (= 6 (length (cdr sheep)))))

(test (std-sheep-initial-values
       :depends-on std-sheep-basic-structure
       :fixture allocate-std-sheep)
  (is (eql =standard-sheep= (car sheep)))
  (is (null (svref (cdr sheep) 0)))
  (is (null (svref (cdr sheep) 1)))
  (is (null (svref (cdr sheep) 2)))
  (is (null (svref (cdr sheep) 3)))
  (is (null (svref (cdr sheep) 4)))
  (is (null (svref (cdr sheep) 5))))

(test (std-sheep-accessors
       :depends-on std-sheep-basic-structure
       :fixture allocate-std-sheep)
  (is (eq (setf (car sheep) (gensym)) (sheep-metasheep sheep)))
  (is (eq (setf (svref (cdr sheep) 0) (gensym)) (sheep-parents sheep)))
  (is (eq (setf (svref (cdr sheep) 1) (gensym)) (sheep-pvalue-vector sheep)))
  (is (eq (setf (svref (cdr sheep) 2) (gensym)) (sheep-property-metaobjects sheep)))
  (is (eq (setf (svref (cdr sheep) 3) (gensym)) (sheep-roles sheep)))
  (is (eq (setf (svref (cdr sheep) 4) (gensym)) (%sheep-hierarchy-cache sheep)))
  (is (eq (setf (svref (cdr sheep) 5) (gensym)) (%sheep-children sheep))))

(in-suite allocation)

(test std-sheep-p
  (let ((sheep (allocate-std-sheep)))
    (is (std-sheep-p sheep))
    (is (not (std-sheep-p (cons 1 2))))
    (is (not (std-sheep-p (cons =standard-sheep=
                                nil))))
    (is (std-sheep-p (cons =standard-sheep=
                           (make-array 6 :initial-element nil))))
    (is (not (std-sheep-p (cons =standard-sheep=
                                (make-array 5 :initial-element nil)))))
    (is (not (std-sheep-p (cons =standard-sheep=
                                (make-array 7 :initial-element nil)))))
    (setf (sheep-parents sheep) '(foo))
    (is (std-sheep-p sheep))))

(test (allocate-sheep)
  (let ((sheep (allocate-sheep =standard-sheep=)))
    (is (std-sheep-p sheep))))

(test sheepp
  (let ((sheep (allocate-sheep =standard-sheep=)))
    (is (sheepp sheep))))

(def-suite low-level-accessors :in sheeple)
(in-suite low-level-accessors)

(test sheep-metasheep
  (let ((sheep (allocate-std-sheep)))
    (is (eql =standard-sheep= (sheep-metasheep sheep)))))

(test sheep-parents
  (let ((sheep (allocate-std-sheep)))
    (is (eql nil (sheep-parents sheep)))
    (is (equal '(foo) (setf (sheep-parents sheep) '(foo))))
    (is (equal '(foo) (sheep-parents sheep)))
    (is (equal '(bar foo) (push 'bar (sheep-parents sheep))))
    (is (equal '(bar foo) (sheep-parents sheep)))))

(test sheep-pvalue-vector
  (let ((sheep (allocate-std-sheep)))
    (is (eql nil (sheep-pvalue-vector sheep)))
    (is (equal '(foo) (setf (sheep-pvalue-vector sheep) '(foo))))
    (is (equal '(foo) (sheep-pvalue-vector sheep)))
    (is (equal '(bar foo) (push 'bar (sheep-pvalue-vector sheep))))
    (is (equal '(bar foo) (sheep-pvalue-vector sheep)))))

(test sheep-property-metaobjects
  (let ((sheep (allocate-std-sheep)))
    (is (eql nil (sheep-property-metaobjects sheep)))
    (is (equal '(foo) (setf (sheep-property-metaobjects sheep) '(foo))))
    (is (equal '(foo) (sheep-property-metaobjects sheep)))
    (is (equal '(bar foo) (push 'bar (sheep-property-metaobjects sheep))))
    (is (equal '(bar foo) (sheep-property-metaobjects sheep)))))

(test sheep-roles
  (let ((sheep (allocate-std-sheep)))
    (is (eql nil (sheep-roles sheep)))
    (is (equal '(foo) (setf (sheep-roles sheep) '(foo))))
    (is (equal '(foo) (sheep-roles sheep)))
    (is (equal '(bar foo) (push 'bar (sheep-roles sheep))))
    (is (equal '(bar foo) (sheep-roles sheep)))))

(test %sheep-hierarchy-cache
  (let ((sheep (allocate-std-sheep)))
    (is (eql nil (%sheep-hierarchy-cache sheep)))
    (is (equal '(foo) (setf (%sheep-hierarchy-cache sheep) '(foo))))
    (is (equal '(foo) (%sheep-hierarchy-cache sheep)))
    (is (equal '(bar foo) (push 'bar (%sheep-hierarchy-cache sheep))))
    (is (equal '(bar foo) (%sheep-hierarchy-cache sheep)))))

(test %sheep-children
  (let ((sheep (allocate-std-sheep)))
    (is (eql nil (%sheep-children sheep)))
    (is (equal '(foo) (setf (%sheep-children sheep) '(foo))))
    (is (equal '(foo) (%sheep-children sheep)))
    (is (equal '(bar foo) (push 'bar (%sheep-children sheep))))
    (is (equal '(bar foo) (%sheep-children sheep)))))

(def-suite inheritance :in sheeple)
(in-suite inheritance)

(test collect-ancestors
  (let ((sheep1 (allocate-std-sheep))
        (sheep2 (allocate-std-sheep))
        (sheep3 (allocate-std-sheep)))
    (setf (sheep-parents sheep1) (list sheep2))
    (setf (sheep-parents sheep2) (list sheep3))
    (is (find sheep2 (collect-ancestors sheep1)))
    (is (find sheep3 (collect-ancestors sheep2)))
    (is (find sheep3 (collect-ancestors sheep1)))
    (is (not (find sheep1 (collect-ancestors sheep1))))
    (is (not (find sheep1 (collect-ancestors sheep2))))))

(test local-precedence-ordering
  (let* ((a (allocate-std-sheep))
         (b (allocate-std-sheep))
         (c (allocate-std-sheep))
         (d (allocate-std-sheep)))
    (setf (sheep-parents a) (list =dolly=))
    (setf (sheep-parents b) (list =dolly=))
    (setf (sheep-parents c) (list =dolly=))
    (setf (sheep-parents d) (list a b c))
    (is (equal (list (list d a) (list a b) (list b c))
               (local-precedence-ordering d)))))

(test std-tie-breaker-rule
  (let* ((a (allocate-std-sheep))
         (b (allocate-std-sheep))
         (c (allocate-std-sheep))
         (e (allocate-std-sheep))
         (f (allocate-std-sheep)) 
         (g (allocate-std-sheep)))
    (setf (sheep-parents a) (list =dolly=))
    (setf (sheep-parents b) (list =dolly=))
    (setf (sheep-parents c) (list =dolly=))
    (setf (sheep-parents e) (list a))
    (setf (sheep-parents f) (list b))
    (setf (sheep-parents g) (list c))
    (is (eq c (std-tie-breaker-rule (list a b c) (list e f g))))
    (is (eq b (std-tie-breaker-rule (list a b c) (list e g f))))
    (is (eq a (std-tie-breaker-rule (list a b c) (list g f e))))))

(test compute-sheep-hierarchy-list
  (let* ((parent (allocate-std-sheep))
         (child (allocate-std-sheep)))
    (setf (sheep-parents child) (list parent))
    (is (equal (list child parent)
               (compute-sheep-hierarchy-list child))))
  (let* ((a (allocate-std-sheep))
         (b (allocate-std-sheep))
         (c (allocate-std-sheep)) 
         (d (allocate-std-sheep))
         (e (allocate-std-sheep))
         (f (allocate-std-sheep)) 
         (g (allocate-std-sheep))
         (h (allocate-std-sheep)))
    (setf (sheep-parents c) (list a))
    (setf (sheep-parents d) (list a))
    (setf (sheep-parents e) (list b c))
    (setf (sheep-parents f) (list d))
    (setf (sheep-parents g) (list c f))
    (setf (sheep-parents h) (list g e))
    (is (equal (list h g e b c f d a) (compute-sheep-hierarchy-list h)))))

(test initialize-children-cache
  (let ((sheep (allocate-std-sheep)))
    (is (null (%sheep-children sheep)))
    (initialize-children-cache sheep)
    (is (hash-table-p (%sheep-children sheep)))))

(test memoize-sheep-hierarchy-list
  (let ((sheep1 (allocate-std-sheep))
        (sheep2 (allocate-std-sheep)))
    (setf (sheep-parents sheep1) (list sheep2))
    (is (null (%sheep-hierarchy-cache sheep1)))
    (is (null (%sheep-hierarchy-cache sheep2)))
    (memoize-sheep-hierarchy-list sheep1)
    (is (equal (list sheep1 sheep2) (%sheep-hierarchy-cache sheep1)))))

(test std-finalize-sheep-inheritance
  (let ((sheep1 (allocate-std-sheep))
        (sheep2 (allocate-std-sheep)))
    (is (equal (list sheep2) (setf (sheep-parents sheep1) (list sheep2))))
    (is (eql sheep1 (std-finalize-sheep-inheritance sheep1)))
    (is (hash-table-p (%sheep-children sheep2)))
    (is (gethash sheep1 (%sheep-children sheep2)))
    (is (find sheep2 (sheep-parents sheep1)))
    (is (find sheep2 (%sheep-hierarchy-cache sheep1)))))

(test finalize-sheep-inheritance
  ;; todo - write tests for this once bootstrapping is set...
  )
(test remove-parent
  (let ((sheep1 (allocate-std-sheep))
        (sheep2 (allocate-std-sheep)))
    (add-parent sheep2 sheep1)
    (is (eql sheep2 (car (sheep-parents sheep1))))
    (signals error (remove-parent sheep1 sheep1))
    (is (eql sheep1 (remove-parent sheep2 sheep1)))
    (is (null (sheep-parents sheep1)))
    (signals error (remove-parent sheep2 sheep1))
    (signals error (remove-parent sheep1 sheep1)))
  ;; todo - more thorough tests, post-bootstrap
  )

(test std-remove-parent
  (let ((sheep1 (allocate-std-sheep))
        (sheep2 (allocate-std-sheep)))
    (std-add-parent sheep2 sheep1)
    (is (eql sheep2 (car (sheep-parents sheep1))))
    (signals error (std-remove-parent sheep1 sheep1))
    (is (eql sheep1 (std-remove-parent sheep2 sheep1)))
    (is (null (sheep-parents sheep1)))
    (signals error (std-remove-parent sheep2 sheep1))
    (signals error (std-remove-parent sheep1 sheep1))))

(test std-add-parent
  (let ((sheep1 (allocate-std-sheep))
        (sheep2 (allocate-std-sheep)))
    (is (eql sheep1 (std-add-parent sheep2 sheep1)))
    (is (eql sheep2 (car (sheep-parents sheep1))))
    (signals error (std-add-parent sheep1 sheep1))
    (signals error (std-add-parent sheep2 sheep1))
    (signals sheeple-hierarchy-error (std-add-parent sheep1 sheep2))))

(test add-parent
  (let ((sheep1 (allocate-std-sheep))
        (sheep2 (allocate-std-sheep)))
    (is (eql sheep1 (add-parent sheep2 sheep1)))
    (is (eql sheep2 (car (sheep-parents sheep1))))
    (signals error (add-parent sheep1 sheep1))
    (signals error (add-parent sheep2 sheep1))
    (signals sheeple-hierarchy-error (add-parent sheep1 sheep2)))
  ;; todo - more thorough tests post-bootstrap
  )
(test add-parents
  (let ((a (allocate-std-sheep))
        (b (allocate-std-sheep))
        (c (allocate-std-sheep)))
    (is (eql c (add-parents (list a b) c)))
    (is (equal (list a b) (sheep-parents c)))))

;; (def-suite cloning :in sheeple)
;; (in-suite cloning)

;; (test add-parent
;;   (let ((obj1 (allocate-std-sheep))
;;         (obj2 (allocate-std-sheep)))
;;     (setf (sheep-parents obj1) (list =dolly=))
;;     (setf (sheep-parents obj2) (list =dolly=))
;;     (is (eql obj1 (add-parent obj2 obj1)))
;;     (is (eql obj2 (car (sheep-parents obj1))))))

;; (test add-parents
;;   (let ((parent1 (allocate-std-sheep))
;;         (parent2 (allocate-std-sheep))
;;         (parent3 (allocate-std-sheep))
;;         (child (allocate-std-sheep)))
;;     (setf (sheep-parents parent1) (list =dolly=))
;;     (setf (sheep-parents parent2) (list =dolly=))
;;     (setf (sheep-parents parent3) (list =dolly=))
;;     (setf (sheep-parents child) (list =dolly=))
;;     (is (eql child (add-parents (list parent1 parent2 parent3) child)))
;;     (is (equal (list parent1 parent2 parent3 =dolly=)
;;                (sheep-parents child)))))

;; ;;;
;; ;;; Cloning
;; ;;;
;; (def-suite cloning :in sheeple)

;; (def-suite clone-general :in cloning)
;; (in-suite clone-general)

;; (test equitable-sheep
;;   (let ((sheep1 (clone)))
;;     (is (eql sheep1 sheep1))))

;; (defclass test-sheep-class (standard-sheep) ())
;; (test spawn-sheep
;;   (let ((standard-sheep (clone))
;;         (test-metaclass-sheep (spawn-sheep nil :metaclass 'test-sheep-class)))
;;     (is (eql =dolly= (car (sheep-parents (spawn-sheep nil)))))
;;     (is (eql standard-sheep (car (sheep-parents (spawn-sheep (list standard-sheep))))))
;;     (is (eql (find-class 'test-sheep-class) (class-of test-metaclass-sheep)))))

;; (test init-sheep
;;   (let ((parent (spawn-sheep () :properties '((prop1 NIL)))))
;;     (defreply init-sheep :before ((sheep parent) &key)
;;       (is (has-property-p sheep 'prop1))
;;       (is (eq NIL (property-value sheep 'prop1)))
;;       (is (not (has-property-p sheep 'prop2))))
;;     (defreply init-sheep :after ((sheep parent) &key)
;;       (is (has-property-p sheep 'prop1))
;;       (is (eq 'val1 (property-value sheep 'prop1)))
;;       (is (has-property-p sheep 'prop2))
;;       (is (eq 'val2 (property-value sheep 'prop2))))
;;     (defreply init-sheep :around ((sheep parent) &key)
;;       (is (eq NIL (property-value sheep 'prop1)))
;;       (prog1 (call-next-reply)
;;         (is (eq 'val1 (property-value sheep 'prop1)))
;;         (is (eq 'val2 (property-value sheep 'prop2)))))
;;     (let ((test-sheep (spawn-sheep (list parent) :properties '((prop1 val1) (prop2 val2)))))
;;       (is (eq 'val1 (property-value test-sheep 'prop1)))
;;       (is (eq 'val2 (property-value test-sheep 'prop2))))))

;; (test reinit-sheep
;;   (let ((test-sheep (clone))
;;         (another (clone)))
;;     (is (eql test-sheep (add-property test-sheep 'var "value" :make-accessor-p nil)))
;;     (is (has-direct-property-p test-sheep 'var))
;;     (is (eql test-sheep (add-parent another test-sheep)))
;;     (is (parent-p another test-sheep))
;;     (is (eql test-sheep (reinit-sheep test-sheep)))
;;     (is (parent-p =dolly= test-sheep))
;;     (is (not (has-direct-property-p test-sheep 'var)))
;;     (is (not (parent-p another test-sheep)))
;;     (is (eql test-sheep (reinit-sheep test-sheep :new-parents (list another))))
;;     (is (parent-p another test-sheep))))

;; (test clone
;;   (is (eql =dolly= (car (sheep-parents (clone)))))
;;   (let ((obj1 (clone)))
;;     (is (eql obj1
;;              (car (sheep-parents (clone obj1))))))
;;   (let* ((obj1 (clone))
;;          (obj2 (clone obj1)))
;;     (is (eql obj1
;;              (car (sheep-parents obj2))))))

;; (test sheep-nickname
;;   (let ((sheep (clone)))
;;     (setf (sheep-nickname sheep) 'test)
;;     (is (eq 'test (sheep-nickname sheep)))))

;; (test sheep-documentation
;;   (let ((sheep (clone)))
;;     (setf (sheep-documentation sheep) 'test)
;;     (is (eq 'test (sheep-documentation sheep)))))

;; (test sheep-parents
;;   (let* ((grandpa (clone))
;;          (father (clone grandpa))
;;          (child (clone father)))
;;     (is (= 1 (length (sheep-parents father))))
;;     (is (eql grandpa (car (sheep-parents father))))
;;     (is (not (member grandpa (sheep-parents child))))
;;     (is (eql =dolly= (car (sheep-parents grandpa))))))

;; (test sheep-direct-roles)
;; (test sheep-hierarchy-list
;;   (let* ((parent (clone))
;;          (child (clone parent)))
;;     (is (member child (sheep-hierarchy-list child)))
;;     (is (member parent (sheep-hierarchy-list child)))
;;     (is (member =dolly= (sheep-hierarchy-list child)))
;;     (is (member =t= (sheep-hierarchy-list child)))))

;; (defclass foo () ())
;; (test sheep-p
;;   (let ((sheep (clone))
;;         (special-sheep (spawn-sheep nil :metaclass 'test-sheep-class)))
;;     (is (sheep-p sheep))
;;     (is (sheep-p special-sheep))
;;     (is (not (sheep-p (make-instance 'foo))))
;;     (is (not (sheep-p "foo")))
;;     (is (not (sheep-p 5)))))

;; (test copy-sheep ;; TODO - this isn't even written properly yet
;;   )

;; (test add-parent
;;   (let ((obj1 (clone))
;;         (obj2 (clone)))
;;     (is (eql =dolly= (car (sheep-parents obj1))))
;;     (is (eql =dolly= (car (sheep-parents obj2))))
;;     (is (eql obj1 (add-parent obj2 obj1)))
;;     (is (eql obj2 (car (sheep-parents obj1))))))

;; (test add-parents
;;   (let ((parent1 (clone))
;;         (parent2 (clone))
;;         (parent3 (clone))
;;         (child (clone)))
;;     (setf (sheep-nickname parent1) 'parent1)
;;     (setf (sheep-nickname parent2) 'parent2)
;;     (setf (sheep-nickname parent3) 'parent3)
;;     (is (eql child (add-parents (list parent1 parent2 parent3) child)))
;;     (is (equal (list parent1 parent2 parent3 =dolly=)
;;                (sheep-parents child)))))

;; (test remove-parent
;;   (let* ((p1 (clone))
;;          (p2 (clone))
;;          (child (clone p1 p2)))
;;     (is (equal (list p1 p2) (sheep-parents child)))
;;     (is (eql child (remove-parent p1 child)))
;;     (is (equal (list p2) (sheep-parents child)))))

;; (test allocate-sheep
;;   (is (sheep-p (allocate-sheep)))
;;   (is (sheep-p (allocate-sheep 'test-sheep-class))))

;; ;;;
;; ;;; Inheritance
;; ;;;
;; (def-suite inheritance :in cloning)
;; (in-suite inheritance)
;; (test parent-p
;;   (let* ((grandpa (clone))
;;          (father (clone grandpa))
;;          (child (clone father)))
;;     (is (parent-p grandpa father))
;;     (is (parent-p father child))
;;     (is (not (parent-p child father)))
;;     (is (not (parent-p grandpa child)))))

;; (test child-p
;;   (let* ((grandpa (clone))
;;          (father (clone grandpa))
;;          (child (clone father)))
;;     (is (child-p child father))
;;     (is (child-p father grandpa))
;;     (is (not (child-p grandpa father)))
;;     (is (not (child-p father child)))))

;; (test ancestor-p
;;   (let* ((grandpa (clone))
;;          (father (clone grandpa))
;;          (child (clone father)))
;;     (is (ancestor-p grandpa father))
;;     (is (ancestor-p grandpa child))
;;     (is (ancestor-p father child))
;;     (is (not (ancestor-p child grandpa)))
;;     (is (not (ancestor-p child father)))
;;     (is (not (ancestor-p father grandpa)))))

;; (test descendant-p
;;   (let* ((grandpa (clone))
;;          (father (clone grandpa))
;;          (child (clone father)))
;;     (is (descendant-p father grandpa))
;;     (is (descendant-p child grandpa))
;;     (is (descendant-p child father))
;;     (is (not (descendant-p grandpa child)))
;;     (is (not (descendant-p father child)))
;;     (is (not (descendant-p grandpa father)))))

;; (test collect-parents
;;   (let ((sheep (clone)))
;;     (is (equal (list =t= =dolly= sheep) (collect-parents sheep)))))

;; (test compute-sheep-hierarchy-list
;;   (let* ((parent (clone))
;;          (child (clone parent)))
;;     (is (equal (list child parent =dolly= =t=)
;;                (compute-sheep-hierarchy-list child)))))

;; ;;;
;; ;;; DEFCLONE
;; ;;;
;; (def-suite defclone :in cloning)
;; (in-suite defclone)

;; ;;; macro processing
;; (test canonize-sheeple
;;   (is (equal '(list foo bar baz) (canonize-sheeple '(foo bar baz)))))

;; (test canonize-property
;;   (is (equal '(list 'VAR "value") (canonize-property '(var "value"))))
;;   (is (equal '(list 'VAR "value" :readers '(var) :writers '((setf var)))
;;              (canonize-property '(var "value") t)))
;;   (is (equal '(list 'VAR "value" :writers '((setf var)))
;;              (canonize-property '(var "value" :reader nil) t)))
;;   (is (equal '(list 'VAR "value" :readers '(var))
;;              (canonize-property '(var "value" :writer nil) t)))
;;   (is (equal '(list 'VAR "value")
;;              (canonize-property '(var "value" :accessor nil) t))))

;; (test canonize-properties
;;   (is (equal '(list (list 'VAR "value")) (canonize-properties '((var "value"))))))

;; (test canonize-clone-options
;;   (is (equal '(:metaclass 'foo :other-option 'bar)
;;              (canonize-clone-options '((:metaclass 'foo) (:other-option 'bar))))))

;; (test defclone
;;   (let* ((parent (clone))
;;          (test-sheep (defclone (parent) ((var "value")))))
;;     (is (sheep-p test-sheep))
;;     (is (parent-p parent test-sheep))
;;     (is (has-direct-property-p test-sheep 'var))
;;     ;; TODO - this should also check that reader/writer/accessor combinations are properly added
;;     ))

;; ;;;
;; ;;; Protos
;; ;;;
;; (def-suite protos :in cloning)
;; (in-suite protos)

;; (test defproto
;;   (let ((test-proto (defproto =test-proto= () ((var "value")))))
;;     (is (sheep-p test-proto))
;;     (is (eql test-proto =test-proto=))
;;     (is (eql =dolly= (car (sheep-parents test-proto))))
;;     (is (sheep-p =test-proto=))
;;     (is (equal "value" (var =test-proto=)))
;;     (defproto =test-proto= () ((something-else "another-one")))
;;     (is (eql test-proto =test-proto=))
;;     (is (eql =dolly= (car (sheep-parents test-proto))))
;;     (signals unbound-property (direct-property-value test-proto 'var))
;;     (is (equal "another-one" (something-else =test-proto=)))
;;     (is (equal "another-one" (something-else test-proto))))
;;   ;; TODO - check that options work properly (:metaclass, :documentation, :nickname, etc)
;;   ;;        remember that :nickname should override defproto's own nickname-setting.
;;   )
