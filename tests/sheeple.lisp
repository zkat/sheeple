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
(def-suite sheep-objects :in sheeple)
(def-suite creation :in sheep-objects)
(def-suite allocation :in creation)

(def-suite allocate-std-sheep :in allocation)
(in-suite allocate-std-sheep)

(def-fixture allocate-std-sheep ()
  (let ((sheep (allocate-std-sheep)))
    (&body)))

(def-fixture allocate-sheep (metasheep)
  (let ((sheep (allocate-sheep metasheep)))
    (&body)))

(test (std-sheep-basic-structure
       :fixture allocate-std-sheep)
  (is (consp sheep))
  (is (vectorp (cdr sheep)))
  (is (= 6 (length (cdr sheep)))))

(test (std-sheep-initial-values
       :depends-on std-sheep-basic-structure
       :fixture allocate-std-sheep)
  (is (eql =standard-metasheep= (car sheep)))
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

(test (std-sheep-p :fixture allocate-std-sheep)
  (is (std-sheep-p sheep))
  (is (not (std-sheep-p (cons 1 2))))
  (is (not (std-sheep-p (cons =standard-metasheep=
                              nil))))
  (is (std-sheep-p (cons =standard-metasheep=
                         (make-array 6 :initial-element nil))))
  (is (not (std-sheep-p (cons =standard-metasheep=
                              (make-array 5 :initial-element nil)))))
  (is (not (std-sheep-p (cons =standard-metasheep=
                              (make-array 7 :initial-element nil)))))
  (setf (sheep-parents sheep) '(foo))
  (is (std-sheep-p sheep)))

(test (allocate-sheep :fixture (allocate-sheep =standard-metasheep=))
  (is (std-sheep-p sheep)))

(test (sheepp :fixture (allocate-sheep =standard-metasheep=))
  (is (sheepp sheep)))

(test equality-basic
  (let ((sheep1 (allocate-std-sheep)))
    (is (eq sheep1 sheep1))
    (is (eql sheep1 sheep1))
    ;; These two are run so I know the heap isn't blowing up.
    (is (equal sheep1 sheep1))
    (is (equalp sheep1 sheep1))))

(def-suite low-level-accessors :in sheep-objects)
(in-suite low-level-accessors)

(test (sheep-metasheep :fixture allocate-std-sheep)
  (is (eql =standard-metasheep= (sheep-metasheep sheep))))

(test (sheep-parents :fixture allocate-std-sheep)
  (is (eql nil (sheep-parents sheep)))
  (is (equal '(foo) (setf (sheep-parents sheep) '(foo))))
  (is (equal '(foo) (sheep-parents sheep)))
  (is (equal '(bar foo) (push 'bar (sheep-parents sheep))))
  (is (equal '(bar foo) (sheep-parents sheep))))

(test (sheep-pvalue-vector :fixture allocate-std-sheep)
  (is (eql nil (sheep-pvalue-vector sheep)))
  (is (equal '(foo) (setf (sheep-pvalue-vector sheep) '(foo))))
  (is (equal '(foo) (sheep-pvalue-vector sheep)))
  (is (equal '(bar foo) (push 'bar (sheep-pvalue-vector sheep))))
  (is (equal '(bar foo) (sheep-pvalue-vector sheep))))

(test (sheep-property-metaobjects :fixture allocate-std-sheep)
  (is (eql nil (sheep-property-metaobjects sheep)))
  (is (equal '(foo) (setf (sheep-property-metaobjects sheep) '(foo))))
  (is (equal '(foo) (sheep-property-metaobjects sheep)))
  (is (equal '(bar foo) (push 'bar (sheep-property-metaobjects sheep))))
  (is (equal '(bar foo) (sheep-property-metaobjects sheep))))

(test (sheep-roles :fixture allocate-std-sheep)
  (is (eql nil (sheep-roles sheep)))
  (is (equal '(foo) (setf (sheep-roles sheep) '(foo))))
  (is (equal '(foo) (sheep-roles sheep)))
  (is (equal '(bar foo) (push 'bar (sheep-roles sheep))))
  (is (equal '(bar foo) (sheep-roles sheep))))

(test (%sheep-hierarchy-cache :fixture allocate-std-sheep)
  (is (eql nil (%sheep-hierarchy-cache sheep)))
  (is (equal '(foo) (setf (%sheep-hierarchy-cache sheep) '(foo))))
  (is (equal '(foo) (%sheep-hierarchy-cache sheep)))
  (is (equal '(bar foo) (push 'bar (%sheep-hierarchy-cache sheep))))
  (is (equal '(bar foo) (%sheep-hierarchy-cache sheep))))

(test (%sheep-children :fixture allocate-std-sheep)
  (is (eql nil (%sheep-children sheep)))
  (is (equal '(foo) (setf (%sheep-children sheep) '(foo))))
  (is (equal '(foo) (%sheep-children sheep)))
  (is (equal '(bar foo) (push 'bar (%sheep-children sheep))))
  (is (equal '(bar foo) (%sheep-children sheep))))

(def-suite inheritance :in sheep-objects)
(def-suite inheritance-basic :in inheritance)
(in-suite inheritance-basic)

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
    (setf (sheep-parents a) (list =standard-sheep=))
    (setf (sheep-parents b) (list =standard-sheep=))
    (setf (sheep-parents c) (list =standard-sheep=))
    (setf (sheep-parents d) (list a b c))
    (is (equal (list (list d a) (list a b) (list b c))
               (local-precedence-ordering d)))))

(test with-some-sheep-hierarchy
  (with-some-sheep-hierarchy (a b c d)
      (() (a) (a) (b c))
    (is (null (sheep-parents a)))
    (is (equal (list a) (sheep-parents b)))
    (is (equal (list a) (sheep-parents c)))
    (is (equal (list b c) (sheep-parents d))))
  (signals sheeple-hierarchy-error
    (with-some-sheep-hierarchy (a b c d)
        (() (a) (b) (a c)))))

(test std-tie-breaker-rule
  (let* ((a (allocate-std-sheep))
         (b (allocate-std-sheep))
         (c (allocate-std-sheep))
         (e (allocate-std-sheep))
         (f (allocate-std-sheep))
         (g (allocate-std-sheep)))
    (setf (sheep-parents a) (list =standard-sheep=))
    (setf (sheep-parents b) (list =standard-sheep=))
    (setf (sheep-parents c) (list =standard-sheep=))
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

(test std-compute-sheep-hierarchy-list
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

(def-suite inheritance-caching :in inheritance)
(in-suite inheritance-caching)

(test (%child-cache-full-p :fixture allocate-std-sheep)
  (is (not (%child-cache-full-p sheep)))
  (setf (%sheep-children sheep)
        (make-array 5 :initial-element (make-weak-pointer sheep)))
  (is (%child-cache-full-p sheep))
  (setf (%sheep-children sheep)
        (make-array 5 :initial-element nil))
  (is (not (%child-cache-full-p sheep)))
  (setf (aref (%sheep-children sheep)
              1)
        (make-weak-pointer sheep))
  (is (not (%child-cache-full-p sheep))))

(test (%adjust-child-cache :fixture allocate-std-sheep)
  ;; todo - this needs to check that existing entries are moved over correctly.
  (%create-child-cache sheep)
  (is (= 5 (length (%sheep-children sheep))))
  (is (not (adjustable-array-p (%sheep-children sheep))))
  (setf (aref (%sheep-children sheep) 0) (make-weak-pointer sheep))
  (setf (aref (%sheep-children sheep) 1) (make-weak-pointer sheep))
  (setf (aref (%sheep-children sheep) 2) (make-weak-pointer sheep))
  (setf (aref (%sheep-children sheep) 3) (make-weak-pointer sheep))
  (setf (aref (%sheep-children sheep) 4) (make-weak-pointer sheep))
  (is (eql sheep (%adjust-child-cache sheep)))
  (is (= 100 (length (%sheep-children sheep))))
  (is (eql sheep (weak-pointer-value (aref (%sheep-children sheep) 0))))
  (is (eql sheep (weak-pointer-value (aref (%sheep-children sheep) 1))))
  (is (eql sheep (weak-pointer-value (aref (%sheep-children sheep) 2))))
  (is (eql sheep (weak-pointer-value (aref (%sheep-children sheep) 3))))
  (is (eql sheep (weak-pointer-value (aref (%sheep-children sheep) 4))))
  (is (adjustable-array-p (%sheep-children sheep)))
  (is (eql sheep (%adjust-child-cache sheep)))
  (is (= 200 (length (%sheep-children sheep))))
  (is (eql sheep (weak-pointer-value (aref (%sheep-children sheep) 0))))
  (is (eql sheep (weak-pointer-value (aref (%sheep-children sheep) 1))))
  (is (eql sheep (weak-pointer-value (aref (%sheep-children sheep) 2))))
  (is (eql sheep (weak-pointer-value (aref (%sheep-children sheep) 3))))
  (is (eql sheep (weak-pointer-value (aref (%sheep-children sheep) 4)))))

(test (%create-child-cache :fixture allocate-std-sheep)
  (is (null (%sheep-children sheep)))
  (is (simple-vector-p (%create-child-cache sheep)))
  (is (simple-vector-p (%sheep-children sheep))))

(test %add-child
  (let ((sheep1 (allocate-std-sheep))
        (sheep2 (allocate-std-sheep)))
    (is (null (%sheep-children sheep1)))
    (is (null (%sheep-children sheep2)))
    (is (eql sheep1 (%add-child sheep2 sheep1)))
    (is (null (%sheep-children sheep2)))
    (is (simple-vector-p (%sheep-children sheep1)))
    (is (find sheep2 (%sheep-children sheep1) :key #'maybe-weak-pointer-value))))

(test (overwriting-garbage-children :depends-on %add-child)
  (loop :repeat 5 :for sheep := (allocate-std-sheep)
     :do (loop :repeat 5 :do (%add-child (allocate-std-sheep) sheep)
            (loop :repeat 9999 :collect (list))
            :finally (gc :full t))
     (unless (every #'maybe-weak-pointer-value (%sheep-children sheep))
       (return
         (if (= 5 (length (%sheep-children (%add-child (allocate-std-sheep) sheep))))
             (pass "#'%ADD-CHILD overrode a garbage-collected child")
             (fail "#'%ADD-CHILD didn't override garbage-collected children"))))
     :finally (skip "Unable to perform test -- not enough garbage collected")))

(test %remove-child
  (let ((sheep1 (allocate-std-sheep))
        (sheep2 (allocate-std-sheep)))
    (is (null (%sheep-children sheep1)))
    (is (null (%sheep-children sheep2)))
    (%add-child sheep2 sheep1)
    (is (eql sheep1 (%remove-child sheep2 sheep1) ))
    (is (simple-vector-p (%sheep-children sheep1)))
    (is (not (find sheep2 (%sheep-children sheep1) :key #'maybe-weak-pointer-value)))))

(test %map-children
  (let ((parent (allocate-std-sheep))
        (child1 (allocate-std-sheep))
        (child2 (allocate-std-sheep))
        (child3 (allocate-std-sheep)))
    (%add-child child1 parent)
    (%add-child child2 parent)
    (%add-child child3 parent)
    (is (vectorp (%map-children (lambda (child)
                                  (setf (cdr child) nil))
                                parent)))
    (is (null (cdr child1)))
    (is (null (cdr child2)))
    (is (null (cdr child3)))))

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
    (is (find sheep2 (sheep-parents sheep1)))
    (is (find sheep2 (%sheep-hierarchy-cache sheep1)))))

(test finalize-sheep-inheritance
  ;; todo - write tests for this once bootstrapping is set...
)

(def-suite add/remove-parents :in inheritance)
(in-suite add/remove-parents)

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

(test sheep-hierarchy-list
  (let ((a (allocate-std-sheep))
        (b (allocate-std-sheep))
        (c (allocate-std-sheep)))
    (is (eql nil (sheep-hierarchy-list c)))
    (is (eql a (add-parent b a)))
    (is (eql b (add-parent c b)))
    (is (equal (list a b c) (sheep-hierarchy-list a)))))

(def-suite inheritance-predicates :in inheritance)
(in-suite inheritance-predicates)

(test parentp
  (let ((a (allocate-std-sheep))
        (b (allocate-std-sheep))
        (c (allocate-std-sheep)))
    (add-parent a b)
    (add-parent b c)
    (is (parentp a b))
    (is (parentp b c))
    (is (not (parentp a c)))
    (is (not (parentp c a)))
    (is (not (parentp b a)))
    (is (not (parentp c b)))))

(test childp
  (let ((a (allocate-std-sheep))
        (b (allocate-std-sheep))
        (c (allocate-std-sheep)))
    (add-parent a b)
    (add-parent b c)
    (is (childp b a))
    (is (childp c b))
    (is (not (childp c a)))
    (is (not (childp a c)))
    (is (not (childp a b)))
    (is (not (childp b c)))))

(test ancestorp
  (let ((a (allocate-std-sheep))
        (b (allocate-std-sheep))
        (c (allocate-std-sheep)))
    (add-parent a b)
    (add-parent b c)
    (is (ancestorp a b))
    (is (ancestorp b c))
    (is (ancestorp a c))
    (is (not (ancestorp c a)))
    (is (not (ancestorp b a)))
    (is (not (ancestorp c b)))))

(test descendantp
  (let ((a (allocate-std-sheep))
        (b (allocate-std-sheep))
        (c (allocate-std-sheep)))
    (add-parent a b)
    (add-parent b c)
    (is (descendantp b a))
    (is (descendantp c b))
    (is (descendantp c a))
    (is (not (descendantp a c)))
    (is (not (descendantp a b)))
    (is (not (descendantp b c)))))

;;;
;;; Cloning
;;;
(def-suite cloning :in sheeple)

(def-suite clone-general :in cloning)
(in-suite clone-general)

(postboot-test ensure-sheep
  ;; basic
  (let ((sheep (ensure-sheep nil)))
    (is (sheepp sheep))
    (is (std-sheep-p sheep))
    (is (eql =standard-sheep= (car (sheep-parents (ensure-sheep nil)))))
    (is (eql sheep (car (sheep-parents (ensure-sheep (list sheep))))))
    (is (eql =standard-metasheep= (sheep-metasheep sheep))))
  ;; properties arg
  (let ((sheep (ensure-sheep nil
                             :properties '((foo bar) (baz quux)))))
    (is (has-direct-property-p sheep 'foo))
    (is (has-direct-property-p sheep 'baz))
    (is (eql 'bar (direct-property-value sheep 'foo)))
    (is (eql 'quux (direct-property-value sheep 'baz))))
  ;; other metasheep
  (let* ((test-metasheep (ensure-sheep =standard-metasheep=))
         (sheep (ensure-sheep nil :metasheep test-metasheep)))
    ;; metasheep tests
    (is (sheepp test-metasheep))
    (is (std-sheep-p test-metasheep))
    (is (eql =standard-metasheep= (sheep-metasheep test-metasheep)))
    ;; sheep tests
    (is (sheepp sheep))
    (is (not (std-sheep-p sheep)))
    (is (eql test-metasheep (sheep-metasheep sheep)))
    (is (eql =standard-sheep= (car (sheep-parents sheep))))))

(postboot-test clone
  (is (eql =standard-metasheep= (sheep-metasheep (clone))))
  (is (sheepp (clone)))
  (is (std-sheep-p (clone)))
  (is (eql =standard-sheep= (car (sheep-parents (clone)))))
  (let ((obj1 (clone)))
    (is (eql obj1
             (car (sheep-parents (clone obj1))))))
  (let* ((obj1 (clone))
         (obj2 (clone obj1)))
    (is (eql obj1
             (car (sheep-parents obj2)))))
  (let* ((o1 (clone))
         (o2 (clone))
         (o3 (clone))
         (o4 (clone))
         (sheep (clone o1 o2 o3 o4)))
    (is (parentp o1 sheep))
    (is (parentp o2 sheep))
    (is (parentp o3 sheep))
    (is (parentp o4 sheep))))

(postboot-test sheep-nickname
  (let ((sheep (clone)))
    (setf (sheep-nickname sheep) 'test)
    (is (eq 'test (sheep-nickname sheep)))
    (is (eq 'test (sheep-nickname (clone sheep))))))

(postboot-test sheep-documentation
  (let ((sheep (clone)))
    (setf (sheep-documentation sheep) 'test)
    (is (eq 'test (sheep-documentation sheep)))
    (is (eq 'test (sheep-documentation (clone sheep))))))

(test copy-sheep
  ;; TODO
  )

;;;
;;; DEFCLONE
;;;
(def-suite defclone :in cloning)
(in-suite defclone)

;;; macro processing
(test canonize-sheeple
  (is (equal '(list foo bar baz) (canonize-sheeple '(foo bar baz))))
  (is (equal '(list) (canonize-sheeple '()))))

(test canonize-property
  (is (equal '(list 'VAR "value") (canonize-property '(var "value"))))
  (is (equal '(list 'VAR "value" :readers '(var) :writers '((setf var)))
             (canonize-property '(var "value") t)))
  (is (equal '(list 'VAR "value" :writers '((setf var)))
             (canonize-property '(var "value" :reader nil) t)))
  (is (equal '(list 'VAR "value" :readers '(var))
             (canonize-property '(var "value" :writer nil) t)))
  (is (equal '(list 'VAR "value")
             (canonize-property '(var "value" :accessor nil) t))))

(test canonize-properties
  (is (equal '(list (list 'VAR "value")) (canonize-properties '((var "value")))))
  (is (equal '(list (list 'VAR "value") (list 'ANOTHER "another-val"))
             (canonize-properties '((var "value") (another "another-val")))))
  (is (equal '(list (list 'VAR "value" :readers '(var) :writers '((setf var)))
               (list 'ANOTHER "another-val" :readers '(another) :writers '((setf another))))
             (canonize-properties '((var "value") (another "another-val")) t))))

(test canonize-clone-options
  (is (equal '(:metasheep foo :other-option 'bar)
             (canonize-clone-options '((:metasheep foo) (:other-option 'bar))))))

(postboot-test defclone
  (let* ((parent (clone))
         (test-sheep (defclone (parent) ((var "value")))))
    (is (sheepp test-sheep))
    (is (parentp parent test-sheep))
    (is (has-direct-property-p test-sheep 'var))
    ;; TODO - this should also check that reader/writer/accessor combinations are properly added
    ))

;;;
;;; Protos
;;;
(def-suite protos :in cloning)
(in-suite protos)

(postboot-test defproto
  (let ((test-proto (defproto =test-proto= () ((var "value")))))
    (is (sheepp test-proto))
    (is (eql test-proto =test-proto=))
    (is (eql =standard-sheep= (car (sheep-parents test-proto))))
    (is (sheepp =test-proto=))
    (is (equal "value" (var =test-proto=)))
    (defproto =test-proto= () ((something-else "another-one")))
    (is (eql test-proto =test-proto=))
    (is (eql =standard-sheep= (car (sheep-parents test-proto))))
    (signals unbound-property (direct-property-value test-proto 'var))
    (is (equal "another-one" (something-else =test-proto=)))
    (is (equal "another-one" (something-else test-proto))))
  ;; TODO - check that options work properly
  )
(test create-or-reinit-sheep)
