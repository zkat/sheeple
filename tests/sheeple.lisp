;;;; -*- Mode: lisp; indent-tabs-mode: nil -*-
;;;;
;;;; This file is part of Sheeple.

;;;; tests/sheeple.lisp
;;;;
;;;; Unit tests for src/sheeple.lisp
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :sheeple)

(def-fixture allocate-std-sheep ()
  (let ((sheep (allocate-std-sheep)))
    (&body)))

(def-fixture allocate-sheep (metasheep)
  (let ((sheep (allocate-sheep metasheep)))
    (&body)))

(def-suite sheep-objects :in sheeple)

;;;
;;; Early printing
;;;

(def-suite early-printing :in sheep-objects)
(in-suite early-printing)

(test verify-print-settings
  (let ((*print-pretty* *print-pretty*)
        (*print-circle* *print-circle*))
    (flet ((set-them (pretty circle)
             (setf *print-pretty* pretty
                   *print-circle* circle)))
      (set-them nil nil)
      (signals error (verify-print-settings))
      (set-them nil t)
      (signals warning (verify-print-settings))
      (set-them t nil)
      (5am:finishes (verify-print-settings))
      (set-them t t)
      (5am:finishes (verify-print-settings)))))

(test print-pretty
  (if *print-pretty*
      (pass "*PRINT-PRETTY* is true.")
      (fail "*PRINT-PRETTY* is false. This is such a transparent bit of code that
 the .asd is probably messed up if this test is failing.")))

;;; This test is pretty crude. The correct thing to do would be to
;;; parse the output, extract the implementation-dependant stuff,
;;; and verify the rest. This would be painful without CL-PPCRE.
(test sheep-printing
  (with-output-to-string (*standard-output*)
    (5am:finishes (print (allocate-std-sheep)))
    (5am:finishes (print (add-parent (allocate-std-sheep)
                                     (allocate-std-sheep))))))

;;;
;;; Allocation
;;;
;;; new structure for sheep objects:
;;; #(metasheep parents properties roles %hierarchy-cache %children)
;;; parents := proper list of parents, in order of precedence.
;;; properties := property*
;;; property := (property-metaobject . property-value)
;;; roles := proper list of direct roles
;;; %hierarchy-cache := list that holds a cached version of the sheep's entire hierarchy-list
;;; %children := a vector of weak pointers that points to all of this sheep's children

(def-suite creation :in sheep-objects)
(def-suite allocation :in creation)

(def-suite allocate-std-sheep :in allocation)
(in-suite allocate-std-sheep)

(test (std-sheep-basic-structure
       :fixture allocate-std-sheep)
  (is (vectorp sheep))
  (is (= 6 (length sheep))))

(test (std-sheep-initial-values
       :depends-on std-sheep-basic-structure
       :fixture allocate-std-sheep)
  (is (eql =standard-metasheep= (svref sheep 0)))
  (is (null (svref sheep 1)))
  (is (null (svref sheep 2)))
  (is (null (svref sheep 3)))
  (is (null (svref sheep 4)))
  (is (null (svref sheep 5))))

(postboot-test allocate-sheep
  (let ((sheep (allocate-sheep =standard-metasheep=)))
    (is (vectorp sheep))
    (is (= 6 (length sheep)))
    (is (eql =standard-metasheep= (svref sheep 0)))
    (is (null (svref sheep 1)))
    (is (null (svref sheep 2)))
    (is (null (svref sheep 3)))
    (is (null (svref sheep 4)))
    (is (null (svref sheep 5)))))

(in-suite allocation)

(test (std-sheep-p :fixture allocate-std-sheep)
  (is (std-sheep-p sheep))
  (is (not (std-sheep-p (vector 1 2 3 4 5 6))))
  (is (not (std-sheep-p (vector 1 2))))
  (is (not (std-sheep-p (vector =standard-metasheep= 2 3 4 5))))
  (is (not (std-sheep-p (vector =standard-metasheep= 2 3 4 5 6 7))))
  (is (std-sheep-p (vector =standard-metasheep= nil nil nil nil nil)))
  (is (not (std-sheep-p (vector =standard-metasheep= nil nil nil nil))))
  (is (not (std-sheep-p (vector =standard-metasheep= nil nil nil nil nil nil))))
  (setf (%sheep-parents sheep) '(foo))
  (is (std-sheep-p sheep)))

(postboot-test (sheepp :fixture (allocate-sheep =standard-metasheep=))
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

(test (%sheep-metasheep :fixture allocate-std-sheep)
  (is (eql =standard-metasheep= (%sheep-metasheep sheep)))
  (is (null (setf (%sheep-metasheep sheep) nil)))
  (is (null (%sheep-metasheep sheep))))

(test (%sheep-parents :fixture allocate-std-sheep)
  (is (null (%sheep-parents sheep)))
  (is (equal '(foo) (setf (%sheep-parents sheep) '(foo))))
  (is (equal '(foo) (%sheep-parents sheep)))
  (is (equal '(bar foo) (push 'bar (%sheep-parents sheep))))
  (is (equal '(bar foo) (%sheep-parents sheep))))

(test (%sheep-direct-properties :fixture allocate-std-sheep)
  (is (null (%sheep-direct-properties sheep)))
  (is (equal '(foo) (setf (%sheep-direct-properties sheep) '(foo))))
  (is (equal '(foo) (%sheep-direct-properties sheep)))
  (is (equal '(bar foo) (push 'bar (%sheep-direct-properties sheep))))
  (is (equal '(bar foo) (%sheep-direct-properties sheep))))

(test (%sheep-roles :fixture allocate-std-sheep)
  (is (null (%sheep-roles sheep)))
  (is (equal '(foo) (setf (%sheep-roles sheep) '(foo))))
  (is (equal '(foo) (%sheep-roles sheep)))
  (is (equal '(bar foo) (push 'bar (%sheep-roles sheep))))
  (is (equal '(bar foo) (%sheep-roles sheep))))

(test (%sheep-hierarchy-cache :fixture allocate-std-sheep)
  (is (null (%sheep-hierarchy-cache sheep)))
  (is (equal '(foo) (setf (%sheep-hierarchy-cache sheep) '(foo))))
  (is (equal '(foo) (%sheep-hierarchy-cache sheep)))
  (is (equal '(bar foo) (push 'bar (%sheep-hierarchy-cache sheep))))
  (is (equal '(bar foo) (%sheep-hierarchy-cache sheep))))

(test (%sheep-children :fixture allocate-std-sheep)
  (is (null (%sheep-children sheep)))
  (is (equal '(foo) (setf (%sheep-children sheep) '(foo))))
  (is (equal '(foo) (%sheep-children sheep)))
  (is (equal '(bar foo) (push 'bar (%sheep-children sheep))))
  (is (equal '(bar foo) (%sheep-children sheep))))

(def-suite interface-accessors :in sheep-objects)
(in-suite interface-accessors)

(test (sheep-metasheep :fixture allocate-std-sheep)
  (is (eql =standard-metasheep= (sheep-metasheep sheep)))
  (is (null (fboundp '(setf sheep-metasheep)))))

(test (sheep-parents :fixture allocate-std-sheep)
  (is (null (sheep-parents sheep)))
  (setf (%sheep-parents sheep) '(foo))
  (is (equal '(foo) (sheep-parents sheep)))
  (push 'bar (%sheep-parents sheep))
  (is (equal '(bar foo) (sheep-parents sheep)))
  (is (null (fboundp '(setf sheep-parents)))))

(def-suite inheritance :in sheep-objects)
(def-suite inheritance-basic :in inheritance)
(in-suite inheritance-basic)

;;; <<<<<<< BEGIN OUTDATED CODE BLOCK >>>>>>>
(defun mapappend (fun &rest args)
  (if (some #'null args)
      ()
      (append (apply fun (mapcar #'car args))
              (apply #'mapappend fun (mapcar #'cdr args)))))

(defun topological-sort-old (elements constraints tie-breaker)
  (let ((remaining-constraints constraints)
        (remaining-elements elements)
        (result ()))
    (loop
       (let ((minimal-elements
              (remove-if
               (lambda (sheep)
                 (member sheep remaining-constraints
                         :key #'cadr))
               remaining-elements)))
         (when (null minimal-elements)
           (if (null remaining-elements)
               (return-from topological-sort-old result)
               (error "Inconsistent precedence graph.")))
         (let ((choice (if (null (cdr minimal-elements))
                           (car minimal-elements)
                           (funcall tie-breaker
                                    minimal-elements
                                    result))))
           (setf result (append result (list choice)))
           (setf remaining-elements
                 (remove choice remaining-elements))
           (setf remaining-constraints
                 (remove choice
                         remaining-constraints
                         :test #'member)))))))

(defun compute-sheep-hierarchy-list-old (sheep)
  (handler-case
      ;; since collect-ancestors only collects the _ancestors_, we cons the sheep in front.
      (let ((sheeple-to-order (cons sheep (collect-ancestors sheep))))
        (topological-sort sheeple-to-order
                          (remove-duplicates
                           (mapappend #'local-precedence-ordering
                                      sheeple-to-order))
                          #'std-tie-breaker-rule))
    (simple-error ()
      (error 'sheeple-hierarchy-error :sheep sheep))))

(defun local-precedence-ordering-old (sheep)
  (mapcar #'list
          (cons sheep
                (butlast (sheep-parents sheep)))
          (sheep-parents sheep)))
;;; <<<<<<< END OUTDATED CODE BLOCK >>>>>>>

(test collect-ancestors
  (let ((sheep1 (allocate-std-sheep))
        (sheep2 (allocate-std-sheep))
        (sheep3 (allocate-std-sheep)))
    (setf (%sheep-parents sheep1) (list sheep2))
    (setf (%sheep-parents sheep2) (list sheep3))
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
    (setf (%sheep-parents a) (list =standard-sheep=))
    (setf (%sheep-parents b) (list =standard-sheep=))
    (setf (%sheep-parents c) (list =standard-sheep=))
    (setf (%sheep-parents d) (list a b c))
    (is (equal (list (list d a) (list a b) (list b c))
               (local-precedence-ordering d)))))

(test std-tie-breaker-rule
  (let* ((a (allocate-std-sheep))
         (b (allocate-std-sheep))
         (c (allocate-std-sheep))
         (e (allocate-std-sheep))
         (f (allocate-std-sheep))
         (g (allocate-std-sheep)))
    (setf (%sheep-parents a) (list =standard-sheep=))
    (setf (%sheep-parents b) (list =standard-sheep=))
    (setf (%sheep-parents c) (list =standard-sheep=))
    (setf (%sheep-parents e) (list a))
    (setf (%sheep-parents f) (list b))
    (setf (%sheep-parents g) (list c))
    (is (eq c (std-tie-breaker-rule (list a b c) (list e f g))))
    (is (eq b (std-tie-breaker-rule (list a b c) (list e g f))))
    (is (eq a (std-tie-breaker-rule (list a b c) (list g f e))))))

(test compute-sheep-hierarchy-list
  (let ((parent (allocate-std-sheep))
        (child (allocate-std-sheep)))
    (setf (%sheep-parents child) (list parent))
    (is (equal (list child parent)
               (compute-sheep-hierarchy-list child))))
  (let ((a (allocate-std-sheep))
        (b (allocate-std-sheep))
        (c (allocate-std-sheep))
        (d (allocate-std-sheep))
        (e (allocate-std-sheep))
        (f (allocate-std-sheep))
        (g (allocate-std-sheep))
        (h (allocate-std-sheep)))
    (setf (%sheep-parents c) (list a))
    (setf (%sheep-parents d) (list a))
    (setf (%sheep-parents e) (list b c))
    (setf (%sheep-parents f) (list d))
    (setf (%sheep-parents g) (list c f))
    (setf (%sheep-parents h) (list g e))
    (is (equal (list h g e b c f d a) (compute-sheep-hierarchy-list h)))))

(def-suite inheritance-caching :in inheritance)
(in-suite inheritance-caching)

(test (%create-child-cache :fixture allocate-std-sheep)
  (%create-child-cache sheep)
  (is (typep (%sheep-children sheep) `(simple-vector ,*child-cache-initial-size*))))

(test (%child-cache-full-p :fixture allocate-std-sheep)
  (macrolet ((full (&body body)
               `(progn ,@body (is (%child-cache-full-p sheep))))
             (not-full (&body body)
               `(progn ,@body (is (not (%child-cache-full-p sheep))))))
    (not-full)
    (full (setf (%sheep-children sheep) (vector)))
    (not-full (setf (%sheep-children sheep) (vector nil)))
    (full (setf (%sheep-children sheep) (vector (make-weak-pointer T))))
    (not-full (setf (%sheep-children sheep) (vector (make-weak-pointer T) nil nil)))
    (full (setf (%sheep-children sheep)
                (apply #'vector
                       (make-list 3 :initial-element (make-weak-pointer T)))))))

(test (%enlarge-child-cache :fixture allocate-std-sheep)
  (%create-child-cache sheep)
  (%enlarge-child-cache sheep)
  (is (typep (%sheep-children sheep)
             `(simple-vector ,(* *child-cache-initial-size*
                                 *child-cache-grow-ratio*)))))

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
  (loop :repeat 10 :for sheep := (allocate-std-sheep)
     :do (loop :repeat *child-cache-initial-size*
            :do (%add-child (allocate-std-sheep) sheep)
            (loop :repeat 9999 :collect (list))
            :finally (gc :full t))
     (unless (every #'maybe-weak-pointer-value (%sheep-children sheep))
       (return
         (if (= *child-cache-initial-size*
                (length (%sheep-children (%add-child (allocate-std-sheep) sheep))))
             (pass "#'%ADD-CHILD overrode a garbage-collected child")
             (fail "#'%ADD-CHILD didn't override garbage-collected children"))))
     :finally (skip "Unable to perform test -- Insufficient garbage collection")))

(test (%remove-child :fixture allocate-std-sheep)
  (let ((child (allocate-std-sheep)))
    (is (eq sheep (%add-child child sheep)))
    (let ((original-children (%sheep-children sheep)))
      (is (eq sheep (%remove-child child sheep)))
      (is (eq original-children (%sheep-children sheep))))
    (is (null (find child (%sheep-children sheep) :key #'maybe-weak-pointer-value)))))

(test %map-children
  (let ((parent (allocate-std-sheep))
        (child1 (allocate-std-sheep))
        (child2 (allocate-std-sheep))
        (child3 (allocate-std-sheep)))
    (%add-child child1 parent)
    (%add-child child2 parent)
    (%add-child child3 parent)
    (%map-children (fun (setf (elt _ 0) nil)) parent)
    (is (null (elt child1 0)))
    (is (null (elt child2 0)))
    (is (null (elt child3 0)))))

(test memoize-sheep-hierarchy-list
  (let ((sheep1 (allocate-std-sheep))
        (sheep2 (allocate-std-sheep)))
    (setf (%sheep-parents sheep1) (list sheep2))
    (is (null (%sheep-hierarchy-cache sheep1)))
    (is (null (%sheep-hierarchy-cache sheep2)))
    (memoize-sheep-hierarchy-list sheep1)
    (is (equal (list sheep1 sheep2) (%sheep-hierarchy-cache sheep1)))))

(test std-finalize-sheep-inheritance
  (let ((sheep1 (allocate-std-sheep))
        (sheep2 (allocate-std-sheep)))
    (is (equal (list sheep2) (setf (%sheep-parents sheep1) (list sheep2))))
    (is (eql sheep1 (std-finalize-sheep-inheritance sheep1)))
    (is (find sheep2 (sheep-parents sheep1)))
    (is (find sheep2 (%sheep-hierarchy-cache sheep1)))))

;;; Testing the utility...
(in-suite sheep-objects)
(test with-sheep-hierarchy
  (with-sheep-hierarchy (a (b a) (c a) (d b c))
    (is (null (sheep-parents a)))
    (is (equal (list a) (sheep-parents b)))
    (is (equal (list a) (sheep-parents c)))
    (is (equal (list b c) (sheep-parents d))))
  (signals sheeple-hierarchy-error
    (with-sheep-hierarchy (a (b a) (c b) (d a c))
      (declare (ignore d)))))

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
    (is (eql sheep2 (car (%sheep-parents sheep1))))
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
;;; Spawning
;;;
(def-suite spawning :in sheeple)

(def-suite spawn-general :in spawning)
(in-suite spawn-general)

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

(postboot-test spawn
  (is (eql =standard-metasheep= (sheep-metasheep (spawn))))
  (is (sheepp (spawn)))
  (is (std-sheep-p (spawn)))
  (is (eql =standard-sheep= (car (sheep-parents (spawn)))))
  (let ((obj1 (spawn)))
    (is (eql obj1
             (car (sheep-parents (spawn obj1))))))
  (let* ((obj1 (spawn))
         (obj2 (spawn obj1)))
    (is (eql obj1
             (car (sheep-parents obj2)))))
  (let* ((o1 (spawn))
         (o2 (spawn))
         (o3 (spawn))
         (o4 (spawn))
         (sheep (spawn o1 o2 o3 o4)))
    (is (parentp o1 sheep))
    (is (parentp o2 sheep))
    (is (parentp o3 sheep))
    (is (parentp o4 sheep))))

(postboot-test sheep-nickname
  (let ((sheep (spawn)))
    (setf (sheep-nickname sheep) 'test)
    (is (eq 'test (sheep-nickname sheep)))
    (is (eq 'test (sheep-nickname (spawn sheep))))))

(postboot-test sheep-documentation
  (let ((sheep (spawn)))
    (setf (sheep-documentation sheep) 'test)
    (is (eq 'test (sheep-documentation sheep)))
    (is (eq 'test (sheep-documentation (spawn sheep))))))

(test copy-sheep
  ;; TODO - I don't even know if I want this. -- Sykopomp
  ;; Why not? -- Adlai
  )

;;;
;;; DEFSHEEP
;;;
(def-suite defsheep :in spawning)
(in-suite defsheep)

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

(test canonize-options
  (is (equal '(:metasheep foo :other-option 'bar)
             (canonize-options '((:metasheep foo) (:other-option 'bar))))))

(postboot-test defsheep
  (let* ((parent (spawn))
         (test-sheep (defsheep (parent) ((var "value")))))
    (is (sheepp test-sheep))
    (is (parentp parent test-sheep))
    (is (has-direct-property-p test-sheep 'var))
    ;; TODO - this should also check that reader/writer/accessor combinations are properly added
    ))

;;;
;;; Protos
;;;
(def-suite protos :in spawning)
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
