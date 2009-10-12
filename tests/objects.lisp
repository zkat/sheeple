;;;; -*- Mode: lisp; indent-tabs-mode: nil -*-
;;;;
;;;; This file is part of Sheeple.

;;;; tests/sheeple.lisp
;;;;
;;;; Unit tests for src/sheeple.lisp
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :sheeple)

(defun cons-std-object ()
  (finalize-object-inheritance (std-allocate-object =standard-metaobject=)))

(def-fixture with-std-object ()
  (let ((object (cons-std-object)))
    (&body)))

(def-fixture allocate-object (metaobject)
  (let ((object (allocate-object metaobject)))
    (&body)))

(def-suite objects :in sheeple)

;;;
;;; Early printing
;;;

(def-suite early-printing :in objects)
(in-suite early-printing)

;;; This test is pretty crude. The correct thing to do would be to
;;; parse the output, extract the implementation-dependant stuff,
;;; and verify the rest. This would be painful without CL-PPCRE.
(test object-printing
  (with-output-to-string (*standard-output*)
    (5am:finishes (print (cons-std-object)))
    (5am:finishes (print (add-parent (cons-std-object) (cons-std-object))))))

;;;
;;; Allocation
;;;
;;; new structure for sheep objects:
;;; #(metaobject parents properties roles %hierarchy-cache %children)
;;; parents := proper list of parents, in order of precedence.
;;; properties := property*
;;; property := (property-metaobject . property-value)
;;; roles := proper list of direct roles
;;; %hierarchy-cache := list that holds a cached version of the object's entire hierarchy-list
;;; %children := a vector of weak pointers that points to all of this object's children

(def-suite creation :in objects)
(def-suite allocation :in creation)

(def-suite std-allocate-object :in allocation)
(in-suite std-allocate-object)

(test (std-object-basic-structure :fixture with-std-object)
  (is (typep object 'structure-object))
  (is (typep object 'object)))

(test std-object-initial-values
  (let ((object (std-allocate-object =standard-metaobject=)))
    (is (eq =standard-metaobject= (%object-metaobject object)))
    (is (eq nil (%object-parents object)))
    (is (eq nil (%object-properties object)))
    (is (eq nil (%object-roles object)))
    (is (eq nil (%object-hierarchy-cache object)))
    (is (eq nil (%object-children object)))))

(test allocate-object
  (let ((object (allocate-object =standard-metaobject=)))
    (is (eq =standard-metaobject= (%object-metaobject object)))
    (is (eq nil (%object-parents object)))
    (is (eq nil (%object-properties object)))
    (is (eq nil (%object-roles object)))
    (is (eq nil (%object-hierarchy-cache object)))
    (is (eq nil (%object-children object)))))

(in-suite allocation)

(test std-object-p
  (for-all ((object (fun (std-allocate-object (funcall (gen-integer))))))
    (is (not (std-object-p object))))
  (for-all ((object (fun (std-allocate-object =standard-metaobject=))))
    (is (std-object-p object))))

(test (objectp :fixture (allocate-object =standard-metaobject=))
  (is (objectp object)))

(test (equality-basic :fixture with-std-object)
  (is (eq object object))
  (is (eql object object))
  (5am:finishes                         ; Does the heap blow up?
    (equal object object)
    (equalp object object)))

(def-suite low-level-accessors :in objects)
(in-suite low-level-accessors)

(test (%object-metaobject :fixture with-std-object)
  (is (eql =standard-metaobject= (%object-metaobject object)))
  (is (null (setf (%object-metaobject object) nil)))
  (is (null (%object-metaobject object))))

(test (%object-parents :fixture with-std-object)
  (is (null (%object-parents object)))
  (is (equal '(foo) (setf (%object-parents object) '(foo))))
  (is (equal '(foo) (%object-parents object)))
  (is (equal '(bar foo) (push 'bar (%object-parents object))))
  (is (equal '(bar foo) (%object-parents object))))

(test (%object-properties :fixture with-std-object)
  (is (null (%object-properties object)))
  (is (equal '(foo) (setf (%object-properties object) '(foo))))
  (is (equal '(foo) (%object-properties object)))
  (is (equal '(bar foo) (push 'bar (%object-properties object))))
  (is (equal '(bar foo) (%object-properties object))))

(test (%object-roles :fixture with-std-object)
  (is (null (%object-roles object)))
  (is (equal '(foo) (setf (%object-roles object) '(foo))))
  (is (equal '(foo) (%object-roles object)))
  (is (equal '(bar foo) (push 'bar (%object-roles object))))
  (is (equal '(bar foo) (%object-roles object))))

(test (%object-hierarchy-cache :fixture with-std-object)
  (is (equal (list object) (%object-hierarchy-cache object)))
  (is (equal '(foo) (setf (%object-hierarchy-cache object) '(foo))))
  (is (equal '(foo) (%object-hierarchy-cache object)))
  (is (equal '(bar foo) (push 'bar (%object-hierarchy-cache object))))
  (is (equal '(bar foo) (%object-hierarchy-cache object))))

(test (%object-children :fixture with-std-object)
  (is (null (%object-children object)))
  (is (equal '(foo) (setf (%object-children object) '(foo))))
  (is (equal '(foo) (%object-children object)))
  (is (equal '(bar foo) (push 'bar (%object-children object))))
  (is (equal '(bar foo) (%object-children object))))

(def-suite interface-accessors :in objects)
(in-suite interface-accessors)

(test (object-metaobject :fixture with-std-object)
  (is (eql =standard-metaobject= (object-metaobject object)))
  (is (null (fboundp '(setf object-metaobject)))))

(test (object-parents :fixture with-std-object)
  (is (null (object-parents object)))
  (setf (%object-parents object) '(foo))
  (is (equal '(foo) (object-parents object)))
  (push 'bar (%object-parents object))
  (is (equal '(bar foo) (object-parents object)))
  (is (null (fboundp '(setf object-parents)))))

(def-suite inheritance :in object-objects)
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
               (lambda (object)
                 (member object remaining-constraints
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

(defun compute-object-hierarchy-list-old (object)
  (handler-case
      ;; since collect-ancestors only collects the _ancestors_, we cons the object in front.
      (let ((objects-to-order (cons object (collect-ancestors-old object))))
        (topological-sort-old objects-to-order
                              (remove-duplicates
                               (mapappend #'local-precedence-ordering-old
                                          objects-to-order))
                              #'std-tie-breaker-rule-old))
    (simple-error ()
      (error 'object-hierarchy-error :object object))))

(defun collect-ancestors-old (object)
  "Recursively collects all of OBJECT's ancestors."
  (labels ((all-parents-loop (seen parents)
              (let ((to-be-processed
                     (set-difference parents seen)))
                (if (null to-be-processed)
                    parents
                    (let ((object-to-process
                           (car to-be-processed)))
                      (all-parents-loop
                       (cons object-to-process seen)
                       (union (object-parents object-to-process)
                              parents)))))))
    (all-parents-loop () (object-parents object))))

(defun std-tie-breaker-rule-old (minimal-elements hl-so-far)
  (dolist (hl-constituent (reverse hl-so-far))
    (let* ((supers (object-parents hl-constituent))
           (common (intersection minimal-elements supers)))
      (when (not (null common))
        (return-from std-tie-breaker-rule-old (car common))))))

(defun local-precedence-ordering-old (object)
  (mapcar #'list
          (cons object
                (butlast (object-parents object)))
          (object-parents object)))
;;; <<<<<<< END OUTDATED CODE BLOCK >>>>>>>

(test collect-ancestors
  (with-object-hierarchy (a (b a) (c b))
    (is (find b (collect-ancestors c)))
    (is (find a (collect-ancestors b)))
    (is (find a (collect-ancestors c)))
    (is (not (find c (collect-ancestors c))))
    (is (not (find c (collect-ancestors b))))))

(test local-precedence-ordering
  (with-object-hierarchy (a b c (d a b c))
    (is (equal (acons d a (acons a b (acons b c nil)))
               (local-precedence-ordering d)))))

;;; I'm gonna stop pretending as though I have a clue
;;; how to test what this actually SHOULD do
(test std-tie-breaker-rule)

(test compute-object-hierarchy-list
  (with-object-hierarchy (parent (child parent))
    (is (equal (list child parent =standard-object= =t=)
               (compute-object-hierarchy-list child))))
  (with-object-hierarchy (a b (c a) (d a) (e b c) (f d) (g c f) (h g e))
    (is (equal (list h g e b c f d a =standard-object= =t=)
               (compute-object-hierarchy-list h)))))

(def-suite inheritance-caching :in inheritance)
(in-suite inheritance-caching)

(test (%create-child-cache :fixture with-std-object)
  (%create-child-cache object)
  (is (typep (%object-children object) `(simple-vector ,*child-cache-initial-size*))))

(test (%child-cache-full-p :fixture with-std-object)
  (macrolet ((full (&body body)
               `(progn ,@body (is (%child-cache-full-p object))))
             (not-full (&body body)
               `(progn ,@body (is (not (%child-cache-full-p object))))))
    (not-full)
    (full (setf (%object-children object) (vector)))
    (not-full (setf (%object-children object) (vector nil)))
    (full (setf (%object-children object) (vector (make-weak-pointer T))))
    (not-full (setf (%object-children object) (vector (make-weak-pointer T) nil nil)))
    (full (setf (%object-children object)
                (apply #'vector
                       (make-list 3 :initial-element (make-weak-pointer T)))))))

(test (%enlarge-child-cache :fixture with-std-object)
  (%create-child-cache object)
  (%enlarge-child-cache object)
  (is (typep (%object-children object)
             `(simple-vector ,(* *child-cache-initial-size*
                                 *child-cache-grow-ratio*)))))

(test %add-child
  (let ((object1 (cons-std-object))
        (object2 (cons-std-object)))
    (is (null (%object-children object1)))
    (is (null (%object-children object2)))
    (is (eql object1 (%add-child object2 object1)))
    (is (null (%object-children object2)))
    (is (simple-vector-p (%object-children object1)))
    (is (find object2 (%object-children object1) :key #'maybe-weak-pointer-value))))

(test (overwriting-garbage-children :depends-on %add-child)
  (loop :repeat 10 :for object := (cons-std-object)
     :do (loop :repeat *child-cache-initial-size*
            :do (%add-child (cons-std-object) object)
            (loop :repeat 9999 :collect (list))
            :finally (gc :full t))
     (unless (every #'maybe-weak-pointer-value (%object-children object))
       (return
         (if (= *child-cache-initial-size*
                (length (%object-children (%add-child (cons-std-object) object))))
             (pass "#'%ADD-CHILD overrode a garbage-collected child")
             (fail "#'%ADD-CHILD didn't override garbage-collected children"))))
     :finally (skip "Unable to perform test -- Insufficient garbage collection")))

(test (%remove-child :fixture with-std-object)
  (let ((child (cons-std-object)))
    (is (eq object (%add-child child object)))
    (let ((original-children (%object-children object)))
      (is (eq object (%remove-child child object)))
      (is (eq original-children (%object-children object))))
    (is (null (find child (%object-children object) :key #'maybe-weak-pointer-value)))))

(test %map-children
  (let* ((parent (cons-std-object))
         (child1 (cons-std-object))
         (child2 (cons-std-object))
         (list   (list child1 child2)))
    (%add-child child1 parent)
    (%add-child child2 parent)
    (%map-children (fun (deletef list _)) parent)
    (is (null list))))

(test memoize-object-hierarchy-list
  (let ((object1 (cons-std-object))
        (object2 (cons-std-object)))
    (setf (%object-parents object1) (list object2))
    (memoize-object-hierarchy-list object1)
    (is (equal (list object1 object2) (%object-hierarchy-cache object1)))))

(test std-finalize-object-inheritance
  (let ((object1 (cons-std-object))
        (object2 (cons-std-object)))
    (is (equal (list object2) (setf (%object-parents object1) (list object2))))
    (is (eql object1 (std-finalize-object-inheritance object1)))
    (is (find object2 (object-parents object1)))
    (is (find object2 (%object-hierarchy-cache object1)))))

;;; Testing the utility...
(in-suite objects)
(test with-object-hierarchy
  (with-object-hierarchy (a (b a) (c a) (d b c))
    (is (eq =standard-object= (car (object-parents a))))
    (is (eq a (car (object-parents b))))
    (is (eq a (car (object-parents c))))
    (is (eq b (car (object-parents d))))
    (is (eq c (cadr (object-parents d)))))
  (signals object-hierarchy-error
    (with-object-hierarchy (a (b a) (c b) (d a c))
      (declare (ignore d)))))

(test finalize-object-inheritance
  ;; todo - write tests for this once bootstrapping is set...
)

(def-suite add/remove-parents :in inheritance)
(in-suite add/remove-parents)

(test remove-parent
  (let ((object1 (cons-std-object))
        (object2 (cons-std-object)))
    (add-parent object2 object1)
    (is (eql object2 (car (object-parents object1))))
    (signals error (remove-parent object1 object1))
    (is (eql object1 (remove-parent object2 object1)))
    (is (null (object-parents object1)))
    (signals error (remove-parent object2 object1))
    (signals error (remove-parent object1 object1)))
  ;; todo - more thorough tests, post-bootstrap
)

(test std-remove-parent
  (let ((object1 (cons-std-object))
        (object2 (cons-std-object)))
    (std-add-parent object2 object1)
    (is (eql object2 (car (%object-parents object1))))
    (signals error (std-remove-parent object1 object1))
    (is (eql object1 (std-remove-parent object2 object1)))
    (is (null (object-parents object1)))
    (signals error (std-remove-parent object2 object1))
    (signals error (std-remove-parent object1 object1))))

(test std-add-parent
  (let ((object1 (cons-std-object))
        (object2 (cons-std-object)))
    (is (eql object1 (std-add-parent object2 object1)))
    (is (eql object2 (car (object-parents object1))))
    (signals error (std-add-parent object1 object1))
    (signals error (std-add-parent object2 object1))
    (signals object-hierarchy-error (std-add-parent object1 object2))))

(test add-parent
  (let ((object1 (cons-std-object))
        (object2 (cons-std-object)))
    (is (eql object1 (add-parent object2 object1)))
    (is (eql object2 (car (object-parents object1))))
    (signals error (add-parent object1 object1))
    (signals error (add-parent object2 object1))
    (signals object-hierarchy-error (add-parent object1 object2)))
  ;; todo - more thorough tests post-bootstrap
  )

(test add-parents
  (let ((a (cons-std-object))
        (b (cons-std-object))
        (c (cons-std-object)))
    (is (eq c (add-parents (list a b) c)))
    (is (equal (list a b) (object-parents c)))))

(test object-hierarchy-list
  (let ((a (cons-std-object))
        (b (cons-std-object))
        (c (cons-std-object)))
    (is (eql a (add-parent b a)))
    (is (eql b (add-parent c b)))
    (is (equal (list a b c) (object-hierarchy-list a)))))

(def-suite inheritance-predicates :in inheritance)
(in-suite inheritance-predicates)

(test parentp
  (let ((a (cons-std-object))
        (b (cons-std-object))
        (c (cons-std-object)))
    (add-parent a b)
    (add-parent b c)
    (is (parentp a b))
    (is (parentp b c))
    (is (not (parentp a c)))
    (is (not (parentp c a)))
    (is (not (parentp b a)))
    (is (not (parentp c b)))))

(test childp
  (let ((a (cons-std-object))
        (b (cons-std-object))
        (c (cons-std-object)))
    (add-parent a b)
    (add-parent b c)
    (is (childp b a))
    (is (childp c b))
    (is (not (childp c a)))
    (is (not (childp a c)))
    (is (not (childp a b)))
    (is (not (childp b c)))))

(test ancestorp
  (let ((a (cons-std-object))
        (b (cons-std-object))
        (c (cons-std-object)))
    (add-parent a b)
    (add-parent b c)
    (is (ancestorp a b))
    (is (ancestorp b c))
    (is (ancestorp a c))
    (is (not (ancestorp c a)))
    (is (not (ancestorp b a)))
    (is (not (ancestorp c b)))))

(test descendantp
  (let ((a (cons-std-object))
        (b (cons-std-object))
        (c (cons-std-object)))
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
(def-suite spawning :in objectle)

(def-suite spawn-general :in spawning)
(in-suite spawn-general)

(test make-object
  ;; basic
  (let ((object (make-object nil)))
    (is (objectp object))
    (is (std-object-p object))
    (is (eql =standard-object= (car (object-parents object))))
    (is (eql object (car (object-parents (make-object (list object))))))
    (is (eql =standard-metaobject= (object-metaobject object))))
  ;; properties arg
  (let ((object (make-object nil :properties '((foo bar) (baz quux)))))
    (is (has-direct-property-p object 'foo))
    (is (has-direct-property-p object 'baz))
    (is (eql 'bar (direct-property-value object 'foo)))
    (is (eql 'quux (direct-property-value object 'baz))))
  #+ (or) ;; other metaobject -- Expected failure, left out of v3.0
  (let* ((test-metaobject (make-object =standard-metaobject= :nickname 'test-metaobject))
         (object (make-object nil :metaobject test-metaobject)))
    ;; metaobject tests
    (is (objectp test-metaobject))
    (is (std-object-p test-metaobject))
    (is (eql =standard-metaobject= (object-metaobject test-metaobject)))
    ;; object tests
    (is (objectp object))
    (is (not (std-object-p object)))
    (is (eql test-metaobject (object-metaobject object)))
    (is (eql =standard-object= (car (object-parents object))))))

(test spawn
  (is (eql =standard-metaobject= (object-metaobject (spawn))))
  (is (objectp (spawn)))
  (is (std-object-p (spawn)))
  (is (eql =standard-object= (car (object-parents (spawn)))))
  (let ((obj1 (spawn)))
    (is (eql obj1
             (car (object-parents (spawn obj1))))))
  (let* ((obj1 (spawn))
         (obj2 (spawn obj1)))
    (is (eql obj1
             (car (object-parents obj2)))))
  (let* ((o1 (spawn))
         (o2 (spawn))
         (o3 (spawn))
         (o4 (spawn))
         (object (spawn o1 o2 o3 o4)))
    (is (parentp o1 object))
    (is (parentp o2 object))
    (is (parentp o3 object))
    (is (parentp o4 object))))

(test clone)

#+nil
(test object-nickname
  (let ((object (spawn)))
    (setf (object-nickname object) 'test)
    (is (eq 'test (object-nickname object)))
    (is (eq 'test (object-nickname (spawn object))))))

#+nil
(test object-documentation
  (let ((object (spawn)))
    (setf (object-documentation object) 'test)
    (is (eq 'test (object-documentation object)))
    (is (eq 'test (object-documentation (spawn object))))))

;;;
;;; DEFOBJECT
;;;
(def-suite defobject :in spawning)
(in-suite defobject)

;;; macro processing
(test canonize-objects
  (is (equal '(list foo bar baz) (canonize-objects '(foo bar baz))))
  (is (equal '(list) (canonize-objects '()))))

(test canonize-property
  (is (equal '(list 'VAR "value") (canonize-property '(var "value"))))
  (is (equal '(list 'VAR "value" :accessor 'var)
             (canonize-property '(var "value") t)))
  (is (equal '(list 'VAR "value" :reader nil :accessor 'var)
             (canonize-property '(var "value" :reader nil) t)))
  (is (equal '(list 'VAR "value" :writer nil :accessor 'var)
             (canonize-property '(var "value" :writer nil) t)))
  (is (equal '(list 'VAR "value" :accessor nil)
             (canonize-property '(var "value" :accessor nil) t))))

(test canonize-properties
  (is (equal '(list (list 'VAR "value")) (canonize-properties '((var "value")))))
  (is (equal '(list (list 'VAR "value") (list 'ANOTHER "another-val"))
             (canonize-properties '((var "value") (another "another-val")))))
  (is (equal '(list (list 'VAR "value" :accessor 'var)
               (list 'ANOTHER "another-val" :accessor 'another))
             (canonize-properties '((var "value") (another "another-val")) t))))

(test canonize-options
  (is (equal '(:metaobject foo :other-option 'bar)
             (canonize-options '((:metaobject foo) (:other-option 'bar))))))

(test defobject
  (let* ((parent (spawn))
         (test-object (defobject (parent) ((var "value")))))
    (is (objectp test-object))
    (is (parentp parent test-object))
    (is (has-direct-property-p test-object 'var))
    ;; TODO - this should also check that reader/writer/accessor combinations are properly added
    ))

;;;
;;; Protos
;;;
(def-suite protos :in spawning)
(in-suite protos)

(test defproto
  (let ((test-proto (defproto =test-proto= () ((var "value")))))
    (is (objectp test-proto))
    (is (eql test-proto (symbol-value '=test-proto=)))
    (is (eql =standard-object= (car (object-parents test-proto))))
    (is (objectp (symbol-value '=test-proto=)))
    (is (equal "value" (funcall 'var (symbol-value '=test-proto=))))
    (defproto =test-proto= () ((something-else "another-one")))
    (is (eql test-proto (symbol-value '=test-proto=)))
    (is (eql =standard-object= (car (object-parents test-proto))))
    (signals unbound-property (direct-property-value test-proto 'var))
    (is (equal "another-one" (funcall 'something-else (symbol-value '=test-proto=))))
    (is (equal "another-one" (funcall 'something-else test-proto)))
    ;; TODO - check that options work properly
    (undefreply var ((object test-proto)))
    (undefreply something-else ((object test-proto)))
    (makunbound '=test-proto=)
    ))
(test ensure-object)
