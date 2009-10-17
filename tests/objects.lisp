;;;; -*- Mode: lisp; indent-tabs-mode: nil -*-
;;;;
;;;; This file is part of Sheeple.

;;;; tests/objects.lisp
;;;;
;;;; Unit tests for src/objects.lisp
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :sheeple)

(def-fixture with-std-object ()
  (let ((object (std-allocate-object =standard-metaobject=)))
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
#+nil
(test object-printing
  (with-output-to-string (*standard-output*)
    (5am:finishes (print (std-allocate-object =standard-metaobject=)))
    (5am:finishes (print (add-parent (std-allocate-object =standard-metaobject=) (std-allocate-object =standard-metaobject=))))))

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
    (is (null (%object-mold object)))
    (is (null (%object-property-values object)))
    (is (null (%object-roles object)))))

#+nil
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

#+nil
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

(def-suite interface-accessors :in objects)
(in-suite interface-accessors)

(test (object-metaobject :fixture with-std-object)
  (is (eql =standard-metaobject= (object-metaobject object)))
  (is (null (fboundp '(setf object-metaobject)))))

(def-suite inheritance :in objects)
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

(test object-hierarchy-list
  (with-object-hierarchy (parent (child parent))
    (is (equal (list child parent =standard-object= =t=)
               (object-hierarchy-list child))))
  (with-object-hierarchy (a (b a) (c b))
    (is (equal (list c b a =standard-object= =t=)
               (object-hierarchy-list c))))
  (with-object-hierarchy (a b (c a) (d a) (e b c) (f d) (g c f) (h g e))
    (is (equal (list h g e b c f d a =standard-object= =t=)
               (object-hierarchy-list h)))))

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

(def-suite child-caching :in sheeple)
(in-suite child-caching)

(test cache-update-basic
  (with-object-hierarchy (a b (c a))
    (push b (object-parents a))
    (is (equal (list c a b =standard-object= =t=)
               (object-hierarchy-list c)))
    (setf (object-parents c) (list b))
    (is (equal (list c b =standard-object= =t=)
               (object-hierarchy-list c)))
    (push c (object-parents a))
    (is (equal (list a c b =standard-object= =t=)
               (object-hierarchy-list a)))))

(test cache-update-moderate
  (with-object-hierarchy (a (b a) (c a) (d b) (e c) x)
    (push x (object-parents a))
    (is (equal (list a x =standard-object= =t=)
               (object-hierarchy-list a)))
    (is (equal (list b a x =standard-object= =t=)
               (object-hierarchy-list b)))
    (is (equal (list c a x =standard-object= =t=)
               (object-hierarchy-list c)))
    (is (equal (list d b a x =standard-object= =t=)
               (object-hierarchy-list d)))
    (is (equal (list e c a x =standard-object= =t=)
               (object-hierarchy-list e)))))

(test cache-update-extensive
  (with-object-hierarchy (a b c d e f g h)
    (mapcar (lambda (parent object)
              (push parent (object-parents object)))
            (list g f e c d b a c a)
            (list h g h g f e c e d))
    (is (equal (list c a =standard-object= =t=)
               (object-hierarchy-list c)))
    (is (equal (list d a =standard-object= =t=)
               (object-hierarchy-list d)))
    (is (equal (list e c a b =standard-object= =t=)
               (object-hierarchy-list e)))
    (is (equal (list f d a =standard-object= =t=)
               (object-hierarchy-list f)))
    (is (equal (list g c f d a =standard-object= =t=)
               (object-hierarchy-list g)))
    (is (equal (list h e g c f d a b =standard-object= =t=)
               (object-hierarchy-list h)))))

(def-suite inheritance-predicates :in inheritance)
(in-suite inheritance-predicates)

(test parentp
  (let* ((a (spawn))
         (b (spawn a))
         (c (spawn b)))
    (is (parentp a b))
    (is (parentp b c))
    (is (not (parentp a c)))
    (is (not (parentp c a)))
    (is (not (parentp b a)))
    (is (not (parentp c b)))))

(test childp
  (let* ((a (spawn))
         (b (spawn a))
         (c (spawn b)))
    (is (childp b a))
    (is (childp c b))
    (is (not (childp c a)))
    (is (not (childp a c)))
    (is (not (childp a b)))
    (is (not (childp b c)))))

(test ancestorp
  (let* ((a (spawn))
         (b (spawn a))
         (c (spawn b)))
    (is (ancestorp a b))
    (is (ancestorp b c))
    (is (ancestorp a c))
    (is (not (ancestorp c a)))
    (is (not (ancestorp b a)))
    (is (not (ancestorp c b)))))

(test descendantp
  (let* ((a (spawn))
         (b (spawn a))
         (c (spawn b)))
    (is (descendantp b a))
    (is (descendantp c b))
    (is (descendantp c a))
    (is (not (descendantp a c)))
    (is (not (descendantp a b)))
    (is (not (descendantp b c)))))

;;;
;;; Spawning
;;;
(def-suite spawning :in objects)

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
