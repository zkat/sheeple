;;;; -*- Mode: lisp; indent-tabs-mode: nil -*-
;;;;
;;;; This file is part of Sheeple.

;;;; tests/objects.lisp
;;;;
;;;; Unit tests for src/objects.lisp
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :sheeple)

(defmacro with-std-object (name &body body)
  `(let ((,name (std-allocate-object =standard-metaobject=))) ,@body))

(defmacro with-object ((name &optional (metaobject '=standard-metaobject=)) &body body)
  `(let ((,name (smop:allocate-object ,metaobject))) ,@body))

;;; For lack of a better place, here comes the macro extraordinaire!
(defmacro with-test-message (message-name &body body)
  "Ensures that the message MESSAGE-NAME will be cleaned up after the BODY runs.
Also, sets up a local macro of the same name for invoking the message. This sounds
confusing, but actually enables crystal clear warning-free test code."
  `(unwind-protect
        (macrolet ((,message-name (&rest args)
                     `(funcall (symbol-function ',',message-name) ,@args)))
          ,@body)
     (undefmessage ,message-name)))

(def-suite objects :in sheeple)
;;;
;;; Allocation
;;;
;;; new structure for sheep objects:
;;; #(mold metaobject properties-values roles)
;;; mold := pointer to the appropriate mold for this object
;;; metaobject := pointer to the appropriate metaobject
;;; property-values := vector of direct property values
;;; roles := proper list of direct roles
(def-suite creation :in objects)
(def-suite allocation :in creation)

(def-suite std-allocate-object :in allocation)
(in-suite std-allocate-object)

(test std-object-basic-structure
  (let ((object (std-allocate-object =standard-metaobject=)))
    (is (typep object 'structure-object))
    (is (typep object 'object))))

(test std-object-initial-values
  (let ((object (std-allocate-object =standard-metaobject=)))
    (is (eq =standard-metaobject= (%object-metaobject object)))
    (is (null (%object-property-values object)))
    (is (null (%object-roles object)))))

(test allocate-object
  (let ((object (smop:allocate-object =standard-metaobject=)))
    (is (eq =standard-metaobject= (object-metaobject object)))
    (is (eq nil (object-parents object)))))

(in-suite allocation)

(test std-object-p
  (is (std-object-p (smop:allocate-object =standard-metaobject=)))
  (is (not (std-object-p (smop:allocate-object (clone =standard-metaobject=))))))

(test objectp
  (with-object (object)
    (is (objectp object))))

(test equality-basic
  (with-std-object object
    (is (eq object object))
    (is (eql object object))
    (Eos:finishes                       ; Does the heap blow up?
      (equal object object)
      (equalp object object))))

(def-suite low-level-accessors :in objects)
(in-suite low-level-accessors)

(test %object-metaobject
  (with-std-object object
    (is (eql =standard-metaobject= (%object-metaobject object)))))

(test %object-property-values
  (with-std-object object
    (is (null (%object-property-values object)))
    (is (equal 'test (setf (%object-property-values object) 'test)))
    (is (equal 'test (%object-property-values object)))))

(test %object-roles
  (with-std-object object
    (is (null (%object-roles object)))
    (is (equal '(foo) (setf (%object-roles object) '(foo))))
    (is (equal '(foo) (%object-roles object)))))

(def-suite interface-accessors :in objects)
(in-suite interface-accessors)

(test object-metaobject
  (with-std-object object
    (is (eql =standard-metaobject= (object-metaobject object)))))

(def-suite inheritance :in objects)
(def-suite inheritance-basic :in inheritance)
(in-suite inheritance-basic)

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

(test object-precedence-list
  (with-object-hierarchy (parent (child parent))
    (is (equal (list child parent =standard-object= =t=)
               (object-precedence-list child))))
  (with-object-hierarchy (a (b a) (c b))
    (is (equal (list c b a =standard-object= =t=)
               (object-precedence-list c))))
  (with-object-hierarchy (a b (c a) (d a) (e b c) (f d) (g c f) (h g e))
    (is (equal (list h g e b c f d a =standard-object= =t=)
               (object-precedence-list h)))))

;;; Testing the utility...
(in-suite objects)
(test with-object-hierarchy
  (with-object-hierarchy (a (b a) (c a) (d b c))
    (is (eq =standard-object= (car (object-parents a))))
    (is (eq a (car (object-parents b))))
    (is (eq a (car (object-parents c))))
    (is (eq b (car (object-parents d))))
    (is (eq c (cadr (object-parents d)))))
  (signals object-precedence-error
    (with-object-hierarchy (a (b a) (c b) (d a c))
      (declare (ignore d)))))

(def-suite child-caching :in sheeple)
(in-suite child-caching)

(test cache-update-basic
  (with-object-hierarchy (a b (c a))
    (push b (object-parents a))
    (is (equal (list c a b =standard-object= =t=)
               (object-precedence-list c)))
    (setf (object-parents c) (list b))
    (is (equal (list c b =standard-object= =t=)
               (object-precedence-list c)))
    (push c (object-parents a))
    (is (equal (list a c b =standard-object= =t=)
               (object-precedence-list a)))))

(test cache-update-moderate
  (with-object-hierarchy (a (b a) (c a) (d b) (e c) x)
    (push x (object-parents a))
    (is (equal (list a x =standard-object= =t=)
               (object-precedence-list a)))
    (is (equal (list b a x =standard-object= =t=)
               (object-precedence-list b)))
    (is (equal (list c a x =standard-object= =t=)
               (object-precedence-list c)))
    (is (equal (list d b a x =standard-object= =t=)
               (object-precedence-list d)))
    (is (equal (list e c a x =standard-object= =t=)
               (object-precedence-list e)))))

(test cache-update-extensive
  (with-object-hierarchy (a b c d e f g h)
    (mapcar (lambda (parent object)
              (push parent (object-parents object)))
            (list g f e c d b a c a)
            (list h g h g f e c e d))
    (is (equal (list c a =standard-object= =t=)
               (object-precedence-list c)))
    (is (equal (list d a =standard-object= =t=)
               (object-precedence-list d)))
    (is (equal (list e c a b =standard-object= =t=)
               (object-precedence-list e)))
    (is (equal (list f d a =standard-object= =t=)
               (object-precedence-list f)))
    (is (equal (list g c f d a =standard-object= =t=)
               (object-precedence-list g)))
    (is (equal (list h e g c b f d a =standard-object= =t=)
               (object-precedence-list h)))))

(def-suite inheritance-predicates :in inheritance)
(in-suite inheritance-predicates)

(test parentp
  (let* ((a (object))
         (b (object :parents (list a)))
         (c (object :parents (list b))))
    (is (parentp a b))
    (is (parentp b c))
    (is (not (parentp a c)))
    (is (not (parentp c a)))
    (is (not (parentp b a)))
    (is (not (parentp c b)))))

(test childp
  (let* ((a (object))
         (b (object :parents (list a)))
         (c (object :parents (list b))))
    (is (childp b a))
    (is (childp c b))
    (is (not (childp c a)))
    (is (not (childp a c)))
    (is (not (childp a b)))
    (is (not (childp b c)))))

(test ancestorp
  (let* ((a (object))
         (b (object :parents (list a)))
         (c (object :parents (list b))))
    (is (ancestorp a b))
    (is (ancestorp b c))
    (is (ancestorp a c))
    (is (not (ancestorp c a)))
    (is (not (ancestorp b a)))
    (is (not (ancestorp c b)))))

(test descendantp
  (let* ((a (object))
         (b (object :parents (list a)))
         (c (object :parents (list b))))
    (is (descendantp b a))
    (is (descendantp c b))
    (is (descendantp c a))
    (is (not (descendantp a c)))
    (is (not (descendantp a b)))
    (is (not (descendantp b c)))))

(in-suite creation)
(test object
  ;; basic
  (let ((object (object)))
    (is (objectp object))
    (is (std-object-p object))
    (is (eql =standard-object= (car (object-parents object))))
    (is (eql object (car (object-parents (object :parents (list object))))))
    (is (eql =standard-metaobject= (object-metaobject object))))
  ;; properties arg
  (let ((object (object :properties '((foo bar) (baz quux)))))
    (is (direct-property-p object 'foo))
    (is (direct-property-p object 'baz))
    (is (eql 'bar (direct-property-value object 'foo)))
    (is (eql 'quux (direct-property-value object 'baz))))
  (let* ((test-metaobject (object :parents (list =standard-metaobject=) :nickname 'test-metaobject))
         (object (object :metaobject test-metaobject)))
    ;; metaobject tests
    (is (objectp test-metaobject))
    (is (std-object-p test-metaobject))
    (is (eql =standard-metaobject= (object-metaobject test-metaobject)))
    ;; object tests
    (is (objectp object))
    (is (not (std-object-p object)))
    (is (eql test-metaobject (object-metaobject object)))
    (is (eql =standard-object= (car (object-parents object))))))

(test object-nickname
  (let ((object (object)))
    (setf (object-nickname object) 'test)
    (is (eq 'test (object-nickname object)))
    (is (eq 'test (object-nickname (object :parents (list object)))))))

(test clone
  (let ((obj (object)))
    (let ((clone (clone obj)))
      (is (objectp clone))
      (is (null (%object-property-values clone)))
      (is (null (%object-roles clone)))
      (is (equal (list =standard-object=) (object-parents clone)))
      (setf (property-value obj 'test) 'test)
      (let ((clone2 (clone obj)))
        (is (objectp clone2))
        (is (null (direct-property-p clone 'test)))
        (is (not (null (direct-property-p clone2 'test))))
        (is (eq 'test (direct-property-value clone2 'test))))
      (with-test-message clone-test
        (defmessage clone-test (thingy)
          (:reply ((xyzzy obj)) xyzzy))
        (let ((clone3 (clone obj)))
          (is (eq clone3 (clone-test clone3)))
          (signals no-applicable-reply (clone-test clone)))))))

;;;
;;; DEFOBJECT
;;;
(def-suite defobject :in creation)
(in-suite defobject)

;;; macro processing
(test canonize-parents
  (is (equal '(list foo bar baz) (canonize-parents '(foo bar baz))))
  (is (equal '(list) (canonize-parents '())))
  (is (equal '(list foo) (canonize-parents 'foo))))

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
  (is (equal '(:metaobject foo :nickname 'bar)
             (canonize-options '(:metaobject foo :nickname 'bar))))
  (is (equal '(a b c d) (canonize-options '(a b c d))))
  (signals deprecated-feature (canonize-options '((|deprecated feature|))))
  (handler-bind
      ((deprecated-feature #'muffle-warning))
    (is (equal '(:metaobject foo :nickname 'bar)
               (canonize-options '((:metaobject foo) (:nickname 'bar)))))
    (is (equal '('a '(b)) (canonize-options '((a b)))))))

(test defobject
  (let* ((parent (object))
         (test-object (defobject (parent) ((var "value")))))
    (is (objectp test-object))
    (is (parentp parent test-object))
    (is (direct-property-p test-object 'var))
    ;; TODO - this should also check that reader/writer/accessor combinations are properly added
    ))

;;;
;;; Protos
;;;
(def-suite protos :in creation)
(in-suite protos)

(test nonexistant-prototype
  (let ((name (gensym)))
    (handler-case (proto name)
      (nonexistant-prototype (condition)
        (let ((cname (nonexistant-prototype-name condition)))
          (is (eq name cname)
              ;; FIXME: I'm ugly-printed. Prettify me!
              "Signaled condition was named ~S (instead of ~S)." cname name)))
      (:no-error (proto)
        (fail "Bogus proto ~S returned instead of an error." proto)))))

(test defproto
  ;; FIXME: I'm ugly.
  (macrolet ((test ()
               (with-gensyms (name)
                 `(symbol-macrolet ((,name (proto ',name))) ; FIXME: This is cheating
                    (with-test-message var
                      (with-test-message something-else
                        (let ((test-proto (defproto ,name () ((var "value")))))
                          (is (objectp test-proto))
                          (is (eql test-proto ,name))
                          (is (eql =standard-object= (car (object-parents test-proto))))
                          (is (objectp ,name))
                          (is (equal "value" (var ,name)))
                          (defproto ,name () ((something-else "another-one")))
                          (is (eql test-proto ,name))
                          (is (eql =standard-object= (car (object-parents test-proto))))
                          (signals unbound-property (direct-property-value test-proto 'var))
                          (is (equal "another-one" (something-else ,name)))
                          (is (equal "another-one" (something-else test-proto)))
                          ;; TODO - check that options work properly
                          (undefreply var (test-proto))
                          (undefreply something-else (test-proto)))))))))
    (test)))
(test ensure-object)
