;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; indent-tabs-mode: nil -*-
;;;; This is a little demo put together to give a basic introduction of the Sheeple API
(in-package :sheeple-user)

;;;
;;; Basic object creation
;;;
(defparameter *obj* (object :nickname '*obj*)
  "Create an empty object. This object only has =standard-object= as its parent.")

(defparameter *child* (object :parents (list *obj*) :nickname '*child*)
  "This creates a new object which delegates to *obj*.")

(defparameter *clone* (clone *obj*)
  "This is the other method of creating new objects. Instead of establishing a relationship,
the original object is simply copied. All properties and roles are (shallow-)copied locally.")

;; What's the difference?

;; *clone* won't be affected by changes to *obj*. *child* will:
(setf (property-value *obj* 'var) "value")

(describe *obj*)
(describe *child*)
(describe *clone*)

;; In a diagram:
;;          =T=
;;           |
;;    =standard-object=
;;        |         \
;;      *obj* ->  *clone* (properties copied directly, delegation is to *obj*'s parents.
;;        |
;;     *child* (nothing copied locally. All behavior not defined locally delegated to *obj*

;; Sheeple implements multiple inheritance. Ancestors are topologically sorted using the same method
;; CLOS uses. Objects' parent lists are available to read -and- write at runtime.

;; Let's try a mixin:
(defparameter *mixin* (object :nickname '*mixin*))
(describe *mixin*)

;; We already saw that =standard-object= is *obj*'s only parent. Regardless, we'd like to add
;; *MIXIN* and let it have some new behavior.
(push *mixin* (object-parents *obj*))
;; And that's it...
(describe *obj*)
;; The hierarchy, now:
;;                  =T=
;;                   |
;;       =standard-object=
;;         /   /
;;    *mixin* /
;;       \   /
;;       *obj*
;;         |
;;      *child*
;;

;; But that may not be the precedence we want, or even the location in the hierarchy graph we
;; want *obj* to be at. Let's give *obj* a new parent.
(defparameter *super-obj* (object :nickname '*super-obj*))

;; Now let's get rid of =standard-object=, but keep *mixin*.
(setf (object-parents *obj*)
      (cons *super-obj* ; we want *super-obj* to have higher precedence than *mixin*
            (remove =standard-object= (object-parents *obj*))))

;; (SETF OBJECT-PARENTS) will alert you if you're doing something naughty, though, so no worries...
;; (push *obj* (object-parents *obj*)) => ERROR
;; (describe *obj*) => all is well

;; Let's admire our new, improved *obj*
(describe *obj*)
;; and remember, these changes affected *child*
(object-hierarchy-list *child*)

;; So now, ignoring *clone*, we have:
;;           =T=
;;            |
;;   =standard-object=
;;        /      \
;; *super-obj*  *mixin*
;;        \     /
;;         *obj*
;;           |
;;        *child*
;;
;; And the hierarchy list for child (most specific first)
;; *child* -> *obj* -> *super-obj* -> *mixin* -> =std-obj= -> =t=

;;;
;;; Properties
;;;
;; Properties can be either direct (present locally), or delegated (present in hierarchy list)

;; Simply SETFing the property you want to add will add it. You can optionally generate accessors.
(setf (property-value *mixin* 'mixed-in :accessor t) "Mixin's VAR value")

(describe *obj*)

;; Property values are retrieved with PROPERTY-VALUE
(property-value *obj* 'mixed-in) ; *obj* delegates to *mixin*

;; But (SETF PROPERTY-VALUE) created an accessor. Let's just use that:
(mixed-in *obj*) ; just like property-value. This is a place.

;; Delegated properties are retrieved according to the hierarchy list's precedence:
(setf (property-value *mixin* 'foo :accessor t) "mixin's value")
(setf (property-value *super-obj* 'foo) "*super-obj*'s value")

;; *mixin* defined an accessor, so *obj* can use it.
(foo *obj*)

;; There are also various functions for programmatically checking what properties
;; are available as well. I won't demo them now, though.

;;;
;;; Messages
;;;
;;; Messages are Sheeple's generic functions, and replies its methods.
;;; They follow similar rules wrt dispatch ordering, things like lambda-lists, etc.
;;; They also multi-dispatch.
(defmessage synergize (a b)
  (:documentation "This message Synergizes its arguments, preparing them for Web 3.0")
  (:reply (a b) (error "Don't know how to synergize ~A and ~A" a b)))

;; Some toy objects:
(defparameter *o1* (object :nickname '*o1*))
(defparameter *o2* (object :nickname '*o2*))

;; There are no classes in Sheeple. Because of that, replies dispatch on objects.
;; That means Sheeple's replies are essentially polymorphic EQL-specialized.
(defreply synergize ((o1 *o1*) (o2 *o2*))
  (print "Got *o1* as first arg, *o2* as second arg."))

(defreply synergize ((o2 *o2*) (o1 *o1*))
  (print "Got *o2* as first arg, *o1* as second arg."))

;; Nothing special going on here if you're already familiar with CLOS.

;; Sheeple defines some built-in object types meant for use in places like this.
;; Non-sheeple lisp types are automatically boxed when appropriate. The names of
;; the built-in prototypes are the same as their regular counterparts, wrapped in =foo=.
;; More on the =foo= naming scheme later.
(defreply synergize ((str1 =string=) (str2 =string=))
  (concatenate 'string str1 str2))

(defreply synergize ((a =number=) (b =number=))
  (+ a b))

;; Sheeple also has CLOS-style standard method combination, with :before, :around, and :after
;; And like I said, everything is basically EQL-specialized.
(defreply synergize :before ((a =number=) (b 3))
  (print "I think 3 is a very special number. I like 3 a lot."))

;; some sample calls:
(synergize *o1* *o2*)
(synergize *o2* *o1*)
(synergize "foo" "bar")
(synergize 1 2)
(synergize 2 3)

;; Let's get rid of one reply and try something different...
(undefreply synergize :before (=number= 3))
(synergize 2 3)

;; Now with :around, and a fancy lambda-list. &aux is awesome.
(defreply synergize :around ((a =number=) (b 3) &aux (total (call-next-reply)))
  (format t "The total is ~A.~%" total)
  total)

(synergize 8 3)

;;;
;;; Beyond the basics: Going the extra mile
;;;

;;; DEFOBJECT
;;; More convenient syntax for creating new objects
(defparameter *convenient!*
  (defobject *obj*
      ((var "value")
       other-var)                       ; this is initialized to NIL
    :nickname '*convenient!*))

;;; DEFPROTO
;;; Abstracting the "class" pattern in object-oriented programming.
(defproto =prototype= *super-obj*
  ((proto-property "Excellent!")))

(describe =prototype=)
;; Advantages of defproto:
;; 1. Automatically creates accessors
;; 2. Re-evaluating defproto form reinitializes the object, without changing identity
;; 3. Automatically assigns a nickname to the object

(defproto =prototype= *convenient!*
  ((another-property "Hey ho")))

(describe =prototype=)
;; DEFPROTO is only a convenience wrapper that handles regular objects.
;;
;; The idea is that some objects are very clearly only meant to be used as prototypes.
;; For those objects, DEFPROTO is a good option, since it lets you keep re-evaluating a single
;; form to alter it. You do not have to start an object off as a prototype, either:
(defvar =obj= *obj*)
(defproto =obj= (*super-obj* *mixin*))

(eq =obj= *obj*)

(describe =obj=)
;; The variable that DEFPROTO binds/uses is a regular dynamic variable.
;; It does not need to actually be wrapped by =foo=, but it is a stylistic marker to note
;; that the object is specifically meant to be used as a prototype.
