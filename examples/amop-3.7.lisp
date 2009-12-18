;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; indent-tabs-mode: nil -*-

;;;; This is a translation of section 3.7 from Art of the Metaobject Protocol, which
;;;; implements a 'monitored class' whose slot-accesses are taken note of.

(in-package :sheeple-user)

;; The first step is to create the metaobject. MOP messages will dispatch in this to alter behavior.
(defproto =monitored-metaobject= =standard-metaobject=
  ;; The AMOP example puts the access history in a closure. In our case, we can simply use the
  ;; metaobject to hold this 'global' data.
  ((access-history nil)))

;; We use NOTE-OPERATION to actually register each property operation in our access history.
(defmessage note-operation (metaobject object property-name operation)
  (:reply ((metaobject =monitored-metaobject=) object property-name operation)
    (push (list operation object property-name (get-universal-time)) (access-history metaobject))))

;; Now we can actually define our MOP replies. The Sheeple MOP lives in the Sheeple-MOP package.
;; It has an smop: nickname for convenience, since the package is not meant to be :used by anything
;; that already :uses Sheeple.
;;
;; MOP messages are named identically to their standard Sheeple counterparts, and have the same
;; lambda-list except for an extra argument, the metaobject, which the message uses to dispatch.
;; Users of the MOP should not specialize these replies on anything but the metaobjects.
;;
;; The pattern for actually defining our replies is simple: We just write a :before reply for each
;; bit of property access we want to keep track of, and make it call NOTE-OPERATION before continuing
;; with their standard behavior.
;;
(defreply smop:direct-property-value :before ((mo =monitored-metaobject=) object pname)
  (note-operation mo object pname 'direct-property-access))

(defreply smop:property-value :before ((mo =monitored-metaobject=) object pname)
  (note-operation mo object pname 'property-access))

(defreply (setf smop:property-value) :before
          (new-value (mo =monitored-metaobject=) object pname &key)
  (declare (ignore new-value))
  (note-operation mo object pname 'set-property-value))

(defreply smop:direct-property-p :before ((mo =monitored-metaobject=) object pname)
  (note-operation mo object pname 'checked-direct-property-existence))

(defreply smop:property-makunbound :before ((mo =monitored-metaobject=) object pname)
  (note-operation mo object pname 'property-removal))

;; Once we've defined our 'alternate' object system, it's just a matter of creating a new object
;; and making it use our =monitored-metaobject=, which essentially puts it in the realm of
;; 'The fork of Sheeple that happens to monitor some property-related stuff'.
(defproto =monitored-object= ()
  ((prop1 "var")
   (prop2 234))
  :metaobject =monitored-metaobject=)

;; Just the act of creating the monitored object should trigger a couple of these.
;; You can see which ones have already been performed by doing
;; (access-history =monitored-metaobject=)
;;
;; To try it further, perform some of the operations we wrote replies for.

;; One neat feature of Sheeple's MOP is that you are able to, when appropriate, change metaobjects
;; for existing objects.

;; Let's clear the existing access history to demonstrate...
(setf (access-history =monitored-metaobject=) nil)

;; We first create our object normally...
(defproto =initially-unmonitored= ()
  ((unmonitored-property "Test")))

;; Now we can swap the metaobjects...
(setf (object-metaobject =initially-unmonitored=) =monitored-metaobject=)

(print "History is empty...")
(print (access-history =monitored-metaobject=))

(setf (unmonitored-property =initially-unmonitored=) "Big Brother is watching me.")

(print "Object is now monitored!")
(print (access-history =monitored-metaobject=))
