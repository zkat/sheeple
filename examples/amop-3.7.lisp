;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; indent-tabs-mode: nil -*-

;;;; This is a translation of section 3.7 from Art of the Metaobject Protocol, which
;;;; implements a 'monitored class' whose slot-accesses are taken note of.

(in-package :sheeple-user)

(defproto =monitored-metaobject= =standard-metaobject=
  ((access-history nil)))

(defmessage note-operation (metaobject object property-name operation)
  (:reply ((metaobject =monitored-metaobject=) object property-name operation)
    (push (list operation object property-name (get-universal-time)) (access-history metaobject))))

(defmessage reset-access-history (metaobject)
  (:reply ((metaobject =monitored-metaobject=))
    (setf (access-history metaobject) nil)))

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

(defproto =monitored-object= ()
  ((prop1 "var")
   (prop2 234))
  :metaobject =monitored-metaobject=)

;; Just the act of creating the monitored object should trigger a couple of these.
;; You can see which ones have already been performed by doing
;; (access-history =monitored-metaobject=)
;;
;; To try it further, try performing some of the operations we wrote replies for.
