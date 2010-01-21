;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; indent-tabs-mode: nil -*-

;;;; This file is part of Sheeple

;;;; boot/init.lisp
;;;;
;;;; Defining the object initialization protocol
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :sheeple)

(defmessage shared-init (object &rest initargs)
  (:documentation "Performs shared initialization tasks shared by both INIT-OBJECT and
  REINIT-OBJECT. Such tasks include handling of the :PROPERTIES, :NICKNAME, and :DOCUMENTATION
  arguments to both INIT-OBJECT and REINIT-OBJECT.")
  (:reply (object &key properties (documentation nil doxp) (nickname nil nicknamep))
    (dolist (property-spec properties)
      (destructuring-bind (name &optional value &rest keys) (ensure-list property-spec)
        (apply #'(setf property-value) value object name keys)))
    (when nicknamep
      (setf (object-nickname object) nickname))
    (when doxp
      (setf (documentation object t) documentation))
    object))

(defmessage init-object (object &rest initargs)
  (:documentation "Called by the `OBJECT' function when a new object is first created. The standard
  primary reply for this message simply calls `SHARED-INIT' on OBJECT using INITARGS.")
  (:reply (object &rest initargs &key)
    (apply #'shared-init object initargs)))

(defmessage reinit-object (object &rest initargs)
  (:documentation "Reinitializes OBJECT, removing all properties and parents. The primary reply for
this message accepts :parents and :metaobject keywords. Providing these allows users to give the
object a new set of parents, or provide a new metaobject. Any other arguments passed to
REINIT-OBJECT will be passed to SHARED-INIT after parents and metaobject are reset.  This message's
primary client is DEFPROTO, which calls REINIT-OBJECT on existing prototypes to reset them whenever
the DEFPROTO form is re-evaluated.")
  (:reply (object &rest initargs &key parents (metaobject =standard-metaobject=))
    (when (null parents)                ; Guard against funny business
      (push =standard-object= parents))
    ;; This is a bit ugly. (SETF SMOP:OBJECT-METAOBJECT) also calls CHANGE-LINEAGE,
    ;; so we're actually changing the lineage twice.
    (setf (object-metaobject object) metaobject)
    (change-lineage object (ensure-lineage metaobject parents))
    (change-mold object *empty-mold*)
    (apply #'shared-init object initargs)))

(defmessage create (proto &key)
  (:documentation "Creates a PROTO. Intended for customization.")
  (:reply :before ((proto =t=) &key)
    (warn 'deprecated-feature :version "3.0.4"
          :feature (symbol-function 'create)))
  (:reply (proto &rest properties)
    (object :parents `(,proto) :properties (plist-to-wide-alist properties))))

(defmessage make (proto &key)
  (:documentation "Makes a PROTO. Intended for customization.")
  (:reply (proto &rest properties)
    (object :parents `(,proto) :properties (plist-to-wide-alist properties))))
