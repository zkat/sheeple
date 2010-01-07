;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; indent-tabs-mode: nil -*-

;;;; This file is part of Sheeple

;;;; boot/init.lisp
;;;;
;;;; Defining the object initialization protocol
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :sheeple)

(defmessage shared-init (object &rest initargs)
  (:documentation "Adds properties to OBJECT and performs general initialization tasks.")
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
  (:documentation "Performs 'once-only' initialization tasks on OBJECT.")
  (:reply (object &rest initargs &key)
    (apply #'shared-init object initargs)))

(defmessage reinit-object (object &rest initargs)
  (:documentation "Resets parents and properties without changing OBJECT's identity.")
  (:reply (object &rest initargs &key parents (metaobject =standard-metaobject=))
    (when (null parents)                ; Guard against funny business
      (push =standard-object= parents))
    (setf (object-metaobject object) metaobject)
    (change-mold object (ensure-mold metaobject parents))
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
