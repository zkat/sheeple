;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; indent-tabs-mode: nil -*-

;;;; This file is part of Sheeple

;;;; bootstrap.lisp
;;;;
;;;; This creates all the base objects that will be used during cloning.
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :sheeple)

;;; Bootstrap time! Watch closely, it's very simple.

;;; We need to construct the following objects:

;;;          Object        |      Metaobject       |    Parents
;;; -----------------------+-----------------------+---------------
;;;           =T=          | =STANDARD-METAOBJECT= |     NIL
;;;    =STANDARD-OBJECT=   | =STANDARD-METAOBJECT= |    (=T=)
;;;  =STANDARD-METAOBJECT= | =STANDARD-METAOBJECT= |    (=T=)

;;; We only want to bootstrap an image once, so we perform a little check:
(unless (objectp =standard-metaobject=)

  ;; First, let's just get our objects:
  (setf =standard-metaobject= (std-allocate-object)
          =standard-object=   (std-allocate-object)
                 =t=          (std-allocate-object))

  ;; Now, we have a circular link to take care of:
  (setf (%object-metaobject =standard-metaobject=) =standard-metaobject=)

  ;; Focus on the family!
  (push =t= (object-parents =standard-object=))
  (push =t= (object-parents =standard-metaobject=))

  ;; Break the ice by playing the name game:
  (dolist (name '(=t= =standard-object= =standard-metaobject=))
    (setf (property-value (symbol-value name) 'nickname) name)))

;;; Well, not really. We still need some messages to create objects:
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
  (:reply (object &rest initargs &key parents)
    (when (null parents)                ; Guard against funny business
      (push =standard-object= parents))
    (change-mold object (ensure-mold parents))
    (apply #'shared-init object initargs)))

;;; And, we need to mirror the CL type system:
(defproto =boxed-object= =t= ())

(macrolet ((define-boxed-objects (&body names)
             `(progn ,@(loop for (name . parents) in (mapcar 'ensure-list names)
                          collect `(defproto ,name ,(or parents '=boxed-object=) ())))))
  (define-boxed-objects
    =character= =function= =hash-table= =package= =pathname= =readtable=
    =stream= =sequence= =symbol= (=boolean= =symbol=) (=list= =sequence=)
    (=null= =symbol= =list=) (=cons= =list=) =array= (=vector= =array= =sequence=)
    (=bit-vector= =vector=) (=string= =vector=) =number= (=complex= =number=)
    (=integer= =number=) (=float= =number=)))
