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
  (setf =standard-metaobject= (std-allocate-object =standard-metaobject=)
          =standard-object=   (std-allocate-object =standard-metaobject=)
                 =t=          (std-allocate-object =standard-metaobject=))

  ;; Now, we have to hardcode two circular links.

  ;; =STANDARD-METAOBJECT= is its own metaobject. This link isn't created above
  ;; because '=STANDARD-METAOBJECT= is bound to junk before boot, so we fix
  ;; this manually now.
  (setf (%object-metaobject =standard-metaobject=) =standard-metaobject=)

  ;; =T= has a one-object hierarchy list, just itself. Hierarchy lists usually get
  ;; cached whenever an object's mold changes; however, because objects start out
  ;; pointing at the "null mold", and =T= remains pointing at this mold, we need
  ;; to fix =T='s %object-hierarchy manually.
  (push =t= (%object-hierarchy =t=))

  ;; Now, focus on the family!
  (push =t= (object-parents =standard-object=))
  (push =t= (object-parents =standard-metaobject=))

  ;; To finish up, break the ice by playing the name game:
  (macrolet ((set-name (name) `(setf (property-value ,name 'nickname) ',name)))
    (set-name =t=)
    (set-name =standard-object=)
    (set-name =standard-metaobject=)))

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
                          collect `(defproto ,name ,(or parents '=boxed-object=))))))
  (define-boxed-objects
    =character= =function= =hash-table= =package= =pathname= =readtable=
    =stream= =sequence= =symbol= (=boolean= =symbol=) (=list= =sequence=)
    (=null= =symbol= =list=) (=cons= =list=) =array= (=vector= =array= =sequence=)
    (=bit-vector= =vector=) (=string= =vector=) =number= (=complex= =number=)
    (=integer= =number=) (=float= =number=)))
