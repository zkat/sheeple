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
(unless *bootstrappedp*

  ;; Before anything else happens, we need =STANDARD-METAOBJECT= to exist:
  (setf =standard-metaobject= (std-allocate-object =standard-metaobject=)
        (%object-metaobject =standard-metaobject=) =standard-metaobject=
        (%object-mold =standard-metaobject=) (ensure-mold nil #()))
  (add-property =standard-metaobject= 'nickname '=standard-metaobject=)

  ;; =T= and =STANDARD-OBJECT= have special rules about parents.
  (setf =t=
        (aprog1 (std-allocate-object =standard-metaobject=)
          (setf (%object-mold it) (ensure-mold nil #()))
          (add-property it 'nickname '=t=))
        =standard-object=
        (aprog1 (std-allocate-object =standard-metaobject=)
          (setf (%object-mold it) (ensure-mold (list =t=) #()))
          ;; This could help, in case the boot fails before the DEFPROTO
          (add-property it 'nickname '=standard-object=)))

  ;; Adding the missing link in the holy trinity
  (push =t= (object-parents =standard-metaobject=))

  ;; ... and we're done!
  (setf *bootstrappedp* t))

(defmessage shared-init (object &rest initargs &key &allow-other-keys)
  (:documentation "Adds properties to OBJECT and performs general initialization tasks."))
(defreply shared-init (object &key properties documentation nickname)
  (dolist (property-spec properties)
    (destructuring-bind (name value &rest keys) property-spec
      (apply 'add-property object name value keys)))
  (when (and (not (null nickname)) (symbolp nickname)) ; NIL isn't a valid nickname
    (setf (object-nickname object) nickname))
  (when (stringp documentation)
    (setf (documentation object t) documentation))
  object)

(defmessage init-object (object &rest initargs &key &allow-other-keys)
  (:documentation "Performs 'once-only' initialization tasks on OBJECT."))
(defreply init-object (object &rest initargs &key &allow-other-keys)
  (apply #'shared-init object initargs))

(defmessage reinit-object (object &rest initargs &key &allow-other-keys)
  (:documentation "Resets parents and properties without changing OBJECT's identity."))
(defreply reinit-object (object &rest initargs &key parents &allow-other-keys)
  (when (null parents)                  ; Guard against funny business
    (push =standard-object= parents))
  (change-mold object (ensure-mold (objectify-list parents) #()))
  (apply #'shared-init object initargs))

(defproto =boxed-object= =t= ())
(defproto =symbol= =boxed-object= ())
(defproto =sequence= =boxed-object= ())
(defproto =array= =boxed-object= ())
(defproto =number= =boxed-object= ())
(defproto =character= =boxed-object= ())
(defproto =function= =boxed-object= ())
(defproto =hash-table= =boxed-object= ())
(defproto =package= =boxed-object= ())
(defproto =pathname= =boxed-object= ())
(defproto =readtable= =boxed-object= ())
(defproto =stream= =boxed-object= ())
(defproto =list= =sequence= ())
(defproto =null= (=symbol= =list=) ())
(defproto =cons= =list= ())
(defproto =vector= (=array= =sequence=) ())
(defproto =bit-vector= =vector= ())
(defproto =string= =vector= ())
(defproto =complex= =number= ())
(defproto =integer= =number= ())
(defproto =float= =number= ())
