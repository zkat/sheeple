;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; indent-tabs-mode: nil -*-

;;;; This file is part of Sheeple

;;;; bootstrap.lisp
;;;;
;;;; This creates all the base objects that will be used during cloning.
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :sheeple)

;;; Bootstrap time!

(unless *bootstrappedp*
  ;; Before anything else happens, we need =STANDARD-METAOBJECT= to exist:
  (setf =standard-metaobject= (std-allocate-object =standard-metaobject=))
  (setf (%object-metaobject =standard-metaobject=) =standard-metaobject=
        (%object-mold =standard-metaobject=) (ensure-mold nil #()))

  ;; =T= and =STANDARD-OBJECT= have special rules about parents.
  (setf =t=
        (aprog1 (std-allocate-object =standard-metaobject=)
          (setf (%object-mold it) (ensure-mold nil #()))
          (add-property it 'nickname '=t=))
        =standard-object=
        (let ((obj (std-allocate-object =standard-metaobject=)))
          (setf (%object-mold obj)
                (ensure-mold (list =t=) #()))
          obj)))

(defmessage shared-init (object &rest initargs &key &allow-other-keys)
  (:documentation "Adds properties to OBJECT and performs general initialization tasks."))
(defreply shared-init (object &key properties
                              (documentation nil doxp)
                              (nickname nil nicknamep))
  (dolist (property-spec properties)
    (destructuring-bind (name value &rest keys) property-spec
      (apply 'add-property object name value keys)))
  (when nicknamep
    (setf (object-nickname object) nickname))
  (when doxp
    (setf (documentation object t) documentation))
  object)

(defmessage init-object (object &rest initargs &key &allow-other-keys)
  (:documentation "Performs 'once-only' initialization tasks on OBJECT."))
(defreply init-object (object &rest initargs &key &allow-other-keys)
  (apply #'shared-init object initargs))

(defmessage reinit-object (object &rest initargs &key &allow-other-keys)
  (:documentation "Resets parents and properties without changing OBJECT's identity."))
(defreply reinit-object (object &rest initargs
                                &key parents
                                &allow-other-keys)
  (change-mold object (ensure-mold (if (null parents)
                                       (list =standard-object=)
                                       (objectify-list parents))
                                   #()))
  (apply #'shared-init object initargs)
  object)

(unless *bootstrappedp*
  (defproto =standard-object= (=t=) ())
  (change-mold =standard-metaobject=
               (ensure-mold (list =t=) #()))

  ;; Now we just define all the builtins, and we're good to go.
  (defproto =boxed-object= (=t=) ())
  (defproto =symbol= (=boxed-object=) ())
  (defproto =sequence= (=boxed-object=) ())
  (defproto =array= (=boxed-object=) ())
  (defproto =number= (=boxed-object=) ())
  (defproto =character= (=boxed-object=) ())
  (defproto =function= (=boxed-object=) ())
  (defproto =hash-table= (=boxed-object=) ())
  (defproto =package= (=boxed-object=) ())
  (defproto =pathname= (=boxed-object=) ())
  (defproto =readtable= (=boxed-object=) ())
  (defproto =stream= (=boxed-object=) ())
  (defproto =list= (=sequence=) ())
  (defproto =null= (=symbol= =list=) ())
  (defproto =cons= (=list=) ())
  (defproto =vector= (=array= =sequence=) ())
  (defproto =bit-vector= (=vector=) ())
  (defproto =string= (=vector=) ())
  (defproto =complex= (=number=) ())
  (defproto =integer= (=number=) ())
  (defproto =float= (=number=) ())

  (setf *bootstrappedp* t))
