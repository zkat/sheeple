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
  (setf (%object-metaobject =standard-metaobject=) =standard-metaobject=)
  (finalize-object-inheritance =standard-metaobject=)

  ;; =T= and =STANDARD-OBJECT= have special rules about parents.
  (setf =t= (finalize-object-inheritance (std-allocate-object =standard-metaobject=))
        =standard-object= (add-parent =t= (std-allocate-object =standard-metaobject=)))

  ;; We can define special messages now.
  (defmessage allocate-object (metaobject)
    (:documentation "Allocates a object object based on METAOBJECT."))
  (defreply allocate-object ((object =standard-metaobject=))
    (std-allocate-object object))

  (defmessage shared-init (object &key &allow-other-keys)
    (:documentation "Adds properties to OBJECT and performs general initialization tasks."))
  (defreply shared-init (object &key properties nickname)
    (dolist (property-spec properties)
      (destructuring-bind (name value &rest keys) property-spec
        (apply 'add-property object name value keys)))
    (when nickname
      (setf (object-nickname object) nickname))
    object)

  (defmessage init-object (object &key &allow-other-keys)
    (:documentation "Performs 'once-only' initialization tasks on OBJECT."))
  (defreply init-object (object &key properties nickname)
    (shared-init object :properties properties :nickname nickname))

  (defmessage reinit-object (object &key &allow-other-keys)
    (:documentation "Resets parents and properties without changing OBJECT's identity."))
  (defreply reinit-object (object &key parents documentation properties)
    ;; In order to reinitialize a object, we first remove -all- parents and properties.
    (dolist (parent (%object-parents object)) (remove-parent parent object))
    (remove-all-direct-properties object)
    ;; Now we start over. This function boxes non-object parents.
    (add-parent* (if (null parents) =standard-object= (objectify-list parents)) object)
    ;; Setting up the properties all over again.
    (dolist (property-spec properties)
      (destructuring-bind (name value &rest keys) property-spec
        (apply 'add-property object name value keys)))
    ;; Finally, set the documentation. If none is provided, it's set to NIL.
    ;; It's important to note that documenting a object will keep a reference to it...
    ;; At the same time, REINIT-OBJECT is only meant for protos, so it should be fine.
    (setf (documentation object 't) documentation)
    object)

  ;; Now we redefine =standard-object= and =standard-metaobject= normally.
  (defproto =standard-object= (=t=) ())
  (add-parent =standard-object= =standard-metaobject=)

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

  ;; Make sure that we don't bootstrap the same image twice.
  (setf *bootstrappedp* t))
