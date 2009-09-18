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
  ;; Before anything else happens, we need =STANDARD-METASHEEP= to exist:
  (setf =standard-metasheep= (std-allocate-sheep =standard-metasheep=))
  (setf (%sheep-metasheep =standard-metasheep=) =standard-metasheep=)
  (finalize-sheep-inheritance =standard-metasheep=)

  ;; =T= and =STANDARD-SHEEP= have special rules about parents.
  (setf =t= (finalize-sheep-inheritance (std-allocate-sheep =standard-metasheep=))
        =standard-sheep= (add-parent =t= (std-allocate-sheep =standard-metasheep=)))

  ;; We can define special messages now.
  (defmessage allocate-sheep (metasheep)
    (:documentation "Allocates a sheep object based on METASHEEP."))
  (defreply allocate-sheep ((sheep =standard-metasheep=))
    (std-allocate-sheep sheep))

  (defmessage shared-init (sheep &key &allow-other-keys)
    (:documentation "Adds properties to SHEEP and performs general initialization tasks."))
  (defreply shared-init (sheep &key properties nickname)
    (dolist (property-spec properties)
      (destructuring-bind (name value &rest keys) property-spec
        (apply 'add-property sheep name value keys)))
    (when nickname
      (setf (sheep-nickname sheep) nickname))
    sheep)

  (defmessage init-sheep (sheep &key &allow-other-keys)
    (:documentation "Performs 'once-only' initialization tasks on SHEEP."))
  (defreply init-sheep (sheep &key properties nickname)
    (shared-init sheep :properties properties :nickname nickname))

  (defmessage reinit-sheep (sheep &key &allow-other-keys)
    (:documentation "Resets parents and properties without changing SHEEP's identity."))
  (defreply reinit-sheep (sheep &key new-parents documentation properties)
    ;; In order to reinitialize a sheep, we first remove -all- parents and properties.
    (dolist (parent (%sheep-parents sheep)) (remove-parent parent sheep))
    (remove-all-direct-properties sheep)
    ;; Now we start over. This function boxes non-sheep parents.
    (add-parent* (cond ((null new-parents) =standard-sheep=)
                       ((every 'sheepp new-parents) new-parents)
                       (t (sheepify-list new-parents)))
                 sheep)
    ;; Setting up the properties all over again.
    (dolist (property-spec properties)
      (destructuring-bind (name value &rest keys) property-spec
        (apply 'add-property sheep name value keys)))
    ;; Finally, set the documentation. If none is provided, it's set to NIL.
    ;; It's important to note that documenting a sheep will keep a reference to it...
    ;; At the same time, REINIT-SHEEP is only meant for protos, so it should be fine.
    (setf (documentation sheep 't) documentation)
    sheep)

  ;; Now we redefine =standard-sheep= and =standard-metasheep= normally.
  (defproto =standard-sheep= (=t=) ())
  (add-parent =standard-sheep= =standard-metasheep=)

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
