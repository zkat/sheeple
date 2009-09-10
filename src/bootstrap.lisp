;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; indent-tabs-mode: nil -*-

;;;; This file is part of Sheeple

;;;; bootstrap.lisp
;;;;
;;;; This creates all the base objects that will be used during cloning.
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :sheeple)

;;; Bootstrap time!
;; using a progn should ensure that they're all called in this specific order..
(progn
;; before anything works, we need to make it so =standard-metasheep= exists...
  (setf =standard-metasheep= (std-allocate-sheep =standard-metasheep=))
  (setf (%sheep-metasheep =standard-metasheep=) =standard-metasheep=)

;; Since =T= and =STANDARD-SHEEP= have special rules about parents, we hand-craft them first.
  (setf =t= (std-allocate-sheep =standard-metasheep=)
        =standard-sheep= (add-parent =t= (std-allocate-sheep =standard-metasheep=)))

;; We can define some special messages now. It's not terribly important to do it in this
;; order for 3.0, since messages are just structs, but I'm keeping this layout for 3.1
  (defmessage allocate-sheep (metasheep)
    (:documentation "Allocates a sheep object based on METASHEEP."))
  (defreply allocate-sheep ((sheep =standard-metasheep=))
    (std-allocate-sheep sheep))

  (defmessage shared-init (sheep &key &allow-other-keys)
    (:documentation "Primarily meant to fill SHEEP's properties during initialization when
using certain macros (such as defsheep and defproto)."))
  (defreply shared-init (sheep &key properties)
    "DUMPIN' SUM PROPS!"
    (when properties
      (mapcar (fun (apply #'add-property sheep _))
              properties))
    sheep)

  (defmessage init-sheep (sheep &key &allow-other-keys))
  (defreply init-sheep (sheep
                        &key
                        properties)
    (shared-init sheep :properties properties)
    sheep)

  (defmessage reinit-sheep (sheep &key &allow-other-keys)
    (:documentation "Resets the sheep's parents and properties."))
  (defreply reinit-sheep (sheep &key new-parents
                                documentation
                                properties)
    "If :NEW-PARENTS is  provided, those parents are used when reinitializing,
so DOLLY doesn't end up on the list by default."
    ;; In order to reinitialize a sheep, we first remove -all- parents and properties.
    (map nil (rcurry 'remove-parent sheep) (sheep-parents sheep))
    (remove-all-direct-properties sheep)
    ;; Once that's set, we can start over. We must remember to add =standard-sheep=
    ;; as a last resort.
    (add-parents (if new-parents (sheepify-list new-parents) (list =standard-sheep=))
                 sheep)
    ;; Now we can set up the properties all over again.
    (mapc (fun (apply #'add-property sheep _)) properties)
    ;; And finally, set the documentation. If none is provided, it's set to NIL.
    ;; It's important to note that documenting a sheep will keep a reference to it...
    ;; At the same time, REINIT-SHEEP is only meant for protos, so it should be fine.
    (setf (documentation sheep 't) documentation)
    sheep)

  ;; Now we take care of redefining =standard-sheep= normally. This should give us an idea
  ;; of how bootstrapping went.
  (defproto =standard-sheep= (=t=) ())
  
  ) ; basically bootstrapped!


;; now we just define all the builtins, and we're good to go.
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

(setf *bootstrappedp* t)