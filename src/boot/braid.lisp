;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; indent-tabs-mode: nil -*-

;;;; This file is part of Sheeple

;;;; boot/braid.lisp
;;;;
;;;; Weaving together the base objects and metaobjects
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :sheeple)

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

  ;; We gotta put this guy here. Sorry, Adlai! -- zkat
  (defmessage smop:validate-parent-metaobject (child-mo parent-mo)
    (:reply ((child =t=) (parent =t=)) nil)
    (:reply ((child =standard-metaobject=) (parent =standard-metaobject=)) t))

  ;; Now, focus on the family!
  (push =t= (object-parents =standard-object=))
  (push =t= (object-parents =standard-metaobject=))

  ;; To finish up, break the ice by playing the name game:
  (macrolet ((set-name (name) `(setf (property-value ,name 'nickname) ',name)))
    (set-name =t=)
    (set-name =standard-object=)
    (set-name =standard-metaobject=)))
