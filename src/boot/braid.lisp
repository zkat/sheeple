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
;;;   =STANDARD-PROPERTY=  | =STANDARD-METAOBJECT= |    (=STANDARD-OBJECT=)

;;; We only want to bootstrap an image once, so we perform a little check:
(unless (objectp =standard-metaobject=)

  ;; First, let's just get our objects:
  (setf =standard-metaobject= (std-allocate-object =standard-metaobject=)
          =standard-object=   (std-allocate-object =standard-metaobject=)
                 =t=          (std-allocate-object =standard-metaobject=)
         =standard-property=  (std-allocate-object =standard-metaobject=))

  ;; Now, we have to hardcode two circular links.

  ;; The first circular link is =STANDARD-METAOBJECT='s own metaobject. This
  ;; link isn't created above because '=STANDARD-METAOBJECT= is bound to junk
  ;; before boot, so we fix this manually now.

  ;; Due to the new way of storing metaobjects, this is some kludgy shit... this
  ;; setf is the sole reason that lineage-metaobject can't be read-only.
  (setf (lineage-metaobject (mold-lineage (%object-mold =standard-metaobject=)))
        =standard-metaobject=)

  ;; We also show off Sheeple's freedom from side-effects is by consing less.
  (let ((the-list (list =t=)))

    ;; The second circular link is =T='s hierarchy list. Hierarchy lists usually get
    ;; cached whenever an object's mold changes; however, because objects start out
    ;; pointing at the "null mold", and =T= remains pointing at this mold, we need
    ;; to fix =T='s %object-hierarchy manually.
    (setf (%object-precedence-list =t=) the-list)

    ;; Now, focus on the family!
    ;; We can't (push =t= (object-parents ...)) because VALIDATE-PARENT-METAOBJECT
    ;; doesn't exist yet.
    (change-parents =standard-object=     the-list)
    (change-parents =standard-metaobject= the-list))

  ;; Now we actually have to bootstrap property definitions.
  (change-parents =standard-property= (list =standard-object=))

  ;; We need to fudge a mold with a single standard property called NAME, to identify
  ;; what standard properties are supposed to be.
  (setf *std-propd-mold* (ensure-mold =standard-metaobject= (list =standard-property=)
                                      (vector (cons 'name =standard-property=)))))
