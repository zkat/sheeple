;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; indent-tabs-mode: nil -*-

;;;; This is a translation of the CLOS code for alternative class precedence orderings
;;;; from Section 3.4.1 of The Art of the Metaobject Protocol, into Sheeple MOP code.

(in-package :sheeple-user)

(defproto =flavors-object= =standard-metaobject=)

(defproto =loops-object= =standard-metaobject=)

(defun depth-first-preorder-ancestors* (object)
  (unless (eq object =standard-object=)
    (cons object
          (mapcan #'depth-first-preorder-ancestors*
                  (object-parents object)))))

(defreply smop:compute-object-hierarchy-list ((metaobject =flavors-object=) object)
  (nconc (delete-duplicates
          (depth-first-preorder-ancestors* object)
          :from-end t)
         (list =standard-object= =t=)))

(defreply smop:compute-object-hierarchy-list ((metaobject =loops-object=) object)
  (nconc (delete-duplicates
          (depth-first-preorder-ancestors* object)
          :from-end nil)
         (list =standard-object= =t=)))

;;; Quick little hierarchy to demonstrate these:

(defproto a ())
(defproto b ())
(defproto c ())

(defproto s (a b))
(defproto r (a c))

(defproto q-sheeple (s r)
  ()
  :nickname 'q)

(defproto q-flavors (s r)
  ()
  :metaobject =flavors-object=
  :nickname 'q)

(defproto q-loops (s r)
  ()
  :metaobject =loops-object=
  :nickname 'q)

(flet ((hierarchy-nicks (object)
         (mapcar #'object-nickname (object-hierarchy-list object))))
  (format t "~&Sheeple hierarchy: ~S~@
               Flavors hierarchy: ~S~@
               LOOPS   hierarchy: ~S~%"
          (hierarchy-nicks q-sheeple)
          (hierarchy-nicks q-flavors)
          (hierarchy-nicks q-loops)))
