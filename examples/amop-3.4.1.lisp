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

(defreply smop:compute-object-precedence-list ((metaobject =flavors-object=) object)
  (nconc (delete-duplicates
          (depth-first-preorder-ancestors* object)
          :from-end t)
         (list =standard-object= =t=)))

(defreply smop:compute-object-precedence-list ((metaobject =loops-object=) object)
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

(defproto q (s r))

(flet ((hierarchy-nicks (object)
         (mapcar #'object-nickname (object-precedence-list object))))
  (format t "~&Sheeple hierarchy: ~S~@
               Flavors hierarchy: ~S~@
               LOOPS   hierarchy: ~S~%"
          ;; We can change metaobjects at runtime, but we must actually
          ;; reinitialize them before their precedence list changes.
          (hierarchy-nicks q)
          (progn (defproto q (s r) () :metaobject =flavors-object=)
                 (hierarchy-nicks q))
          (progn (defproto q (s r) () :metaobject =loops-object=)
                 (hierarchy-nicks q))))
