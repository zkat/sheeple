;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; indent-tabs-mode: nil -*-

;;;; This file is part of Sheeple

;;;; mop/objects.lisp
;;;;
;;;; Object creation, cloning, inspection
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :sheeple)

(defmessage smop:allocate-object (metaobject)
  (:reply ((metaobject =standard-metaobject=))
    (std-allocate-object metaobject)))

(defmessage smop:compute-object-precedence-list (metaobject object)
  (:reply ((metaobject =standard-metaobject=) object)
    (std-compute-object-precedence-list object)))

(defmessage smop:validate-parent (child-metaobject child parent-metaobject parent)
  (:reply (child-mo child parent-mo parent)
    (declare (ignore child))
    ;; This reply implements behavior similar to that perscribed in AMOP for the
    ;; default primary method on `validate-superclass'; it returns T when
    ;;   the PARENT argument is the object =t=, or
    ;;   the two metaobjects are identical.
    ;; Sheeple doesn't yet have an equilavent of `funcallable-standard-class'.
    (or (eq parent =t=)
        (eq parent-mo child-mo))))

(defmessage (setf smop:object-metaobject) (new-metaobject old-metaobject object)
  (:reply ((new-mo =standard-metaobject=) (old-mo =standard-metaobject=) object)
    (change-lineage object (ensure-lineage new-mo (%object-parents object)))))
