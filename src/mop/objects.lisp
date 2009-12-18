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

(defmessage smop:compute-object-hierarchy-list (metaobject object)
  (:reply ((metaobject =standard-metaobject=) object)
    (std-compute-object-hierarchy-list object)))

(defmessage smop:validate-parent-metaobject (child-mo parent-mo)
  (:reply ((child =t=) (parent =t=)) nil)
  (:reply ((child =standard-metaobject=) (parent =standard-metaobject=)) t))

(defmessage (setf smop:object-metaobject) (new-metaobject old-metaobject object)
  (:reply ((new-mo =t=) (old-mo =t=) object)
    (error "Cannot change metaobject of ~A from ~A to ~A" object old-mo new-mo))
  (:reply ((new-mo =standard-metaobject=) (old-mo =standard-metaobject=) object)
    (setf (%object-metaobject object) new-mo)))
