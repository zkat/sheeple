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
