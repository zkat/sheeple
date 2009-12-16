;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; indent-tabs-mode: nil -*-

;;;; This file is part of Sheeple

;;;; properties.lisp
;;;;
;;;; Property access, inspection, and management stuff, for the most part.
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :sheeple)

(defmessage smop:direct-property-value (metaobject object property-name)
  (:reply ((metaobject =standard-metaobject=) object property-name)
    (std-sheeple:direct-property-value object property-name)))

(defmessage smop:property-value (metaobject object property-name)
  (:reply ((metaobject =standard-metaobject=) object property-name)
    (std-sheeple:property-value object property-name)))

(defmessage (setf smop:property-value) (new-value metaobject object property-name &key)
  (:reply (new-value (metaobject =standard-metaobject=) object property-name &rest options)
    (apply #'(setf std-sheeple:property-value) new-value object property-name options)))

(defmessage smop:property-makunbound (metaobject object property-name)
  (:reply ((metaobject =standard-metaobject=) object property-name)
    (std-sheeple:property-makunbound object property-name)))

(defmessage smop:remove-all-direct-properties (metaobject object)
  (:reply ((metaobject =standard-metaobject=) object)
    (std-sheeple:remove-all-direct-properties object)))

(defmessage smop:direct-property-p (metaobject object property-name)
  (:reply ((metaobject =standard-metaobject=) object property-name)
    (std-sheeple:direct-property-p object property-name)))

(defmessage smop:property-owner (metaobject object property-name)
  (:reply ((metaobject =standard-metaobject=) object property-name)
    (std-sheeple:property-owner object property-name)))

(defmessage smop:direct-properties (metaobject object)
  (:reply ((metaobject =standard-metaobject=) object)
    (std-sheeple:direct-properties object)))

(defmessage smop:available-properties (metaobject object)
  (:reply ((metaobject =standard-metaobject=) object)
    (std-sheeple:available-properties object)))