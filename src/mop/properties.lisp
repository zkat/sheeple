;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; indent-tabs-mode: nil -*-

;;;; This file is part of Sheeple

;;;; mop/properties.lisp
;;;;
;;;; Property access, inspection, and management stuff, for the most part.
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :sheeple)

(defmessage smop:direct-property-value (metaobject object propd)
  (:reply ((metaobject =standard-metaobject=) object (propd =standard-property=))
    (std-direct-property-value object (property-name propd))))

(defmessage smop:property-value (metaobject object propd)
  (:reply ((metaobject =standard-metaobject=) object (propd =standard-property=))
    (std-property-value object (property-name propd))))

(defmessage smop:add-direct-property (metaobject object property-name &key)
  (:reply ((metaobject =standard-metaobject=) object property-name &rest options)
    (apply #'std-add-direct-property object property-name options)))

(defmessage (setf smop:direct-property-value) (new-value metaobject object propd &key)
  (:reply (new-value (metaobject =standard-metaobject=) object (propd =standard-property=) &rest options)
    (apply #'(setf std-direct-property-value) new-value object (property-name propd) options)))

(defmessage smop:property-makunbound (metaobject object propd)
  (:reply ((metaobject =standard-metaobject=) object (propd =standard-property=))
    (std-property-makunbound object (property-name propd))))

(defmessage smop:remove-all-direct-properties (metaobject object)
  (:reply ((metaobject =standard-metaobject=) object)
    (std-remove-all-direct-properties object)))

(defmessage smop:direct-property-p (metaobject object propd)
  (:reply ((metaobject =standard-metaobject=) object (propd =standard-property=))
    (std-direct-property-p object (property-name propd))))

(defmessage smop:property-owner (metaobject object propd)
  (:reply ((metaobject =standard-metaobject=) object (propd =standard-property=))
    (std-property-owner object (property-name propd))))

(defmessage smop:direct-properties (metaobject object)
  (:reply ((metaobject =standard-metaobject=) object)
    (std-direct-properties object)))

(defmessage smop:available-properties (metaobject object)
  (:reply ((metaobject =standard-metaobject=) object)
    (std-available-properties object)))
