;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; indent-tabs-mode: nil -*-

;;;; This file is part of Sheeple

;;;; post-boot.lisp
;;;;
;;;; Once sheeple is booted up, we can define messages/replies normally
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :sheeple)

;;;
;;; Object Creation Protocol
;;;
(defmessage allocate-object (metaobject)
  (:reply ((metaobject =standard-metaobject=))
    (std-allocate-object metaobject)))

(defmessage compute-object-hierarchy-list-using-metaobject (metaobject object)
  (:reply ((metaobject =standard-metaobject=) object)
    (std-compute-object-hierarchy-list object)))

;;;
;;; Extensible Object Creation
;;;
(defmessage create (proto &key)
  (:documentation "Creates a PROTO. Intended for customization."))
(defreply create ((proto =standard-object=) &rest properties)
  (object :parents proto :properties (plist-to-wide-alist properties)))

;;;
;;; Printing Objects!
;;;

(defmessage print-sheeple-object (object stream)
  (:documentation "Defines the expression print-object uses."))

(defreply print-sheeple-object (object (stream =stream=))
  (std-print-sheeple-object object stream))

(defreply print-sheeple-object ((object =boxed-object=) (stream =stream=))
  (print-unreadable-object (object stream :identity t)
    (format stream "Boxed-object ~:[[~S]~;~S~]"
            (direct-property-p object 'nickname)
            (ignore-errors (object-nickname object)))))
