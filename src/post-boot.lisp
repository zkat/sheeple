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
  (:documentation "Creates a PROTO. Intended for customization.")
  (:reply (proto &rest properties)
    (object :parents proto :properties (plist-to-wide-alist properties))))

;;;
;;; Printing Objects!
;;;
(defmessage print-sheeple-object (object stream)
  (:documentation "Defines the expression print-object uses.")
  (:reply (object (stream =stream=))
    (std-print-sheeple-object object stream))
  (:reply ((object =boxed-object=) (stream =stream=))
    (print-unreadable-object (object stream :identity t)
      (format stream "Boxed-object ~:[[~S]~;~S~]"
              (direct-property-p object 'nickname)
              (ignore-errors (object-nickname object))))))

;;;
;;; Error Reporting and Recovery
;;;
(defmessage no-applicable-reply (message &rest args)
  (:documentation "Called when no reply is applicable for a message invocation.")
  (:reply (message &rest args)
    (error 'no-applicable-reply :message message :args args)))

(defmessage no-next-reply (message reply &rest args)
  (:documentation "Called by `call-next-reply' when there is no next reply.")
  (:reply (message reply &rest args)
    (error 'no-next-reply :message message :reply reply :args args)))
