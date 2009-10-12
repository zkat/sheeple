;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; indent-tabs-mode: nil -*-

;;;; This file is part of Sheeple

;;;; post-boot.lisp
;;;;
;;;; Once sheeple is booted up, we can define messages/replies normally
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :sheeple)

;;;
;;; Object creation protocol
;;;
(defmessage allocate-object (metaobject))
(defreply allocate-object ((metaobject =standard-metaobject=))
  (std-allocate-object metaobject))

(defmessage compute-object-hierarchy-list-using-metaobject (metaobject object))
(defreply compute-object-hierarchy-list-using-metaobject
    ((metaobject =standard-metaobject=) object)
  (std-compute-object-hierarchy-list object))

(defmessage finalize-object-inheritance-using-metaobject (metaobject object))
(defreply finalize-object-inheritance-using-metaobject
    ((metaobject =standard-metaobject=) object)
  (std-finalize-object-inheritance object))

(defmessage remove-parent-using-metaobjects (parent-metaobject child-metaobject parent child))
(defreply remove-parent-using-metaobjects ((parent-metaobject =standard-metaobject=)
                                           (child-metaobject =standard-metaobject=)
                                           parent child)
  (std-remove-parent parent child))

(defmessage add-parent-using-metaobjects (parent-metaobject child-metaobject parent child))
(defreply add-parent-using-metaobjects ((parent-metaobject =standard-metaobject=)
                                        (child-metaobject =standard-metaobject=)
                                        parent child)
  (std-add-parent parent child))

;;;
;;; Nicknames
;;;
(defun object-nickname (object)
  "Returns OBJECT's nickname"
  (property-value object 'nickname))

(defun (setf object-nickname) (new-nickname object)
  "Sets OBJECT's nickname to NEW-NICKNAME"
  (handler-bind ((unbound-property 'continue))
    (setf (property-value object 'nickname) new-nickname)))

;;; Now we name all the built-in object like we're Adam in Eden.
(mapc #'(setf object-nickname)
      '(=t= =standard-object= =standard-metaobject= =boxed-object= =symbol=
        =sequence= =array= =number= =character= =function= =hash-table=
        =package= =pathname= =readtable= =stream= =list= =null= =cons=
        =vector= =bit-vector= =string= =complex= =integer= =float=)
      (list =t= =standard-object= =standard-metaobject= =boxed-object= =symbol=
            =sequence= =array= =number= =character= =function= =hash-table=
            =package= =pathname= =readtable= =stream= =list= =null= =cons=
            =vector= =bit-vector= =string= =complex= =integer= =float=))

;;;
;;; Object Documentation
;;;

(defmethod documentation ((x object) (doc-type (eql 't)))
  (property-value x 'documentation))

(defmethod (setf documentation) (new-value (x object) (doc-type (eql 't)))
  (handler-bind ((unbound-property 'continue))
    (setf (property-value x 'documentation) new-value)))

;;;
;;; Printing object!
;;;

(defmessage print-sheeple-object (object stream)
  (:documentation "Defines the expression print-object uses."))

(defreply print-sheeple-object (object (stream =stream=))
  (std-print-sheeple-object object stream))

(defreply print-sheeple-object ((object =boxed-object=) (stream =stream=))
  (print-unreadable-object (object stream :identity t)
    (format stream "Boxed-object ~:[[~S]~;~S~]"
            (has-direct-property-p object 'nickname)
            (ignore-errors (object-nickname object)))))
