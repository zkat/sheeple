;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; indent-tabs-mode: nil -*-

;;;; This file is part of Sheeple

;;;; properties.lisp
;;;;
;;;; Property access, inspection, and management stuff, for the most part.
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :sheeple)

(defun property-position (property-name object)
  (position property-name (mold-properties (%object-mold object))))

;;;
;;; Existential
;;;
(defun std-sheeple:direct-property-value (object property-name)
  (check-type property-name symbol)
  (aif (property-position property-name object)
       (svref (%object-property-values object) it)
       (restart-case (error 'unbound-property :object object :property-name property-name)
         (continue ()
           :report "Try accessing the property again."
           (direct-property-value object property-name))
         (use-value (value)
           :report "Return a value."
           :interactive (lambda ()
                          (format *query-io* "~&Value to use: ")
                          (list (read *query-io*)))
           value))))
(defun direct-property-value (object property-name)
  "Returns the property-value set locally in OBJECT for PROPERTY-NAME. If the
property is not set locally, a condition of type `unbound-property' is signaled."
  (if (std-object-p object)
      (std-sheeple:direct-property-value object property-name)
      (smop:direct-property-value (object-metaobject object) property-name)))

(defun std-sheeple:direct-property-p (object property-name)
  (handler-case (progn (direct-property-value object property-name) t)
    (unbound-property () nil)))
(defun direct-property-p (object property-name)
  "Returns T if OBJECT has a property called PROPERTY-NAME as a direct property.
NIL otherwise."
  (if (std-object-p object)
      (std-sheeple:direct-property-p object property-name)
      (smop:direct-property-p (object-metaobject object) object property-name)))

(defun std-sheeple:property-makunbound (object property-name)
  (if (direct-property-p object property-name)
      (prog1 object
        (change-mold object
                     (ensure-mold (object-parents object)
                                  (remove property-name
                                          (mold-properties (%object-mold object))))))
      (error 'unbound-property :object object :property-name property-name)))
(defun property-makunbound (object property-name)
  "Removes OBJECT's direct property named PROPERTY-NAME. Signals an error if there is no such
direct property. Returns OBJECT."
  (if (std-object-p object)
      (std-sheeple:property-makunbound object property-name)
      (smop:property-makunbound (object-metaobject object) object property-name)))

(defun remove-property (object property-name)
  "Removes OBJECT's direct property named PROPERTY-NAME. Signals an error if there is no such
direct property. Returns OBJECT."
  (warn 'deprecated-feature :feature #'remove-property :version "3.0.2")
  (property-makunbound object property-name))

(defun std-sheeple:remove-all-direct-properties (object)
  (change-mold object (ensure-mold (object-parents object) #()))
  object)
(defun remove-all-direct-properties (object)
  "Wipes out all direct properties and their values from OBJECT."
  (if (std-object-p object)
      (std-sheeple:remove-all-direct-properties object)
      (smop:remove-all-direct-properties (object-metaobject object) object)))


;;;
;;; Value
;;;
(defun std-sheeple:property-value (object property-name)
  (check-type property-name symbol)
  (dolist (ancestor (object-hierarchy-list object)
           (error 'unbound-property :object object :property-name property-name))
    (handler-bind ((unbound-property (fun (go :next))))
      (return (direct-property-value ancestor property-name)))
    :next))
(defun property-value (object property-name)
  "Returns the property-value for PROPERTY-NAME found first in OBJECT's hierarchy list.
If the value does not exist in the hierarchy list, a condition of type `unbound-property'
is signaled."
  (if (std-object-p object)
      (std-sheeple:direct-property-value object property-name)
      (smop:property-value (object-metaobject object) object property-name)))

(defun (setf std-sheeple:property-value) (new-value object property-name
                                          &key (reader nil readerp) (writer nil writerp) accessor)
  "Sets NEW-VALUE as the value of a direct-property belonging to OBJECT, named
PROPERTY-NAME."
  (check-type property-name symbol)
  ;; (SETF PROPERTY-VALUE) is split into two parts.
  ;; The first actually adds a property-value directly on the object:
  (aif (property-position property-name object)
       (setf (svref (%object-property-values object) it) new-value)
       (progn
         (change-mold object (ensure-transition (%object-mold object) property-name))
         (let ((index (property-position property-name object)))
           (setf (svref (%object-property-values object) index) new-value))))
  ;; Once that's done, we use the options passed to it to generate readers/writers/accessors:
  (when reader (add-reader-to-object reader property-name object))
  (when writer (add-writer-to-object writer property-name object))
  (when accessor
    (let ((accessor-name (if (eq t accessor) property-name accessor)))
      (unless (and readerp (null reader))
        (add-reader-to-object accessor-name property-name object))
      (unless (and writerp (null writer))
        (add-writer-to-object `(setf ,accessor-name) property-name object))))
  ;; Finally, for SETF-compliance, we return the value.
  new-value)
(defun (setf property-value) (new-value object property-name &rest options)
  (if (std-object-p object)
      (apply #'(setf std-sheeple:property-value) new-value object property-name options)
      (apply #'(setf smop:property-value) new-value
             (object-metaobject object) object property-name options)))

;;;
;;; Special Properties
;;;
(defmethod documentation ((x object) (doc-type (eql 't)))
  (property-value x 'documentation))

(defmethod (setf documentation) ((new-value string) (x object) (doc-type (eql 't)))
  (handler-bind ((unbound-property 'continue))
    (setf (property-value x 'documentation) new-value)))

(defun object-nickname (object)
  "Returns OBJECT's nickname"
  (property-value object 'nickname))

(defun (setf object-nickname) (new-nickname object)
  "Sets OBJECT's nickname to NEW-NICKNAME"
  (handler-bind ((unbound-property 'continue))
    (setf (property-value object 'nickname) new-nickname)))

;;;
;;; Reflection API
;;;
(defun property-owner (object property-name)
  "Returns the object, if any, from which OBJECT would fetch the value for PROPERTY-NAME"
  (find-if (rcurry 'direct-property-p property-name) (object-hierarchy-list object)))

(defun direct-properties (object)
  "Returns a list of the names of OBJECT's direct properties -- ie, only ones which have been
set directly in OBJECT using (setf property-value). The consequences of side-effecting this
returned list are undefined."
  (coerce (mold-properties (%object-mold object)) 'list))

(defun available-properties (object)
  "Returns a list of the names of all properties available to OBJECT, including inherited ones."
  (delete-duplicates (nconc (copy-list (direct-properties object))
                            (mapcan 'available-properties (object-parents object)))))

(defmethod describe-object ((object object) stream)
  (format stream
          "~&Object: ~A~@
           Parents: ~A~@
           Properties: ~%~{~A~%~}"
          object (object-parents object)
          (mapcar (fun (format nil "~S: ~S~@[ (Delegated to: ~A)~]"
                               (car _) (second _)
                               (unless (eq object (third _))
                                 (third _))))
                  (mapcar (fun (list  _ (property-value object _) (property-owner object _)))
                          (available-properties object)))))

;;;
;;; Convenience
;;;
(defmacro with-properties (properties object &body body)
  (let ((sh (gensym)))
    `(let ((,sh ,object))
       (symbol-macrolet ,(mapcar (lambda (property-entry)
                                   (let ((var-name
                                          (if (symbolp property-entry)
                                              property-entry
                                              (car property-entry)))
                                         (property-name
                                          (if (symbolp property-entry)
                                              property-entry
                                              (cadr property-entry))))
                                     `(,var-name
                                       (property-value ,sh ',property-name))))
                                 properties)
         ,@body))))
