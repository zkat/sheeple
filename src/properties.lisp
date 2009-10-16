;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; indent-tabs-mode: nil -*-

;;;; This file is part of Sheeple

;;;; properties.lisp
;;;;
;;;; Property access, inspection, and management stuff, for the most part.
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :sheeple)

;;;
;;; Existential
;;;
(defun has-direct-property-p (object property-name)
  "Returns T if OBJECT has a property called PROPERTY-NAME as a direct property.
NIL otherwise."
  (when (find property-name (mold-properties (%object-mold object))) t))

(defun has-property-p (object property-name)
  "Returns T if calling PROPERTY-VALUE on OBJECT using the same property-name
would yield a value (i.e. not signal an unbound-property condition)."
  (some (rcurry 'has-direct-property-p property-name)
        (object-hierarchy-list object)))

(defun add-property (object property-name value
                     &key (reader nil readerp) (writer nil writerp) accessor)
  "Adds a property named PROPERTY-NAME to OBJECT, initialized with VALUE."
  ;; TODO - this needs a solid transition thing
  (prog1 object
    (assert (symbolp property-name))
    (when (has-direct-property-p object property-name)
      (cerror "Remove existing property." "~A already has a direct property named ~A."
              object property-name)
      (remove-property object property-name))
    (change-mold object (ensure-transition (%object-mold object) property-name))
    (let ((position (position property-name (mold-properties (%object-mold object)))))
      (setf (svref (%object-property-values object) position) value))
    (when reader (add-reader-to-object reader property-name object))
    (when writer (add-writer-to-object writer property-name object))
    (when accessor
      (let ((accessor-name (if (eq t accessor) property-name accessor)))
        (unless (and readerp (null reader))
          (add-reader-to-object accessor-name property-name object))
        (unless (and writerp (null writer))
          (add-writer-to-object `(setf ,accessor-name) property-name object))))))

(defun remove-property (object property-name)
  "Removes OBJECT's direct property named PROPERTY-NAME. Signals an error if there is no such
direct property. Returns OBJECT."
  (if (has-direct-property-p object property-name)
      ;; TODO fuckit, something like this... -ish
      (prog1 object
        (change-mold object
                     (ensure-mold (object-parents object)
                                  (remove property-name
                                          (object-direct-properties object)))))
      (error "Cannot remove property: ~A is not a direct property of ~A" property-name object)))

(defun remove-all-direct-properties (object)
  "Wipes out all direct properties and their values from OBJECT."
  (change-mold object (ensure-mold (object-parents object) nil))
  object)

;;; Value
(defun direct-property-value (object property-name)
  "Returns the property-value set locally in OBJECT for PROPERTY-NAME.
If the value is non-local (is delegated or does not exist in the hierarchy list),
a condition of type UNBOUND-PROPERTY condition is signalled."
  (aif (position property-name (mold-properties (%object-mold object)))
       (svref (%object-property-values object) it)
       (error 'unbound-property :object object :property-name property-name)))

(defun property-value (object property-name)
  "Returns a property-value that is not necessarily local to OBJECT."
  (property-value-with-hierarchy-list object property-name))

(defun property-value-with-hierarchy-list (object property-name)
  "Crawls OBJECT's hierarchy list until it finds an object in the hierarchy with PROPERTY-NAME
as a direct property. When it finds one, it returns the direct-property-value of that property,
called on that object. If no object is found in the hierarchy-list with a valid direct-property,
a condition of type UNBOUND-PROPERTY is signaled."
  (acond ((position property-name (mold-properties (%object-mold object)) :test 'eq)
          (svref (%object-property-values object) it))
         ((loop for ancestor in (mold-hierarchy (%object-mold object))
               when (find property-name (mold-properties (%object-mold ancestor)) :test 'eq)
               return ancestor)
          (let ((index (position property-name (mold-properties (%object-mold it)) :test 'eq)))
            (svref (%object-property-values it) index)))
         (t (error 'unbound-property :object object :property-name property-name))))

(defun (setf property-value) (new-value object property-name)
  "Sets NEW-VALUE as the value of a direct-property belonging to OBJECT, named
PROPERTY-NAME. If the property does not already exist anywhere in the hierarchy list, an error
is signaled."
  (acond ((position property-name (mold-properties (%object-mold object)) :test 'eq)
          (setf (svref (%object-property-values object) it) new-value))
         ((loop for ancestor in (mold-hierarchy (%object-mold object))
             when (find property-name (mold-properties (%object-mold ancestor)))
             return ancestor)
          (change-mold object (ensure-transition (%object-mold object) property-name))
          (let ((index (position property-name (mold-properties (%object-mold object)) :test 'eq)))
            (setf (svref (%object-property-values object) index) new-value)))
         (t (cerror "Add the property locally" 'unbound-property
                    :object object
                    :property-name property-name)
            (add-property object property-name new-value)))
  new-value)

;;; Reflection API
(defun property-owner (object property-name &optional errorp)
  "Returns the object object with a direct-property called PROPERTY-NAME from which OBJECT inherits
its value. If ERRORP is T, an error is signaled if the property is unbound. Otherwise, NIL is
returned."
  (or (find-if (rcurry 'has-direct-property-p property-name) (object-hierarchy-list object))
      (when errorp (error 'unbound-property :object object :property-name property-name))))

(defun object-direct-properties (object)
  "Returns a list of the names of OBJECT's direct properties -- ie, only ones which have been
set directly in OBJECT using (setf property-value). The consequences of side-effecting this
returned list are undefined."
  (coerce (mold-properties (%object-mold object)) 'list))

(defun available-properties (object)
  "Returns a list of the names of all properties available to OBJECT, including inherited ones."
  (delete-duplicates (nconc (coerce (object-direct-properties object) 'list)
                            (mapcan 'available-properties (object-parents object)))))

(defun property-summary (object &optional (stream *standard-output*))
  "Provides a pretty-printed representation of OBJECT's available properties."
  (format stream
          "~&Object: ~A~%Properties:~% ~{~{~&~3TName: ~13T~A~%~3TValue: ~13T~S~%~
           ~3TOwner: ~13T~A~%~%~}~}"
          object (mapcar (fun (list _
                                    (property-value object _)
                                    (property-owner object _)))
                         (available-properties object))))

(defun direct-property-summary (object &optional stream)
  "Provides a pretty-printed representation of OBJECT's direct properties."
  (format stream
          "~&Object: ~A~%~
           Direct Properties: ~%~%~
           ~{~{~&~3TName: ~A~%~3TValue: ~S~%~%~}~}"
          object (mapcar (fun (list _ (direct-property-value object _)))
                         (object-direct-properties object))))

;;; Convenience
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
