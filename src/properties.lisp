;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; indent-tabs-mode: nil -*-

;;;; This file is part of Sheeple

;;;; properties.lisp
;;;;
;;;; Property access, inspection, and management stuff, for the most part.
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :sheeple)

;;;
;;; Internals
;;;
(defun %direct-property-value (object property-name)
  (let ((index (position property-name (mold-properties (%object-mold object)))))
    (when index
      (svref (%object-property-values object) (the fixnum index)))))

(defun (setf %direct-property-value) (new-value object property-name)
  (let ((index (position property-name (mold-properties (%object-mold object)))))
    (setf (svref (%object-property-values object) index) new-value)))

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
                     &key (reader nil readerp)
                     (writer nil writerp)
                     accessor)
  "Adds a property named PROPERTY-NAME to OBJECT, initialized with VALUE."
  ;; TODO - this needs a solid transition thing
  (prog1 object
    (assert (symbolp property-name))
    (when (has-direct-property-p object property-name)
      (cerror "Remove existing property." "~A already has a direct property named ~A."
              object property-name)
      (remove-property object property-name))
    (change-node object (ensure-transition (%object-mold object)
                                           property-name))
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
      (prog1 object (change-node object
                                 (ensure-mold (object-parents object)
                                              (remove property-name
                                                      (object-direct-properties object)))))
      (error "Cannot remove property: ~A is not a direct property of ~A" property-name object)))

(defun remove-all-direct-properties (object)
  "Wipes out all direct properties and their values from OBJECT."
  ;; TODO - make sure this change-node thing is solid.
  (change-node object (mold-transition (transition-mold (%object-transition object))))
  object)

;;; Value
(defun direct-property-value (object property-name)
  "Returns the property-value set locally in OBJECT for PROPERTY-NAME.
If the value is non-local (is delegated or does not exist in the hierarchy list),
a condition of type UNBOUND-DIRECT-PROPERTY condition is signalled."
  ;; TODO - rewrite this shit
  (if (%object-properties object)
      (aif (%get-property-cons object property-name)
           (cdr it)
           (error 'unbound-property
                  :object object :property-name property-name))
      (error 'unbound-property
             :object object :property-name property-name)))

(defun property-value (object property-name)
  "Returns a property-value that is not necessarily local to OBJECT."
  (property-value-with-hierarchy-list object property-name))

(declaim (inline property-value-with-hierarchy-list))
(defun property-value-with-hierarchy-list (object property-name)
  (aif (find-if (rcurry 'has-direct-property-p property-name) (object-hierarchy-list object))
       (direct-property-value it property-name)
       (error 'unbound-property :object object :property-name property-name)))

(defun (setf property-value) (new-value object property-name)
  "Sets NEW-VALUE as the value of a direct-property belonging to OBJECT, named
PROPERTY-NAME. If the property does not already exist anywhere in the hierarchy list, an error
is signaled."
  ;; TODO - This -also- needs to be involved in the mold-management process.
  (cond ((has-direct-property-p object property-name)
         (setf (%direct-property-value object property-name) new-value))
        ((has-property-p object property-name)
         (change-node object property-name)
         (setf (%direct-property-value object property-name) new-value))
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
  "Returns a set of direct property definition metaobjects."
  (awhen (%object-properties object)
    (loop for prop across it when prop collect (car prop))))

(defun available-properties (object)
  "Returns a list of property objects describing all properties available to OBJECT, including
inherited ones."
  (delete-duplicates (append (object-direct-properties object)
                             (mapcan 'available-properties (object-parents object)))))

(defun property-summary (object &optional (stream *standard-output*))
  "Provides a pretty-printed representation of OBJECT's available properties."
  (format stream
          "~&Object: ~A~%Properties:~% ~{~{~&~3TName: ~13T~A~%~3TValue: ~13T~S~%~
           ~3TOwner: ~13T~A~%~%~}~}"
          object (mapcar (fun (let ((pname _))
                               (list pname
                                     (property-value object pname)
                                     (property-owner object pname))))
                        (available-properties object))))

(defun direct-property-summary (object &optional stream)
  "Provides a pretty-printed representation of OBJECT's direct properties."
  (format stream
          "~&Object: ~A~%~
           Direct Properties: ~%~%~
           ~{~{~&~3TName: ~A~%~3TValue: ~S~%~%~}~}"
          object (mapcar (fun (let ((pname _))
                               (list pname
                                     (direct-property-value object pname))))
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
