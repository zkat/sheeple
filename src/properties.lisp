;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; indent-tabs-mode: nil -*-

;;;; This file is part of Sheeple

;;;; properties.lisp
;;;;
;;;; Property access, inspection, and management stuff, for the most part.
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :sheeple)

(defun property-position (property-name object)
  (position property-name (mold-properties (%object-mold object)) :test #'eq))

;;;
;;; Base Property API
;;;

(defun direct-property-value (object property-name)
  "Returns the property-value set locally in OBJECT for PROPERTY-NAME. If the
property is not set locally, a condition of type `unbound-property' is signaled."
  (if (std-object-p object)
      (std-direct-property-value object property-name)
      (funcall 'smop:direct-property-value (object-metaobject object) object property-name)))
(defun std-direct-property-value (object property-name)
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

(defun property-value (object property-name)
  "Returns the property-value for PROPERTY-NAME found first in OBJECT's hierarchy list.
If the value does not exist in the hierarchy list, a condition of type `unbound-property'
is signaled."
  (if (std-object-p object)
      (std-property-value object property-name)
      (funcall 'smop:property-value (object-metaobject object) object property-name)))
(defun std-property-value (object property-name)
  (dolist (ancestor (object-precedence-list object)
           (error 'unbound-property :object object :property-name property-name))
    (handler-bind ((unbound-property (fun (go :next))))
      (return (direct-property-value ancestor property-name)))
    :next))

(defun (setf direct-property-value) (new-value object property-name &rest options)
  "Tries to set a direct property value for OBJECT. If it succeeds, returns T, otherwise NIL."
  (if (std-object-p object)
      (apply '(setf std-direct-property-value) new-value object property-name options)
      (apply '(setf smop:direct-property-value) new-value (object-metaobject object) object property-name options)))
(defun (setf std-direct-property-value) (new-value object property-name &rest options)
  (declare (ignore options))
  (awhen (property-position property-name object)
    (setf (svref (%object-property-values object) it) new-value)
    t))

(defun add-direct-property (object property-name &rest options)
  "Adds a direct property to object, which involves any necessary allocation."
  (if (std-object-p object)
      (apply 'std-add-direct-property object property-name options)
      (apply 'smop:add-direct-property (object-metaobject object) object property-name options)))
(defun std-add-direct-property (object property-name &rest options)
  (declare (ignore options))
  (change-mold object (ensure-transition (%object-mold object) property-name)))

(defun (setf property-value) (new-value object property-name &rest options
                              &key (reader nil readerp) (writer nil writerp) accessor)
  "Sets NEW-VALUE as the value of a direct-property belonging to OBJECT, named
PROPERTY-NAME."
  (flet ((maybe-set-prop () (apply '(setf direct-property-value) new-value object property-name options)))
    (or (maybe-set-prop)                                                 ; try to set it
        (progn (apply 'add-direct-property object property-name options) ; couldn't set it, try adding it
               (maybe-set-prop))                                         ; then try setting it again
        (error "Could not set direct property value.")))                 ; bought the farm
  (when options             ; if we know there's no options, we may as well skip all of the checks..
    (when reader (add-reader-to-object reader property-name object))
    (when writer (add-writer-to-object writer property-name object))
    (when accessor
      (let ((accessor-name (if (eq t accessor) property-name accessor)))
        (unless (and readerp (null reader))
          (add-reader-to-object accessor-name property-name object))
        (unless (and writerp (null writer))
          (add-writer-to-object `(setf ,accessor-name) property-name object)))))
  new-value)

(defun property-makunbound (object property-name)
  "Removes OBJECT's direct property named PROPERTY-NAME. Signals an error if there is no such
direct property. Returns OBJECT."
  (if (std-object-p object)
      (std-property-makunbound object property-name)
      (funcall 'smop:property-makunbound (object-metaobject object) object property-name)))
(defun std-property-makunbound (object property-name)
  (if (direct-property-p object property-name)
      (prog1 object
        (change-mold object
                     (ensure-mold (remove property-name
                                          (mold-properties (%object-mold object))))))
      (error 'unbound-property :object object :property-name property-name)))

(defun remove-property (object property-name)
  "Removes OBJECT's direct property named PROPERTY-NAME. Signals an error if there is no such
direct property. Returns OBJECT."
  (warn 'deprecated-feature :feature #'remove-property :version "3.0.2")
  (property-makunbound object property-name))

(defun remove-all-direct-properties (object)
  "Wipes out all direct properties and their values from OBJECT."
  (if (std-object-p object)
      (std-remove-all-direct-properties object)
      (funcall 'smop:remove-all-direct-properties (object-metaobject object) object)))
(defun std-remove-all-direct-properties (object)
  (change-mold object *empty-mold*)
  object)

;;;
;;; Reflection API
;;;

(defun direct-property-p (object property-name)
  "Returns T if OBJECT has a property called PROPERTY-NAME as a direct property.
NIL otherwise."
  (if (std-object-p object)
      (std-direct-property-p object property-name)
      (funcall 'smop:direct-property-p (object-metaobject object) object property-name)))
(defun std-direct-property-p (object property-name)
  (let ((has-property-p t))
    (handler-case (direct-property-value object property-name)
      (unbound-property () (setf has-property-p nil)))
    has-property-p))

(defun property-owner (object property-name)
  "Returns the object, if any, from which OBJECT would fetch the value for PROPERTY-NAME"
  (if (std-object-p object)
      (std-property-owner object property-name)
      (funcall 'smop:property-owner (object-metaobject object) object property-name)))
(defun std-property-owner (object property-name)
  (find-if (rcurry 'direct-property-p property-name) (object-precedence-list object)))

(defun direct-properties (object)
  "Returns a list of the names of OBJECT's direct properties -- ie, only ones which have been
set directly in OBJECT using (setf property-value). The consequences of side-effecting this
returned list are undefined."
  (if (std-object-p object)
      (std-direct-properties object)
      (funcall 'smop:direct-properties (object-metaobject object) object)))
(defun std-direct-properties (object)
  (coerce (mold-properties (%object-mold object)) 'list))

(defun available-properties (object)
  "Returns a list of the names of all properties available to OBJECT, including inherited ones."
  (if (std-object-p object)
      (std-available-properties object)
      (funcall 'smop:available-properties (object-metaobject object) object)))
(defun std-available-properties (object)
  (delete-duplicates (nconc (copy-list (direct-properties object))
                            (mapcan 'available-properties (object-parents object)))))

;;;
;;; Property-related convenience
;;;

;;; Nicknames
(defun object-nickname (object)
  "Returns OBJECT's nickname"
  (property-value object 'nickname))

(defun (setf object-nickname) (new-nickname object)
  "Sets OBJECT's nickname to NEW-NICKNAME"
  (handler-bind ((unbound-property 'continue))
    (setf (property-value object 'nickname) new-nickname)))

;;; DOCUMENTATION
(defmethod documentation ((x object) (doc-type (eql 't)))
  (property-value x 'documentation))

(defmethod (setf documentation) ((new-value string) (x object) (doc-type (eql 't)))
  (handler-bind ((unbound-property 'continue))
    (setf (property-value x 'documentation) new-value)))

;;; DESCRIBE
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
;;; Property symbol-macro
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

;;;
;;; Property Bootstrap
;;;

(macrolet ((set-name (name) `(setf (property-value ,name 'nickname) ',name)))
  (set-name =t=)
  (set-name =standard-object=)
  (set-name =standard-metaobject=))
