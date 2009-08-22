;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; indent-tabs-mode: nil -*-

;;;; This file is part of Sheeple

;;;; properties.lisp
;;;;
;;;; Property access, inspection, and management stuff, for the most part.
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :sheeple)

;;;
;;; Property spec object
;;;
(defparameter the-std-property-form '(defproto =standard-property= ()
                                      ((property-name 'std-property))))

(defvar =standard-property= (gensym "=STANDARD-PROPERTY="))

;;;
;;; Internals
;;;
(defun %add-property-cons (sheep property-metaobject value)
  ;; treating it as a list for now...
  (push (cons property-metaobject value) (%sheep-direct-properties sheep)))

(defun %get-property-cons (sheep property-name)
  (find prop (%sheep-direct-properties sh)
        :test #'eq :key (fn (property-name (car _)))))

(defun %remove-property-cons (sheep property-name)
  (deletef (%sheep-direct-properties) (%get-property-cons sheep property-name)))

(defun %direct-property-value (sheep property-name)
  (cdr (%get-property-cons sheep property-name)))
(defun (setf %direct-property-value) (new-value sheep property-name)
  (setf (cdr (%get-property-cons sh prop)) new-value))

(defun %direct-property-metaobject (sheep property-name)
  (car (%get-property-cons sheep property-name)))
(defun (setf %direct-property-metaobject) (new-value sheep pname)
  (setf (car (%get-property-cons sheep pname)) new-value))


(defun add-property (sheep property-name value)
  "Adds a property named PROPERTY-NAME to SHEEP, initialized with VALUE."
  (assert (symbolp property-name))
  (when (has-direct-property-p sheep property-name)
    (cerror "Add property anyway." "~A already has a direct property named ~A." 
            sheep property-name))
  (let ((property-metaobject (defsheep (=standard-property=) ((name property-name)))))
    (%add-property-cons sheep property-metaobject value)))

;; TODO - remove-property should look at the property metaobject and remove any replies for
;;        accessors that it points to. This will have to wait until reply-undefinition works
;;        again, though. -syko
;; Addendum - this may not count anymore. I'm not sure if we'll still have the metaobjects
;;            keep track of this info
(defun remove-property (sheep property-name)
  "If PROPERTY-NAME is a direct property of SHEEP, this function removes it. If PROPERTY-NAME
is inherited from one of SHEEP's parents, or if PROPERTY-NAME does not exist in SHEEP's hierarchy
list, an error is signaled. This function returns SHEEP after property removal."
  (if (has-direct-property-p sheep property-name)
      (progn (%remove-property-cons sheep property-name) sheep)
      (error "Cannot remove property: ~A is not a direct property of ~A" name sheep)))

(defun remove-all-direct-properties (sheep)
  "Wipes out all direct properties and their values from SHEEP."
  (setf (%sheep-direct-properties) nil) sheep)

(defun has-direct-property-p (sheep property-name)
  "Returns T if SHEEP has a property called PROPERTY-NAME as a direct property.
NIL otherwise."
  (when (%get-property-cons sheep property-name) t))

(defun has-property-p (sheep property-name)
  "Returns T if calling PROPERTY-VALUE on SHEEP using the same property-name
would yield a value (i.e. not signal an unbound-property condition)."
  (some (fn (has-direct-property-p _ property-name))
        (sheep-hierarchy-list sheep)))

;;; Value
(defun direct-property-value (sheep property-name)
  "Returns the property-value set locally in SHEEP for PROPERTY-NAME.
If the value is non-local (is delegated or does not exist in the hierarchy list),
a condition of type UNBOUND-DIRECT-PROPERTY condition is signalled."
  ;;  (declare (inline %sheep-direct-properties %get-property-cons)) ;commented until after testing
  (awhen (%sheep-direct-properties sheep)
    (cdr (or (%get-property-cons sheep property-name)
             (error 'unbound-direct-property
                    :sheep sheep :property-name (property-name property))))))

(defun property-value (sheep property-name)
  "Returns a property-value that is not necessarily local to SHEEP."
  (property-value-with-hierarchy-list sheep property-name))

(defun property-value-with-hierarchy-list (sheep property-name)
  (let ((hl (sheep-hierarchy-list sheep)))
    (or (loop for sheep in hl
           do (let ((hasp (has-direct-property-p sheep property-name)))
                (when hasp
                  (return-from property-value-with-hierarchy-list
                    (direct-property-value sheep property-name)))))
        (error 'unbound-property :sheep sheep :property-name property-name))))

(defun (setf property-value) (new-value sheep property-name)
  "Sets NEW-VALUE as the value of a direct-property belonging to SHEEP, named
PROPERTY-NAME. If the property does not already exist anywhere in the hierarchy list, an error
is signaled."
  (cond ((has-direct-property-p sheep property-name)
         (setf (%direct-property-value sheep property-name new-value)))
        ((has-property-p sheep property-name)
         (let ((owner-prop-mo (car (%get-property-cons (property-owner sheep property-name)
                                                       property-name))))
           (%add-property-cons sheep owner-prop-mo new-value)))
        (t (cerror "Add the property locally" 'unbound-property
                   :sheep sheep 
                   :property-name property-name)
           (add-property sheep property-name new-value))))

;;; Reflection API
(defun property-owner (sheep property-name &optional errorp)
  "Returns the sheep object with a direct-property called PROPERTY-NAME from which SHEEP inherits
its value. If ERRORP is T, an error is signaled if the property is unbound. Otherwise, NIL is
returned."
  (or (find-if (fn (has-direct-property-p _ property-name)) (sheep-hierarchy-list sheep))
      (when errorp (error 'unbound-property :sheep sheep :property-name property-name))))

(defun direct-property-metaobject (sheep property-name)
  "Returns the direct local metaobject for a property named PROPERTY-NAME."
  (%direct-property-metaobject sheep property-name))

(defun sheep-direct-properties (sheep)
  "Returns a set of direct property definition metaobjects."
  ;; note - this will have to be changed if %sheep-direct-properties switches to vectors.
  (mapcar #'car (%sheep-direct-properties sheep)))

#+nil(defun available-properties (sheep)
  "Returns a list of property objects describing all properties available to SHEEP, including
inherited ones."  
  (let* ((direct-properties (sheep-direct-properties sheep))
         (avail-property-names (mapcar (fn (property-name _))
                                       (remove-duplicates
                                        (flatten
                                         (append direct-properties
                                                 (mapcar #'available-properties
                                                         (sheep-parents sheep))))
                                        :key #'property-name))))
    (mapcar (fn (direct-property-metaobject (property-owner sheep _ nil) _))
            avail-property-names)))

(defun property-summary (sheep &optional (stream *standard-output*))
  "Provides a pretty-printed representation of SHEEP's available properties."
  (format stream
          "~&Sheep: ~A~%Properties:~% ~{~{~&~3TName: ~13T~A~%~3TValue: ~13T~S~%~
           ~3TOwner: ~13T~A~%~%~}~}"
          sheep (mapcar (fn (list (property-name _)
                                  (property-value sheep (property-name _))
                                  (property-owner sheep (property-name _))))
                        (available-properties sheep))))

(defun direct-property-summary (sheep &optional stream)
  "Provides a pretty-printed representation of SHEEP's direct properties."
  ;; todo
  (format stream
          "~&Sheep: ~A~%~
           Direct Properties: ~%~%~
           ~{~{~&~3TName: ~A~%~3TValue: ~S~%~%~}~}"
          sheep (mapcar (fn (list (property-name _)
                                  (direct-property-value sheep (property-name _))))
                        (sheep-direct-properties sheep))))
