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
                                      ((name 'std-property))))
(defun property-name (property)
  (property-value property 'name))
(defun (setf property-name) (new-value property)
  (setf (property-value property 'name) new-value))

(defvar =standard-property= (gensym "=STANDARD-PROPERTY="))

;;;
;;; Internals
;;;

(defvar *property-vector-initial-size* 5
  "The initial size for a sheep's property vector.")

(defvar *property-vector-grow-ratio* 5
  "The ratio by which the property vector is expanded when full.")

(symbol-macrolet ((%properties (%sheep-direct-properties sheep)))

  (defun %create-property-vector (sheep)
    "Sets SHEEP's property vector to a (simple-vector `*property-vector-initial-size*')."
    (setf %properties (make-array *property-vector-initial-size* :initial-element nil)))

  (defun %property-vector-full-p (sheep)
    "A property vector is full when all elements are non-NIL." (aand %properties
          (find nil it :test #'eq)))

  (defun %enlarge-property-vector (sheep)
    (let ((old-vector %properties))
      (setf %properties
            (make-array (* *property-vector-grow-ratio* (length old-vector))
                        :initial-element nil))
      (dotimes (i (length old-vector))
        (setf (svref %properties i) (svref old-vector i))))
    (values))

  (defun %add-property-cons (sheep property-metaobject value)
    (let ((properties %properties))
      (if properties
          (when (%property-vector-full-p sheep)
            (%enlarge-property-vector sheep)
            (setf properties %properties))
          (progn (%create-property-vector sheep)
                 (setf properties %properties)))
      (unless (find property-metaobject properties :key #'property-name :test #'eq)
        (dotimes (i (length properties))
          (unless (svref properties i)
            (return (setf (svref properties i) (cons property-metaobject value)))))))
    sheep)

  (defun %get-property-cons (sheep property-name)
    (find property-name %properties :test #'eq :key (fun (property-name (car _)))))

  (defun %remove-property-cons (sheep property-name)
    (awhen (position property-name %properties
                     :key (fun (property-name (car _))) :test #'eq)
      (setf (svref %properties it) nil))
    sheep))

(defun %direct-property-value (sheep property-name)
  (cdr (%get-property-cons sheep property-name)))
(defun (setf %direct-property-value) (new-value sheep property-name)
  (setf (cdr (%get-property-cons sheep property-name)) new-value))

(defun %direct-property-metaobject (sheep property-name)
  (car (%get-property-cons sheep property-name)))
(defun (setf %direct-property-metaobject) (new-value sheep pname)
  (setf (car (%get-property-cons sheep pname)) new-value))

;;;
;;; Existential
;;;
(defun has-direct-property-p (sheep property-name)
  "Returns T if SHEEP has a property called PROPERTY-NAME as a direct property.
NIL otherwise."
  (when (%get-property-cons sheep property-name) t))

(defun has-property-p (sheep property-name)
  "Returns T if calling PROPERTY-VALUE on SHEEP using the same property-name
would yield a value (i.e. not signal an unbound-property condition)."
  (some (fun (has-direct-property-p _ property-name))
        (sheep-hierarchy-list sheep)))

;; TODO - The way adding/removing properties is done needs to be thought out better.
;;        Right now, doing (defspawn (something) ((var "value"))) would create that
;;        property locally, which could become problematic, specially wrt to how much
;;        space I was hoping to save by adding fresh local property metasheeple only
;;        when they're non-existent in the hierarchy-list, or when add-property is used.
;;
;;        A good part of this issue will probably be resolved when there's a clean,
;;        straightforward API for adding/removing properties. The current interface
;;        is suboptimal, needless to say.
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
  "Removes SHEEP's direct property named PROPERTY-NAME. Signals an error if there is no such
direct property. Returns SHEEP."
  (if (has-direct-property-p sheep property-name)
      (prog1 sheep (%remove-property-cons sheep property-name))
      (error "Cannot remove property: ~A is not a direct property of ~A" property-name sheep)))

(defun remove-all-direct-properties (sheep)
  "Wipes out all direct properties and their values from SHEEP."
  (setf (%sheep-direct-properties sheep) nil))

;;; Value
(defun direct-property-value (sheep property-name)
  "Returns the property-value set locally in SHEEP for PROPERTY-NAME.
If the value is non-local (is delegated or does not exist in the hierarchy list),
a condition of type UNBOUND-DIRECT-PROPERTY condition is signalled."
  ;;  (declare (inline %sheep-direct-properties %get-property-cons)) ;commented until after testing
  (awhen (%sheep-direct-properties sheep)
    (cdr (or (%get-property-cons sheep property-name)
             (error 'unbound-property
                    :sheep sheep :property-name property-name)))))

(defun property-value (sheep property-name)
  "Returns a property-value that is not necessarily local to SHEEP."
  (property-value-with-hierarchy-list sheep property-name))

(defun property-value-with-hierarchy-list (sheep property-name)
  (map nil (fun (when (has-direct-property-p _ property-name)
                  (return-from property-value-with-hierarchy-list
                    (direct-property-value _ property-name))))
       (sheep-hierarchy-list sheep))
  (error 'unbound-property :sheep sheep :property-name property-name))

(defun (setf property-value) (new-value sheep property-name)
  "Sets NEW-VALUE as the value of a direct-property belonging to SHEEP, named
PROPERTY-NAME. If the property does not already exist anywhere in the hierarchy list, an error
is signaled."
  (cond ((has-direct-property-p sheep property-name)
         (setf (%direct-property-value sheep property-name) new-value))
        ((has-property-p sheep property-name)
         ;; We place a restriction on the user that a property metaobject
         ;; itself cannot be side-effected. That restriction allows us,
         ;; in the common case of a property already existing in the hierarchy,
         ;; to reuse the property metaobject by just adding a pointer to it locally.
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
  (or (find-if (fun (has-direct-property-p _ property-name)) (sheep-hierarchy-list sheep))
      (when errorp (error 'unbound-property :sheep sheep :property-name property-name))))

(defun property-metaobject-p (obj)
  (and (sheepp obj)
       (find =standard-property= (sheep-hierarchy-list obj))))

(defun direct-property-metaobject (sheep property-name &optional errorp)
  "Returns the direct local metaobject for a property named PROPERTY-NAME."
  (or (%direct-property-metaobject sheep property-name)
      (when errorp (error 'unbound-property :sheep sheep :property-name property-name))))

(defun sheep-direct-properties (sheep)
  "Returns a set of direct property definition metaobjects."
  (map 'list #'car (%sheep-direct-properties sheep)))

(defun available-properties (sheep)
  "Returns a list of property objects describing all properties available to SHEEP, including
inherited ones."
  (let* ((direct-properties (sheep-direct-properties sheep))
         (avail-property-names (mapcar (fun (property-name _))
                                       (remove-duplicates
                                        (flatten
                                         (append direct-properties
                                                 (mapcar #'available-properties
                                                         (sheep-parents sheep))))
                                        :key #'property-name))))
    (mapcar (fun (direct-property-metaobject (property-owner sheep _ nil) _))
            avail-property-names)))

(defun property-summary (sheep &optional (stream *standard-output*))
  "Provides a pretty-printed representation of SHEEP's available properties."
  (format stream
          "~&Sheep: ~A~%Properties:~% ~{~{~&~3TName: ~13T~A~%~3TValue: ~13T~S~%~
           ~3TOwner: ~13T~A~%~%~}~}"
          sheep (mapcar (fun (list (property-name _)
                                   (property-value sheep (property-name _))
                                   (property-owner sheep (property-name _))))
                        (available-properties sheep))))

(defun direct-property-summary (sheep &optional stream)
  "Provides a pretty-printed representation of SHEEP's direct properties."
  (format stream
          "~&Sheep: ~A~%~
           Direct Properties: ~%~%~
           ~{~{~&~3TName: ~A~%~3TValue: ~S~%~%~}~}"
          sheep (mapcar (fun (list (property-name _)
                                   (direct-property-value sheep (property-name _))))
                        (sheep-direct-properties sheep))))
