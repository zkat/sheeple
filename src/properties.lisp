;; This file is part of Sheeple

;; properties.lisp
;;
;; Property access, inspection, and management stuff, for the most part.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :sheeple)

(declaim (optimize (speed 3) (debug 1) (safety 1)))
(defparameter *secret-unbound-value* (gensym))
;;;
;;; Property spec object
;;;
(defclass property-spec ()
  ((name :initarg :name :accessor property-spec-name)
   (allocation :initarg :allocation :accessor property-spec-allocation)
   (readers :initform nil :initarg :readers :accessor property-spec-readers)
   (writers :initform nil :initarg :writers :accessor property-spec-writers)))

(defun property-spec-equal-p (spec1 spec2)
  (with-slots ((name1 name) (allocation1 allocation) (readers1 readers) (writers1 writers)) spec1
    (with-slots ((name2 name) (allocation2 allocation) (readers2 readers) (writers2 writers)) spec2
      (and (equal name1 name2)
           (equal allocation1 allocation2)
           (equal readers1 readers2)
           (equal writers1 writers2)))))

(defmethod print-object ((property-spec property-spec) stream)
  (print-unreadable-object (property-spec stream :identity t)
    (format stream "Property-Spec ~A" (property-spec-name property-spec))))
;;;
;;; API
;;;
;;; - Generic functions that define the basic interface for interacting with sheep properties.

;;; Existential functions
(defgeneric add-property (sheep property-name value &key)
  (:documentation "Adds a property named PROPERTY-NAME to SHEEP, initalized with VALUE."))

(defgeneric add-property-metaobject-to-sheep (sheep property)
  (:documentation "Adds PROPERTY as a registered property to SHEEP. Clients must -not-
override the primary method for this generic function."))

(defgeneric property-spec-class (sheep &key)
  (:documentation "Determine the class of the property-spec to be added."))

(defgeneric remove-property (sheep property)
  (:documentation "If PROPERTY-NAME is a direct property of SHEEP, this function removes it. If
PROPERTY-NAME is inherited from one of SHEEP's parents, or if PROPERTY-NAME does not exist in SHEEP's
hierarchy list, an error is signaled. This function returns SHEEP after property removal."))

(defgeneric remove-all-direct-properties (sheep)
  (:documentation "Wipes out all direct properties and their values from SHEEP."))

(defgeneric has-direct-property-p (sheep property-name)
  (:documentation "Returns T if SHEEP has a property called PROPERTY-NAME as a direct property. 
NIL otherwise."))

(defgeneric has-property-p (sheep property-name)
  (:documentation "Returns T if calling PROPERTY-VALUE on SHEEP using the same property-name
would yield a value (i.e. not signal an unbound-property condition)."))

;;; Value
(defgeneric direct-property-value (sheep property)
  (:documentation "Returns the property-value set locally in SHEEP for PROPERTY. 
If the value is non-local (is delegated or does not exist in the hierarchy list), an
UNBOUND-PROPERTY condition is signaled."))

(defgeneric property-value (sheep property-name)
  (:documentation "Returns a property-value that is not necessarily local to SHEEP."))

(defgeneric (setf property-value) (new-value sheep property-name)
  (:documentation "Sets NEW-VALUE as the value of a direct-property belonging to SHEEP, named
PROPERTY-NAME. If the property does not already exist anywhere in the hierarchy list, an error
is signaled."))

;;; Reflection API
(defgeneric property-owner (sheep property-name &optional errorp)
  (:documentation "Returns the sheep object with a direct-property called PROPERTY-NAME from which
SHEEP inherits its value. If ERRORP is T, an error is signaled if the property is unbound. Otherwise,
NIL is returned."))

(defgeneric available-properties (sheep)
  (:documentation "Returns a list of property-spec objects describing all properties available to
SHEEP, including inherited ones."))

(defgeneric direct-property-spec (sheep property-name)
  (:documentation "Returns the direct local spec for a property named PROPERTY-NAME."))

(defgeneric property-summary (sheep &optional stream)
  (:documentation "Provides a pretty-printed representation of SHEEP's available properties."))

(defgeneric direct-property-summary (sheep &optional stream)
  (:documentation "Provides a pretty-printed representation of SHEEP's direct properties."))

;;;
;;; Methods
;;;

(defmethod property-spec-class ((sheep standard-sheep) &key)
  (declare (ignore allocation value readers writers))
  (find-class 'property-spec))


(defmethod add-property-spec-to-sheep ((sheep standard-sheep) (property property-spec))
  (let ((pname (property-spec-name property)))
    (setf (gethash pname (sheep-property-spec-table sheep))
          property)))

;;; Existential
(defmethod add-property ((sheep standard-sheep) property-name value 
                         &key readers writers (make-accessor-p t)
                         (allocation :sheep))
  "Allocates VALUE as one of SHEEP's direct-properties. :allocation determines where the value
is actually allocated. For the standard method, anything other than :sheep signals an error."
  ;; What does this actually have to do, overall?
  ;; 1. Create and register a property-spec instance with the sheep.
  ;; 2. Set the value.
  ;; 3. Add readers/writers/etc.
  (if (eq allocation :sheep)
      (let ((property-table (sheep-property-value-table sheep)))
        (when (has-direct-property-p sheep property-name)
          (warn "~A already has a direct property named ~A. Overwriting." sheep property-name))
        (add-property-spec-to-sheep sheep (make-instance (property-spec-class sheep)
                                                         :name property-name
                                                         :allocation allocation))
        (setf (property-value sheep property-name) value)
        (let ((property-spec (gethash property-name (sheep-property-spec-table sheep))))
          (when readers
            (add-readers-to-sheep readers property-name sheep)
            (pushnew readers (property-spec-readers property-spec)))
          (when writers
            (add-writers-to-sheep writers property-name sheep))
          (pushnew writers (property-spec-writers property-spec))
          (when make-accessor-p
            (add-readers-to-sheep `(,property-name) property-name sheep)
            (pushnew `((setf ,property-name)) (property-spec-readers property-spec) :test #'equal)
            (add-writers-to-sheep `((setf ,property-name)) property-name sheep)
            (pushnew `(,property-name) (property-spec-writers property-spec) :test #'equal)))
        sheep)
      (error "Standard sheep can only have :sheep allocation.")))

(defmethod remove-property ((sheep standard-sheep) (property-name symbol))
  (if property-name
      (remove-property sheep (gethash property-name (sheep-property-spec-table sheep)))
      (error "Cannot remove property: ~A is not a direct property of ~A" property-name sheep)))
(defmethod remove-property ((sheep standard-sheep) (property property-spec))
  (let ((name (property-spec-name property)))
    (if (has-direct-property-p sheep name)
        (progn (remhash name (sheep-property-value-table sheep))
               (remhash name (sheep-property-spec-table sheep))
               sheep)
        (error "Cannot remove property: ~A is not a direct property of ~A" name sheep))))

(defmethod remove-all-direct-properties ((sheep standard-sheep))
  (clrhash (sheep-property-value-table sheep))
  (clrhash (sheep-property-spec-table sheep))
  sheep)

(defmethod has-direct-property-p ((sheep standard-sheep) property-name)
  (nth-value 1 (gethash property-name (sheep-property-spec-table sheep))))

(defmethod has-property-p ((sheep standard-sheep) property-name)
  "Returns T if a property with PROPERTY-NAME is available to SHEEP."
  (handler-case
      (when (property-owner sheep property-name)
        t)
    (unbound-property () nil)))

;;; values
(defmethod direct-property-value ((sheep standard-sheep) (property-name symbol))
  (let ((property (gethash property-name (sheep-property-spec-table sheep))))
    (if property
        (direct-property-value sheep property)
        (error 'unbound-property
               :format-control "~A has no direct property with name ~A"
               :format-args (list sheep property-name)))))
(defmethod direct-property-value ((sheep standard-sheep) (property property-spec))
  (multiple-value-bind (value hasp)
      (gethash (property-spec-name property) (sheep-property-value-table sheep))
    (if hasp
        value
        (error 'unbound-property
               :format-control "~A has no direct property with name ~A"
               :format-args (list sheep (property-spec-name property))))))

(defmethod property-value ((sheep standard-sheep) property-name)
  (property-value-with-hierarchy-list sheep property-name))
(defun property-value-with-hierarchy-list (sheep property-name)
  (let ((hl (sheep-hierarchy-list sheep)))
    (or (loop for sheep in hl
           do (let ((hasp (has-direct-property-p sheep property-name)))
                (when hasp
                  (return-from property-value-with-hierarchy-list
                    (direct-property-value sheep property-name)))))
        (error 'unbound-property
               :format-control "Property ~A is unbound for sheep ~S"
               :format-args (list property-name sheep)))))


(defmethod (setf property-value) (new-value (sheep standard-sheep) (property-name symbol))
  (if (has-property-p sheep property-name)
      (let ((property (gethash property-name (sheep-property-spec-table sheep))))
        (setf (property-value sheep property) new-value))
      (error "Property ~A does not exist for sheep ~A." property-name sheep)))
(defmethod (setf property-value) (new-value (sheep standard-sheep) (property property-spec))
  (let ((property-table (sheep-property-value-table sheep)))
    (setf (gethash (property-spec-name property) property-table) new-value)))

;; Reflection
(defun sheep-direct-properties (sheep)
  "Returns a set of direct property-spec definition metaobjects."
  (loop for pname being the hash-keys of (sheep-property-value-table sheep)
     collect (direct-property-spec sheep pname)))

(defmethod direct-property-spec ((sheep standard-sheep) property-name)
  (unless (has-direct-property-p sheep property-name)
    (signal 'unbound-property))
  (nth-value 0 (gethash property-name (sheep-property-spec-table sheep))))

(defmethod property-owner ((sheep standard-sheep) property-name &optional (errorp t))
  (let ((owner
         (loop for obj in (sheep-hierarchy-list sheep)
            when (has-direct-property-p obj property-name)
            return obj)))
    (or owner
        (if errorp
            (error 'unbound-property
                   :format-control "Property ~A is unbound for sheep ~S"
                   :format-args (list property-name sheep))
            nil))))

(defmethod available-properties ((sheep standard-sheep))
  (let* ((direct-properties (sheep-direct-properties sheep))
         (avail-property-names (mapcar (lambda (p)
                                         (property-spec-name p))
                                       (remove-duplicates
                                        (flatten
                                         (append direct-properties
                                                 (mapcar #'available-properties
                                                         (sheep-parents sheep))))
                                        :key #'property-spec-name))))
    (mapcar (lambda (pname)
              (direct-property-spec (property-owner sheep pname nil) pname))
            avail-property-names)))

(defmethod property-summary ((sheep standard-sheep) &optional (stream *standard-output*))
  (let ((all-properties (available-properties sheep)))
    (format stream
            "~&Sheep: ~A~%Properties:~% ~{~{~&~3TName: ~13T~A~%~3TValue: ~13T~S~%~
             ~3TAllocation: ~13T~A~%~3TReaders: ~13T~A~%~3TWriters: ~13T~A~%~
             ~3TOwner: ~13T~A~%~%~}~}"
            sheep (loop for property in all-properties
                     collect (list (property-spec-name property)
                                   (direct-property-value sheep property)
                                   (property-spec-allocation property)
                                   (property-spec-readers property)
                                   (property-spec-writers property)
                                   (property-owner sheep (property-spec-name property)))))))

(defmethod direct-property-summary ((sheep standard-sheep) &optional (stream *standard-output*))
  (format stream
          "~&Sheep: ~A~%~
           Direct Properties: ~%~%~
           ~{~{~&~3TName: ~A~%~3TValue: ~S~%~
           ~3TAllocation: ~A~%~3TReaders: ~A~%~3TWriters: ~A~%~%~}~}"
          sheep (loop for property in (sheep-direct-properties sheep)
                   collect (list (property-spec-name property)
                                 (property-value sheep property)
                                 (property-spec-allocation property)
                                 (property-spec-readers property)
                                 (property-spec-writers property)))))
