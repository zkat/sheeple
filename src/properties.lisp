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
;;; Properties
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

(defgeneric add-property (sheep property-name value &key)
  (:documentation "Adds a property named PROPERTY-NAME to SHEEP, initalized with VALUE."))
(defmethod add-property ((sheep standard-sheep) property-name value 
                         &key readers writers 
                         (make-accessors-p t)
                         (allocation :sheep))
  "Adds a property to SHEEP. Optional Readers and Writers must be a list of
valid function names (in symbol or cons form) that will be used to create responses specialized
on SHEEP. If make-accessors-p is T, the symbol in PROPERTY-NAME will be used to generate accessors
with the format Reader=PROPERTY-NAME, Writer=(SETF PROPERTY-NAME)"
  (if (eq allocation :sheep)
      (let ((property-table (sheep-property-value-table sheep)))
        (when (has-direct-property-p sheep property-name)
          (warn "~A already has a direct property named ~A. Overwriting." sheep property-name))
        (setf (gethash property-name property-table) value))
      (error "Standard sheep can only have :sheep allocation."))
  (let ((property-spec (or (gethash property-name (property-spec-table sheep))
                           (make-instance 'property-spec :name property-name))))
   (when readers
     (add-readers-to-sheep readers property-name sheep)
     (pushnew readers (property-spec-readers sheep)))
   (when writers
     (add-writers-to-sheep writers property-name sheep))
     (pushnew writers (property-spec-writers sheep))
   (when make-accessors-p
     (add-readers-to-sheep `(,property-name) property-name sheep)
     (pushnew readers `((setf ,property-name)) :test #'equal)
     (add-writers-to-sheep `((setf ,property-name)) property-name sheep)
     (pushnew writers `(,property-name) :test #'equal)))
  sheep)

(defgeneric has-direct-property-p (sheep property-name)
  (:documentation "Returns T if SHEEP has a direct property with name PROPERTY-NAME, NIL otherwise."))
(defmethod has-direct-property-p ((sheep standard-sheep) property-name)
  (multiple-value-bind (value has-p)
      (gethash property-name (sheep-property-value-table sheep))
    value
    has-p))

(defgeneric remove-property (sheep property-name)
  (:documentation "If PROPERTY-NAME is a direct property of SHEEP, this function removes it. If
PROPERTY-NAME is inherited from one of SHEEP's parents, or if PROPERTY-NAME does not exist in SHEEP's
hierarchy list, an error is signaled. This function returns SHEEP after property removal."))
(defmethod remove-property ((sheep standard-sheep) property-name)
  (if (has-direct-property-p sheep property-name)
      (progn (remhash property-name (sheep-property-value-table sheep))
             sheep)
      (error "Cannot remove property: ~A is not a direct property of ~A" property-name sheep)))

(defgeneric remove-all-direct-properties (sheep))
(defmethod remove-all-direct-properties ((sheep standard-sheep))
  (clrhash (sheep-property-value-table sheep))
  sheep)

(defgeneric direct-property-value (sheep property-name)
  (:documentation "Returns the property-value set locally in SHEEP for PROPERTY-NAME. If the value
is non-local (is delegated or does not exist in the hierarchy list), an UNBOUND-PROPERTY is signaled."))
(defmethod direct-property-value ((sheep standard-sheep) property-name)
  (unless (symbolp property-name)
    (error "Property-name must be a symbol"))
  (multiple-value-bind (value hasp)
      (gethash property-name (sheep-property-value-table sheep))
    (if hasp
        value
        (error 'unbound-property
               :format-control "~A has no direct property with name ~A"
               :format-args (list sheep property-name)))))

(defgeneric property-value (sheep property-name)
  (:documentation ""))
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

(defgeneric has-property-p (sheep property-name))
(defmethod has-property-p ((sheep standard-sheep) property-name)
  "Returns T if a property with PROPERTY-NAME is available to SHEEP."
  (handler-case
      (when (property-owner sheep property-name)
        t)
    (unbound-property () nil)))

(defgeneric (setf property-value) (new-value sheep property-name)
  (:documentation "Sets NEW-VALUE as the value of a direct-property belonging to SHEEP, named
PROPERTY-NAME. If the property does not already exist anywhere in the hierarchy list, an error
is signaled."))
(defmethod (setf property-value) (new-value (sheep standard-sheep) property-name)
  (unless (symbolp property-name)
    (error "Property-name must be a symbol"))
  (if (has-property-p sheep property-name)
      (let ((property-table (sheep-property-value-table sheep)))
        (setf (gethash property-name property-table) new-value))
      (error "Property ~A does not exist for sheep ~A." property-name sheep)))

;; Reflection
(defmethod print-object ((property-spec property-spec) stream)
  (print-unreadable-object (property-spec stream :identity t)
    (format stream "Property-Spec ~A" (property-spec-name property-spec))))
(defun sheep-direct-properties (sheep)
  "Returns a set of direct property-spec definition metaobjects."
  (loop for pname being the hash-keys of (sheep-property-value-table sheep)
     collect (direct-property-spec sheep pname)))

(defun direct-property-spec (sheep property-name)
  (unless (has-direct-property-p sheep property-name)
    (error "No such property"))
  (gethash property-name (property-spec-table sheep)))

(defgeneric property-owner (sheep property-name &optional errorp)
  (:documentation "Returns the sheep object with a direct-property called PROPERTY-NAME from which
SHEEP inherits its value. If ERRORP is T, an error is signaled if the property is unbound. Otherwise,
NIL is returned."))
(defmethod property-owner ((sheep standard-sheep) property-name &optional (errorp t))
  (let ((owner
         (loop for obj in (sheep-hierarchy-list sheep)
            when (has-direct-property-p obj property-name)
            return obj)))
    (if owner
        owner
        (if errorp
            (error 'unbound-property
                   :format-control "Property ~A is unbound for sheep ~S"
                   :format-args (list property-name sheep))
            nil))))

(defgeneric available-properties (sheep)
  (:documentation "Returns a list of property-spec objects describing all properties available to
SHEEP, including inherited ones."))
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

(defgeneric property-summary (sheep &optional stream)
  (:documentation "Provides a pretty-printed representation of SHEEP's available properties."))
(defmethod property-summary ((sheep standard-sheep) &optional (stream *standard-output*))
  (let ((all-properties (available-properties sheep)))
    (format stream
            "~&Sheep: ~A~%Properties:~% ~{~{~&~3TName: ~13T~A~%~3TValue: ~13T~S~%~
             ~3TAllocation: ~13T~A~%~3TReaders: ~13T~A~%~3TWriters: ~13T~A~%~
             ~3TOwner: ~13T~A~%~%~}~}"
            sheep (loop for property in all-properties
                     collect (list (property-spec-name property)
                                   (direct-property-value shee property)
                                   (property-spec-allocation property)
                                   (property-spec-readers property)
                                   (property-spec-writers property)
                                   (property-owner sheep (property-spec-name property)))))))

(defgeneric direct-property-summary (sheep &optional stream)
  (:documentation "Provides a pretty-printed representation of SHEEP's direct properties."))
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

;;;
;;; Memoization
;;;
;; TODO: Reimplement memoization of property-access, if I find a nice way to do it.
;;       Nice, of course, being "thread-compatible, and lightweight on the SETF front"
;;       Something similar to reply caching might work, but I'm not sure anymore.

;; (defun property-value-with-memoized-owner (sheep property-name)
;;   ;; Find who the owner is...
;;   (multiple-value-bind (prop-owner has-p)
;;       (gethash property-name (sheep-property-owners sheep))
;;     (if has-p
;; 	;; Get the actual value from that owner..
;; 	(multiple-value-bind (value has-p)
;; 	    (gethash property-name (sheep-property-value-table prop-owner))
;; 	  (if has-p
;; 	      value
;; 	      (error 'unbound-property
;; 		     :format-control "Property ~A is unbound for sheep ~S"
;; 		     :format-args (list property-name sheep))))
;; 	(error 'unbound-property
;; 	       :format-control "Property ~A is unbound for sheep ~S"
;; 	       :format-args (list property-name sheep)))))

;; (defun %property-value-owner (sheep property-name)
;;   (let ((hierarchy-list (sheep-hierarchy-list sheep)))
;;     (loop for sheep-obj in hierarchy-list
;;        do (multiple-value-bind (value has-p)
;; 	      (gethash property-name (sheep-property-value-table sheep-obj))
;; 	    (declare (ignore value))
;; 	    (when has-p
;; 	      (return-from %property-value-owner sheep-obj))) 
;;        finally (error 'unbound-property
;; 		      :format-control "Property ~A is unbound for sheep ~S"
;; 		      :format-args (list property-name sheep)))))

;; (defun memoize-property-access (sheep property &optional owner)
;;   (let ((actual-owner (or owner
;; 			  (when (has-direct-property-p sheep property)
;; 			    sheep)
;; 			  (%property-value-owner sheep property))))
;;     (setf (gethash property (sheep-property-owners sheep))
;; 	  actual-owner)))

;; (defun memoization-update-descendants (sheep property &optional owner)
;;   (let ((actual-owner (or owner
;; 			  (when (has-direct-property-p sheep property)
;; 			    sheep)
;; 			  (%property-value-owner sheep property))))
;;     (loop for child-pointer in (sheep-direct-children sheep)
;;        do (progn
;; 	    (memoize-specific-property-access (weak-pointer-value child-pointer)
;; 					      property actual-owner)
;; 	    (memoization-update-descendants (weak-pointer-value child-pointer)
;; 					    property actual-owner)))))


