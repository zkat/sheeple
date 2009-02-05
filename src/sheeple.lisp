;; Copyright 2008, 2009 Kat Marchan

;; Permission is hereby granted, free of charge, to any person
;; obtaining a copy of this software and associated documentation
;; files (the "Software"), to deal in the Software without
;; restriction, including without limitation the rights to use,
;; copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the
;; Software is furnished to do so, subject to the following
;; conditions:

;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
;; OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
;; HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
;; WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
;; FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
;; OTHER DEALINGS IN THE SOFTWARE.

;; sheeple.lisp
;;
;; Sheep class and inheritance/property-related code, as well as sheep cloning.
;;
;; TODO:
;; * Add an option that prevents an entire object from being edited?
;; * Add property option that works like :initform ?
;; * Add clone option to copy individual properties?
;; * Clone option to auto-generate all accessors (with optional appending a-la defstruct?)
;; * Documentation!!
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :sheeple)
;(declaim (optimize (speed 3) (safety 1)))
;;;
;;; The Standard Sheep Definition
;;;
(defvar *max-sheep-id* 0)

(defclass standard-sheep ()
  ((sid
    :initform (incf *max-sheep-id*)
    :reader sid)
   (nickname
    :initform nil
    :initarg :nickname
    :accessor sheep-nickname)
   (parents
    :initarg :parents
    :initform nil
    :accessor sheep-direct-parents)
   (children
    :initform nil
    :accessor sheep-direct-children)
   (properties
    :initarg :properties
    :initform (make-hash-table :test #'equal)
    :accessor sheep-direct-properties)
   (property-owners
    :initarg :property-owner
    :initform (make-weak-hash-table :weakness :value :test #'equal)
    :accessor sheep-property-owners)
   (roles
    :initform nil
    :accessor sheep-direct-roles)
   (hierarchy-list
    :initform nil
    :accessor sheep-hierarchy-list)))

;; (defmethod sheep-property-owners (sheep)
;;   (when (weak-pointer-p sheep)
;;     (sheep-property-owners (weak-pointer-value sheep))))

(defclass standard-sheep-property ()
  ((name
    :initarg :name
    :accessor %name)
   (value
    :initarg :value
    :accessor %value)
   (locked-p
    :initarg :locked-p
    :initform nil
    :accessor %locked-p)))

(defgeneric sheep-p (sheep?))
(defmethod sheep-p ((sheep standard-sheep))
  (declare (ignore sheep))
  t)
(defmethod sheep-p (sheep?)
  (declare (ignore sheep?))
  nil)

(defmethod print-object ((sheep standard-sheep) stream)
  (print-unreadable-object (sheep stream :identity t)
    (format stream "Standard Sheep SID: ~a~@[ AKA: ~a~]" (sid sheep) (sheep-nickname sheep))))

;;;
;;; Sheep creation
;;;

(defvar =dolly= (make-instance 'standard-sheep :nickname "=dolly=")
  "=dolly= is the parent object for all Sheeple. Everything and anything in Sheeple has
=dolly= as its parent object. Even fleeced-wolves.")

(defun create-sheep (sheeple properties &optional options)
  "Creates a new sheep with SHEEPLE as its parents, and PROPERTIES as its properties"
  (let ((sheep (make-instance 'standard-sheep)) ;;this should determine SHEEPLE's classes, and clone that.
	(mitosis? (getf (find-if (lambda (option) (equal (car option) :mitosis)) options)
			:mitosis)))
    (cond ((and mitosis?
		(< 1 (length sheeple)))
	   (error 'mitosis-error))  ; I think I can make use of this, actually
	  (mitosis?
	   (setf sheep (mitosis sheeple sheep)))
	  (t
	   (setf sheep (set-up-inheritance sheep sheeple))))
    (set-up-properties properties sheep)
    (set-up-other-options options sheep)
    (memoize-sheep-hierarchy-list sheep)
    (memoize-property-access sheep)
    sheep))

(defun set-up-inheritance (new-sheep sheeple)
  "If SHEEPLE is non-nil, adds them in order to "
  (let ((obj new-sheep))
    (if sheeple
	(loop for sheep in (nreverse sheeple)
	   do (add-parent sheep obj))
	(add-parent =dolly= obj))
    obj))

(defun mitosis (sheeple sheep)
  ;; THIS IS GOING TO BE THE END OF ME, I SWEAR.
  (let* ((model (car sheeple))
	 (parents (sheep-direct-parents model))
	 (properties (sheep-direct-properties model))
	 (roles (sheep-direct-roles model)))
    (setf (sheep-direct-parents sheep)
	  parents)
    (setf (sheep-direct-properties sheep)
	  properties)
    (setf (sheep-direct-roles sheep)
	  roles)
    sheep))

(define-condition mitosis-error (sheeple-error) ())

;;;
;;; Property and property-option setup
;;;

(defun set-up-properties (properties sheep)
  (loop for property-list in properties
     do (set-up-property property-list sheep)))

(defun set-up-property (property-list sheep)
  (let ((name (getf property-list :name))
	(value (getf property-list :value))
	(readers (getf property-list :readers))
	(writers (getf property-list :writers))
	(locked-p (getf property-list :lock)))
    (when (keywordp name)
      (error 'probably-meant-to-be-option))
    (setf (get-property sheep name) value)
    (when locked-p
      (lock-property sheep name))
    (add-readers-to-sheep readers name sheep)
    (add-writers-to-sheep writers name sheep)))

(define-condition probably-meant-to-be-option (sheeple-error) ())

;;;
;;; Clone options
;;;

(defun set-up-other-options (options sheep)
  (loop for option in options
     do (set-up-option option sheep)))

(defun set-up-option (option sheep)
  (let ((option (car option))
	(value (cadr option)))
    (case option
      (:copy-all-values (when (eql value t)
			  (copy-all-values sheep)))
      (:copy-direct-values (when (eql value t)
			     (copy-direct-parent-values sheep)))
      (:deep-copy (when (eql value t)
		    (copy-all-values sheep)))
      (:nickname (setf (sheep-nickname sheep) value))
      (:lock (when (eql value t)
	       (lock-sheep sheep)))
      (:mitosis (warn "Mitosis successful. It probably broke everything... continue with care."))
      (otherwise (error 'invalid-option-error)))))

(define-condition invalid-option-error (sheeple-error) ())

(defun copy-all-values (sheep)
  (let ((all-property-names (available-properties (if (weak-pointer-p sheep)
						      (weak-pointer-value sheep)
						      sheep))))
    (mapc (lambda (pname)
	    (setf (get-property sheep pname)
		  (get-property sheep pname)))
	  all-property-names)))

(defun copy-direct-parent-values (sheep)
  (mapc (lambda (parent)
	  (maphash 
	   (lambda (key value) 
	     (setf (get-property sheep key) value)) (sheep-direct-properties parent)))
	(sheep-direct-parents sheep)))

(defun add-readers-to-sheep (readers prop-name sheep)
  (loop for reader in readers
     do (ensure-buzzword :name reader)
     do (ensure-message :name reader
			:lambda-list '(sheep)
			:participants (list sheep)
			:body `(get-property sheep ',prop-name)
			:function (lambda (sheep) (get-property sheep prop-name)))))

(defun add-writers-to-sheep (writers prop-name sheep)
  (loop for writer in writers
     do (ensure-buzzword :name writer)
     do (ensure-message :name writer
			:lambda-list '(new-value sheep)
			:participants (list =dolly= sheep)
			:body `(setf (get-property sheep ',prop-name) new-value)
			:function (lambda (new-value sheep) (setf (get-property sheep prop-name)
								  new-value)))))

;;;
;;; Inheritance management
;;;

(defgeneric add-parent (new-parent child &key))
(defmethod add-parent ((new-parent standard-sheep) (child standard-sheep) &key)
  "Adds NEW-PARENT as a parent of CHILD. Checks to make sure NEW-PARENT and CHILD are not the same,
and that they arej both of the same class."
  (cond ((equal new-parent child)
	 (error "Can't inherit from self."))
	((not (equal (class-of new-parent)
		   (class-of child)))
	 (error "Wrong metaclass for parent"))
	(t
	 (handler-case
	     (progn
	       (pushnew new-parent (sheep-direct-parents child))
	       (let ((pointer (make-weak-pointer child)))
		 (pushnew pointer (sheep-direct-children new-parent))
		 (finalize child (lambda () (setf (sheep-direct-children new-parent)
						  (delete pointer 
							  (sheep-direct-children new-parent))))))
	       child)
	   (sheep-hierarchy-error ()
	     (progn
	       (setf (sheep-direct-parents child) (delete new-parent (sheep-direct-parents child)))
	       (error 'sheep-hierarchy-error)))))))

(defmethod add-parent :after ((new-parent standard-sheep) (child standard-sheep) &key)
  (declare (ignore new-parent))
  (memoize-sheep-hierarchy-list child)
  (memoize-property-access child)
  (loop for child-pointer in (sheep-direct-children child)
     do (memoize-property-access (weak-pointer-value child-pointer))))

(defgeneric remove-parent (parent child &key))
(defmethod remove-parent ((parent standard-sheep) (child standard-sheep) &key (keep-properties nil))
  "Deletes PARENT from CHILD's parent list."
  (setf (sheep-direct-parents child)
	(delete parent (sheep-direct-parents child)))
  (setf (sheep-direct-children parent)
	(delete child (sheep-direct-children parent) :key #'weak-pointer-value))
  (when keep-properties
    (with-accessors ((parent-properties sheep-direct-properties))
	parent
      (loop for property-name being the hash-keys of parent-properties
	 using (hash-value value)
	 do (unless (has-direct-property-p child property-name)
	      (setf (get-property child property-name) value)))))
  child)

(defmethod remove-parent :after ((parent standard-sheep) (child standard-sheep) &key)
  (memoize-sheep-hierarchy-list child)
  (memoize-property-access child)
  (loop for child-pointer in (sheep-direct-children child)
     do (memoize-property-access (weak-pointer-value child-pointer))))

;;; Inheritance-related predicates
(defun direct-parent-p (maybe-parent child)
  (when (member maybe-parent (sheep-direct-parents child))
    t))

(defun ancestor-p (maybe-ancestor descendant)
  (when (and (not (equal maybe-ancestor descendant))
	     (member maybe-ancestor (collect-parents descendant)))
    t))

(defun direct-child-p (maybe-child parent)
  (direct-parent-p parent maybe-child))

(defun descendant-p (maybe-descendant ancestor)
  (ancestor-p ancestor maybe-descendant))

;;;
;;; Properties
;;;

;;; locked properties
(define-condition unbound-property (sheeple-error)
  ())
(define-condition locked-property (sheeple-error)
  ())
(define-condition property-locking-error (sheeple-error)
  ())

(defgeneric property-locked-p (sheep prop-name)
  (:documentation "Is the property with PROP-NAME in SHEEP locked from editing?"))
(defmethod property-locked-p ((sheep standard-sheep) property-name)
  (%locked-p (%get-property-object sheep property-name)))

(defgeneric lock-property (sheep property-name)
  (:documentation "Locks a property on a particular object, preventing it from being edited."))
(defmethod lock-property ((sheep standard-sheep) property-name)
  (if (has-direct-property-p sheep property-name)
      (setf (%locked-p (%get-property-object sheep property-name)) t)
      (error 'property-locking-error)))

(defgeneric unlock-property (sheep property-name)
  (:documentation "Unlocks a property on a particular object, allowing its value to be changed."))
(defmethod unlock-property ((sheep standard-sheep) property-name)
  (if (has-direct-property-p sheep property-name)
      (setf (%locked-p (%get-property-object sheep property-name)) nil)
      (error 'property-locking-error)))

(defun toggle-lock (sheep property-name)
  (if (property-locked-p sheep property-name)
      (unlock-property sheep property-name)
      (lock-property sheep property-name)))

(defgeneric lock-sheep (sheep)
  (:documentation "Locks all of SHEEP's properties."))
(defmethod lock-sheep ((sheep standard-sheep))
  (maphash (lambda (key value)
	     (declare (ignore value))
	     (lock-property sheep key))
	   (sheep-direct-properties sheep)))

(defgeneric unlock-sheep (sheep)
  (:documentation "Sets all of SHEEP's properties to 'unlocked'"))
(defmethod unlock-sheep ((sheep standard-sheep))
  (maphash (lambda (key value)
	     (declare (ignore value))
	     (unlock-property sheep key))
	   (sheep-direct-properties sheep)))

;;; Getting/setting
(defgeneric get-property (sheep property-name)
  (:documentation "Gets the property value under PROPERTY-NAME for an sheep, if-exists."))
(defmethod get-property ((sheep standard-sheep) property-name)
  "Default behavior is differential inheritance: It will look for that property-name up the entire 
sheep hierarchy."
  (get-property-with-memoized-owner sheep property-name))

(defun %get-property-object (sheep prop-name)
  (gethash prop-name (sheep-direct-properties sheep)))

(defun (setf %get-property-object) (new-value sheep prop-name)
  (setf (gethash prop-name (sheep-direct-properties sheep)) new-value))

(defun get-property-with-memoized-owner (sheep property-name)
  (multiple-value-bind (prop-owner has-p)
      (gethash property-name (sheep-property-owners sheep))
    (if has-p
	(multiple-value-bind (prop-obj has-p)
	    (gethash property-name (sheep-direct-properties prop-owner))
	  (if has-p
	      (%value prop-obj)
	      (error 'unbound-property)))
	(error 'unbound-property))))

(defun get-property-with-hierarchy-list (list property-name)
  "Finds a property value under PROPERTY-NAME using a hierarchy list."
  (loop for sheep in list
     do (multiple-value-bind (prop-obj has-p) 
	    (%get-property-object sheep property-name)
	  (when has-p
	    (return-from get-property-with-hierarchy-list (%value prop-obj))))
     finally (error 'unbound-property)))

(defgeneric (setf get-property) (new-value sheep property-name)
  (:documentation "Sets a SLOT-VALUE with PROPERTY-NAME in SHEEP's properties."))
(defmethod (setf get-property) (new-value (sheep standard-sheep) property-name)
  "Default behavior is to only set it on a specific sheep. This will override its parents'
property values for that same property name, and become the new value for its children."
  (multiple-value-bind (prop-obj has-p)
      (%get-property-object sheep property-name)
    (if has-p
	(if (property-locked-p sheep property-name)
	    (error 'locked-property)
	    (setf (%value prop-obj)
		  new-value))
	(progn
	  (setf (gethash property-name (sheep-direct-properties sheep))
		(make-instance 'standard-sheep-property :name property-name :value new-value))
	  new-value))))

(defgeneric remove-property (sheep property-name)
  (:documentation "Removes a property from a particular sheep."))
(defmethod remove-property ((sheep standard-sheep) property-name)
  "Simply removes the hash value from the sheep. Leaves parents intact."
  (remhash property-name (sheep-direct-properties sheep)))

(defun has-property-p (sheep property-name)
  "Returns T if a property with PROPERTY-NAME is available to SHEEP."
  (handler-case
      (when (get-property sheep property-name)
	t)
    (unbound-property () nil)))

(defgeneric has-direct-property-p (sheep property-name)
  (:documentation "Returns NIL if PROPERTY-NAME is not set in this particular sheep."))
(defmethod has-direct-property-p ((sheep standard-sheep) property-name)
  "Simply catches the second value from gethash, which tells us if the hash exists or not.
This returns T if the value is set to NIL for that property-name."
  (multiple-value-bind (value has-p) (gethash property-name (sheep-direct-properties sheep))
    value
    has-p))

(defgeneric who-sets (sheep property-name)
  (:documentation "Returns the sheep defining the value of SHEEP's property-name."))
(defmethod who-sets ((sheep standard-sheep) property-name)
  (multiple-value-bind (owner has-p)
      (gethash property-name (sheep-property-owners sheep))
    (if has-p
	owner
	(error 'unbound-property))))

(defgeneric available-properties (sheep)
  (:documentation "Returns a list of property-names available to SHEEP."))
(defmethod available-properties ((sheep standard-sheep))
  (let ((obj-keys (loop for keys being the hash-keys of (sheep-direct-properties sheep)
		     collect keys)))
    (remove-duplicates
     (flatten
      (append obj-keys (mapcar #'available-properties (sheep-direct-parents sheep)))))))

;;; Memoization
(defun memoize-property-access (sheep)
  (loop for property in (available-properties sheep)
     do (let ((owner (%get-property-owner sheep property)))
	  (setf (gethash property (sheep-property-owners sheep))
		owner))))

(defun %get-property-owner (sheep property-name)
  (let ((hierarchy-list (sheep-hierarchy-list sheep)))
    (loop for sheep in hierarchy-list
       do (multiple-value-bind (prop-obj has-p)
	      (%get-property-object sheep property-name)
	    (declare (ignore prop-obj))
	    (when has-p
	      (return-from %get-property-owner sheep)))
       finally (error 'unbound-property))))

(defun memoize-sheep-hierarchy-list (sheep)
  (let ((list (compute-sheep-hierarchy-list sheep)))
    (setf (sheep-hierarchy-list sheep)
	  list)
    (mapc (lambda (descendant) 
	    (memoize-sheep-hierarchy-list (weak-pointer-value descendant)))
	  (sheep-direct-children sheep))))

(defmethod (setf get-property) :after (new-value (sheep standard-sheep) property-name)
  (declare (ignore new-value property-name))
  (memoize-property-access sheep)
  (loop for child-pointer in (sheep-direct-children sheep)
     do (memoize-property-access (weak-pointer-value child-pointer))))

(defmethod remove-property :after ((sheep standard-sheep) property-name)
  (remhash property-name (sheep-property-owners sheep))
  (loop for child-pointer in (sheep-direct-children sheep)
     do (remhash property-name (sheep-direct-properties (weak-pointer-value child-pointer)))))
;;;
;;; Hierarchy Resolution
;;; - blatantly stolen from Closette.
;;;
(defun collect-parents (sheep)
  (labels ((all-parents-loop (seen parents)
	     (let ((to-be-processed
		    (set-difference parents seen)))
	       (if (null to-be-processed)
		   parents
		   (let ((sheep-to-process
			  (car to-be-processed)))
		     (all-parents-loop
		      (cons sheep-to-process seen)
		      (union (sheep-direct-parents sheep-to-process)
			     parents)))))))
    (all-parents-loop () (list sheep))))

(defun compute-sheep-hierarchy-list (sheep)
  (handler-case 
      (let ((sheeple-to-order (collect-parents sheep)))
  	(topological-sort sheeple-to-order
			  (remove-duplicates
			   (mapappend #'local-precedence-ordering
				      sheeple-to-order))
			  #'std-tie-breaker-rule))
    (simple-error ()
      (error 'sheep-hierarchy-error))))



(define-condition sheep-hierarchy-error (sheeple-error) ()
  (:documentation "Signaled whenever there is a problem computing the hierarchy list."))

(defun local-precedence-ordering (sheep)
  (mapcar #'list
	  (cons sheep
		(butlast (sheep-direct-parents sheep)))
	  (sheep-direct-parents sheep)))

(defun std-tie-breaker-rule (minimal-elements cpl-so-far)
  (dolist (cpl-constituent (reverse cpl-so-far))
    (let* ((supers (sheep-direct-parents cpl-constituent))
           (common (intersection minimal-elements supers)))
      (when (not (null common))
        (return-from std-tie-breaker-rule (car common))))))

;;; Set up =dolly=
(memoize-sheep-hierarchy-list =dolly=)

;;;
;;; Unused at the moment
;;;
;; (defun push-down-properties (sheep)
;;   "Pushes sheep's slot-values down to its children, unless the children have
;; overridden the values."
;;   (with-accessors ((properties sheep-direct-properties)
;; 		   (children sheep-direct-children))
;;       sheep
;;     (loop for slot-name being the hash-keys of properties
;;        using (hash-value value)
;;        do (loop for child in children
;; 	     do (unless (has-direct-property-p child slot-name)
;; 		  (setf (get-property child slot-name) value))))
;;     sheep))

;; (defun sacrifice-sheep (sheep &key (save-properties nil))
;;   "Deletes SHEEP from the hierarchy, preserving the hierarchy by expanding references to
;; SHEEP into all its parents. Optionally, pushes its direct properties into its children."
;;   (give-children-new-parents sheep)
;;   (when save-properties
;;     (push-down-properties sheep)))

;; (defun give-parents-new-children (sheep)
;;   "Replaces the reference to SHEEP in its parents' children property with the sheep's children."
;;   (with-accessors ((parents sheep-direct-parents)
;; 		   (children sheep-direct-children))
;;       sheep
;;     (loop for parent in parents
;;        do (setf (sheep-direct-children parent)
;; 		(loop for par-child in (sheep-direct-children parent)
;; 		   if (equal par-child sheep)
;; 		   append children
;; 		   else collect par-child)))))

;; (defun give-children-new-parents (sheep)
;;   "Replaces the reference to SHEEP in its children's parents property with the sheep's parents."
;;   (with-accessors ((parents sheep-direct-parents)
;; 		   (children sheep-direct-children))
;;       sheep
;;     (loop for child in children
;;        do (setf (sheep-direct-parents child)
;; 		(loop for child-par in (sheep-direct-parents child)
;; 		   if (equal child-par sheep)
;; 		   append parents
;; 		   else collect child-par)))))

;;; performance test
;; (let* ((sheep1 (clone () ((var "value" :accessor var))))
;;        (sheep2 (clone (sheep1) ()))
;;        (sheep3 (clone (sheep2) ()))
;;        (sheep4 (clone (sheep3) ()))
;;        (sheep5 (clone (sheep4) ()))
;;        (sheep6 (clone (sheep5) ()))
;;        (sheep7 (clone (sheep6) ()))
;;        (sheep8 (clone (sheep7) ()))
;;        (sheep9 (clone (sheep8) ()))
;;        (sheep10 (clone (sheep9) ())))
;;   (time (dotimes (i 100000) (var sheep10))))