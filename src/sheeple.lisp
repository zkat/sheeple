;; Copyright 2008, 2009 Josh Marchan

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
;; * Add an option that prevents an object from being edited?
;; * Add a property metaobject for more tweaked property options?
;; * Write unit tests for everything before doing anything else here
;; * Keep cleaning and testing until it's stable
;; * Documentation!!
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :sheeple)

;;;
;;; The Standard Sheep Definition
;;;
(defvar *max-sheep-id* 0)

(defclass standard-sheep-class ()
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
   (properties
    :initarg :properties
    :initform (make-hash-table :test #'eql)
    :accessor sheep-direct-properties)
   (roles
    :initform nil
    :accessor sheep-direct-roles)))

(defgeneric sheep-p (sheep?))
(defmethod sheep-p ((sheep standard-sheep-class))
  (declare (ignore sheep))
  t)
(defmethod sheep-p (sheep?)
  (declare (ignore sheep?))
  nil)

(defmethod print-object ((sheep standard-sheep-class) stream)
  (print-unreadable-object (sheep stream :identity t)
    (format stream "Standard Sheep SID: ~a~@[ AKA: ~a~]" (sid sheep) (sheep-nickname sheep))))
;;;
;;; Sheep creation
;;;

(defparameter =dolly= (make-instance 'standard-sheep-class :nickname "=dolly=")
  "=dolly= is the parent object for all Sheeple. Everything and anything in Sheeple has
=dolly= as its parent object. Even fleeced-wolves.")

(defun create-sheep (sheeple properties &optional options)
  "Creates a new sheep with SHEEPLE as its parents, and PROPERTIES as its properties"
  (let ((sheep (make-instance 'standard-sheep-class))
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

(define-condition mitosis-error (error) ())

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
	(writers (getf property-list :writers)))
    (when (keywordp name)
      (error 'probably-meant-to-be-option))
    (setf (get-property sheep name) value)
    (add-readers-to-sheep readers name sheep)
    (add-writers-to-sheep writers name sheep)))

(define-condition probably-meant-to-be-option (error) ())

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
		      (copy-available-values sheep)))
      (:copy-direct-values (when (eql value t)
			     (copy-direct-parent-values sheep)))
      (:nickname (setf (sheep-nickname sheep) value))
      (:mitosis (warn "Mitosis successful. It probably broke everything... continue with care."))
      (otherwise (error 'invalid-option-error 
			:format-control "No such option for CLONE: ~s" 
			:format-args (list option))))))

(define-condition invalid-option-error (error) ())

(defun copy-available-values (sheep)
  (let ((all-property-names (available-properties sheep)))
    (loop for pname in all-property-names
       do (let ((value (get-property sheep pname)))
	    (setf (get-property sheep pname) value)))))

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
(defmethod add-parent ((new-parent standard-sheep-class) (child standard-sheep-class) &key)
  "Adds NEW-PARENT as a parent of CHILD. Checks to make sure NEW-PARENT and CHILD are not the same,
and that they arej both of the same class."
  (cond ((eql new-parent child)
	 (error "Can't inherit from self."))
	((not (eql (class-of new-parent)
		   (class-of child)))
	 (error "Wrong metaclass for parent"))
	(t
	 (handler-case
	     (progn
	       (pushnew new-parent (sheep-direct-parents child))
	       (compute-sheep-hierarchy-list child)
	       child)
	   (sheep-hierarchy-error ()
	     (progn
	       (setf (sheep-direct-parents child) (delete new-parent (sheep-direct-parents child)))
	       (error 'sheep-hierarchy-error 
		      "Adding new-parent would result in a
                             circular sheeple hierarchy list")))))))

(defgeneric remove-parent (parent child &key))
(defmethod remove-parent ((parent standard-sheep-class) (child standard-sheep-class) &key (keep-properties nil))
  "Deletes PARENT from CHILD's parent list."
  (setf (sheep-direct-parents child)
	(delete parent (sheep-direct-parents child)))
  (when keep-properties
    (with-accessors ((parent-properties sheep-direct-properties))
	parent
      (loop for property-name being the hash-keys of parent-properties
	 using (hash-value value)
	 do (unless (has-direct-property-p child property-name)
	      (setf (get-property child property-name) value)))))
  child)

;;; Inheritance-related predicates
(defun direct-parent-p (maybe-parent child)
  (when (member maybe-parent (sheep-direct-parents child))
    t))

(defun ancestor-p (maybe-ancestor descendant)
  (when (member maybe-ancestor (collect-parents descendant))
    t))

(defun direct-child-p (maybe-child parent)
  (direct-parent-p parent maybe-child))

(defun descendant-p (maybe-descendant ancestor)
  (ancestor-p ancestor maybe-descendant))


;;;
;;; Property Access
;;;
(define-condition unbound-property (error)
  ())

(defgeneric get-property (sheep property-name)
  (:documentation "Gets the property value under PROPERTY-NAME for an sheep, if-exists."))
(defmethod get-property ((sheep standard-sheep-class) property-name)
  "Default behavior is differential inheritance: It will look for that property-name up the entire 
sheep hierarchy."
  (get-property-with-hierarchy-list (compute-sheep-hierarchy-list sheep) property-name))

(defun get-property-with-hierarchy-list (list property-name)
  "Finds a property value under PROPERTY-NAME using a hierarchy list."
  (loop for sheep in list
     do (multiple-value-bind (value has-p) 
	    (gethash property-name (sheep-direct-properties sheep))
	  (when has-p
	    (return-from get-property-with-hierarchy-list value)))
     finally (error 'unbound-property)))

(defgeneric (setf get-property) (new-value sheep property-name)
  (:documentation "Sets a SLOT-VALUE with PROPERTY-NAME in SHEEP's properties."))
(defmethod (setf get-property) (new-value (sheep standard-sheep-class) property-name)
  "Default behavior is to only set it on a specific sheep. This will override its parents'
property values for that same property name, and become the new value for its children."
  (setf (gethash property-name (sheep-direct-properties sheep))
	new-value))

(defgeneric remove-property (sheep property-name)
  (:documentation "Removes a property from a particular sheep."))
(defmethod remove-property ((sheep standard-sheep-class) property-name)
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
(defmethod has-direct-property-p ((sheep standard-sheep-class) property-name)
  "Simply catches the second value from gethash, which tells us if the hash exists or not.
This returns T if the value is set to NIL for that property-name."
  (multiple-value-bind (value has-p) (gethash property-name (sheep-direct-properties sheep))
    value
    has-p))

(defgeneric who-sets (sheep property-name)
  (:documentation "Returns the sheep defining the value of SHEEP's property-name."))
(defmethod who-sets ((sheep standard-sheep-class) property-name)
  (loop for sheep in (compute-sheep-hierarchy-list sheep)
     if (has-direct-property-p sheep property-name)
     return sheep))

(defgeneric available-properties (sheep)
  (:documentation "Returns a list of property-names available to SHEEP."))
(defmethod available-properties ((sheep standard-sheep-class))
  (let ((obj-keys (loop for keys being the hash-keys of (sheep-direct-properties sheep)
		     collect keys)))
    (remove-duplicates
     (flatten
      (append obj-keys (mapcar #'available-properties (sheep-direct-parents sheep)))))))


;;;
;;; Hierarchy Resolution
;;; - mostly blatantly stolen from Closette.
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

(define-condition sheep-hierarchy-error (error)
  ((text :initarg :text :reader text))
  (:documentation "Signaled whenever there is a problem computing the hierarchy list."))

(defun compute-sheep-hierarchy-list (sheep)
  (handler-case 
      (let ((sheeple-to-order (collect-parents sheep)))
  	(topological-sort sheeple-to-order
  			  (remove-duplicates
  			   (mapappend #'local-precedence-ordering
  				      sheeple-to-order))
  			  #'std-tie-breaker-rule))
    (simple-error ()
      (error 'sheep-hierarchy-error :text "Unable to compute sheep hierarchy list."))))

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


;; For later -- these need a direct-children slot
;;
;; (defun push-down-properties (sheep)
;; "Pushes sheep's slot-values down to its children, unless the children have
;; overridden the values."
;; (with-accessors ((properties sheep-direct-properties)
;;      (children proto-children))
;; sheep
;; (loop for slot-name being the hash-keys of properties
;; using (hash-value value)
;; do (loop for child in children
;;    do (unless (has-direct-slot-p child slot-name)
;;      (set-slot-value child slot-name value))))
;; sheep))

;; (defun sacrifice-sheep (sheep &key (save-properties nil))
;; "Deletes SHEEP from the hierarchy, preserving the hierarchy by expanding references to
;; SHEEP into all its parents. Optionally, pushes its direct properties into its children."
;; (give-children-new-parents sheep)
;; (when save-properties
;; (push-down-properties sheep)))

;; (defun give-parents-new-children (sheep)
;; "Replaces the reference to SHEEP in its parents' children property with the sheep's children."
;; (with-accessors ((parents sheep-direct-parents)
;;      (children proto-children))
;; sheep
;; (loop for parent in parents
;;    do (setf (proto-children parent)
;;      (loop for par-child in (proto-children parent)
;;      if (eql par-child sheep)
;;      append children
;;      else collect par-child)))))

;; (defun give-children-new-parents (sheep)
;; "Replaces the reference to SHEEP in its children's parents property with the sheep's parents."
;; (with-accessors ((parents sheep-direct-parents)
;;      (children proto-children))
;; sheep
;; (loop for child in children
;; do (setf (sheep-direct-parents child)
;;     (loop for child-par in (sheep-direct-parents child)
;;      if (eql child-par sheep)
;;      append parents
;;      else collect child-par)))))
;;
;; (defun push-down-properties (sheep)
;; "Pushes sheep's slot-values down to its children, unless the children have
;; overridden the values."
;; (with-accessors ((properties sheep-direct-properties)
;;      (children proto-children))
;; sheep
;; (loop for slot-name being the hash-keys of properties
;; using (hash-value value)
;; do (loop for child in children
;;    do (unless (has-direct-slot-p child slot-name)
;;      (set-slot-value child slot-name value))))
;; sheep))