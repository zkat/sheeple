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
;; - Figure out a way to reimplement the stuff that was using the children slot
;; - Write unit tests for everything before doing anything else here
;; - Keep cleaning and testing until it's stable
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

(defmethod print-object ((sheep standard-sheep-class) stream)
  (print-unreadable-object (sheep stream :identity t)
    (format stream "Standard-Sheep SID: ~a" (sid sheep))))

;;;
;;; Cloning
;;;

;; Example: (clone (sheep1 sheep2 sheep3) ((property1 value1) (property2 value2)))
(defmacro clone (sheeple properties &optional options)
  "Standard sheep-generation macro"
  `(create-sheep
    :sheeple ,(canonicalize-sheeple sheeple)
    :properties ,(canonicalize-properties properties)))

(defparameter =dolly= (make-instance 'standard-sheep-class))

(defun create-sheep (&key sheeple properties)
  "Creates a new sheep with SHEEPLE as its parents, and PROPERTIES as its properties"
  (let ((sheep
	 (set-up-inheritance
	  (make-instance 'standard-sheep-class)
	  sheeple)))
    (loop for (name . value) in properties
       do (setf (get-property sheep name) value))
    sheep))
  
(defun set-up-inheritance (new-sheep sheeple)
  "If SHEEPLE is non-nil, adds them in order to "
  (let ((obj new-sheep))
    (if sheeple
	(loop for sheep in (nreverse sheeple)
	   do (add-parent sheep obj))
	(add-parent =dolly= obj))
    obj))

(defun sheep-p (maybe-sheep)
  (when (eql (class-of maybe-sheep)
	     (find-class 'standard-sheep-class))
    t))

(defun canonicalize-sheeple (sheeple)
  `(list ,@(mapcar #'canonicalize-sheep sheeple)))

(defun canonicalize-sheep (sheep)
  `(confirm-sheep ,sheep))

(defun confirm-sheep (sheep)
  (when (eql (class-of sheep)
	     (find-class 'standard-sheep-class))
    sheep))

(defun canonicalize-properties (properties)
  `(list ,@(mapcar #'canonicalize-property properties)))

(defun canonicalize-property (property)
  (if (symbolp (car property))
      `(cons ',(car property) ,(cadr property))
      (error "Improper property: property name must be a symbol.")))

;; (defun canonicalize-property (property)
;;   (if (symbolp property)
;;       `(list :name ',property)
;;       (let ((name (car property))
;; 	    (value (cadr property))
;;             (readers nil)
;;             (writers nil)
;;             (other-options nil))
;;         (do ((olist (cddr property) (cddr olist)))
;;             ((null olist))
;;           (case (car olist)
;;             (:reader 
;;              (pushend (cadr olist) readers))
;;             (:writer 
;;              (pushend (cadr olist) writers))
;;             (:accessor
;;              (pushend (cadr olist) readers)
;;              (pushend `(setf ,(cadr olist)) writers))
;;             (otherwise 
;;              (pushend `',(car olist) other-options)
;;              (pushend `',(cadr olist) other-options))))
;;         `(list
;;            :name ',name
;; 	   :value ,value
;;            ,@(when readers `(:readers ',readers))
;;            ,@(when writers `(:writers ',writers))))))

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

