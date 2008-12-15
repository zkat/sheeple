;; Copyright 2008 Kat Marchan

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
;; Main file for Sheeple
;;
;; TODO:
;; - Talents. Look at CLOS generic function implementation.
;; - Figure out a way to reimplement the stuff that was using the children slot
;; - Keep cleaning and testing until it's stable
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :sheeple)

;;;
;;; The Standard Sheep Definition
;;;
(defclass standard-sheep-class ()
  ((identifier
    :initarg :id
    :initform (gensym)
    :accessor sheep-id)
   (parents
    :initarg :parents
    :initform nil
    :accessor sheep-direct-parents)
   (properties
    :initarg :properties
    :initform (make-hash-table :test #'eq)
    :accessor sheep-direct-properties)
   (talents
    :initform nil
    :accessor sheep-direct-talents)))

(defmethod print-object ((sheep standard-sheep-class) stream)
  (print-unreadable-object (sheep stream :identity t)
    (format stream "Standard-Sheep ID: ~a" (sheep-id sheep))))

;;;
;;; Cloning
;;;

;; Example clone call (when it gets written)
;;
;; (clone (obj1 obj2)
;;   ((face "value")
;;    (gut "value")))

(let ((dolly (make-instance 'standard-sheep-class :id 'dolly)))

  ;; TODO: Make sure this is right.
  (defmacro clone (sheeple properties)
    "Clones a set of sheeple and returns a new sheep with them as its parents."
    `(let ((sheep
	    (set-up-inheritance
	     (make-instance 'standard-sheep-class)
	     (list ,sheeple)))
	   (props ',(canonicalize-properties properties)))
       (loop for (name . value) in props
	  do (setf (get-property sheep name) value))
       sheep))
  
  (defun fetch-dolly ()
    "Returns the standard sheep."
    dolly)
  
  (defun set-up-inheritance (new-sheep sheeple)
    "If SHEEPLE is non-nil, adds them in order to "
    (let ((obj new-sheep))
      (if sheeple
	  (loop for sheep in (nreverse sheeple)
	     do (add-parent sheep obj))
	  (add-parent dolly obj))
      obj))
  )

(defun canonicalize-properties (properties)
  (mapcar #'canonicalize-property properties))

(defun canonicalize-property (property)
  (if (symbolp (car property))
      (cons (car property) (cadr property))
      (error "Improper property!")))

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
	 (pushnew new-parent (sheep-direct-parents child))
	 child)))

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
	   do (unless (has-direct-slot-p child property-name)
		(set-slot-value child property-name value)))))
  child)

;;; unusable for now (need references to children)

;; (defun push-down-properties (sheep)
;;   "Pushes sheep's slot-values down to its children, unless the children have 
;; overridden the values."
;;   (with-accessors ((properties sheep-direct-properties)
;; 		   (children proto-children))
;;       sheep
;;     (loop for property-name being the hash-keys of properties
;;        using (hash-value value)
;;        do (loop for child in children
;; 	       do (unless (has-direct-slot-p child property-name)
;; 		    (set-slot-value child property-name value))))
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
;; 		   (children proto-children))
;;       sheep
;;     (loop for parent in parents
;; 	 do (setf (proto-children parent)
;; 		  (loop for par-child in (proto-children parent)
;; 		     if (eql par-child sheep)
;; 		     append children
;; 		     else collect par-child)))))

;; (defun give-children-new-parents (sheep)
;;   "Replaces the reference to SHEEP in its children's parents property with the sheep's parents."
;;   (with-accessors ((parents sheep-direct-parents)
;; 		   (children proto-children))
;;       sheep
;;     (loop for child in children
;;        do (setf (sheep-direct-parents child)
;; 		(loop for child-par in (sheep-direct-parents child)
;; 		   if (eql child-par sheep)
;; 		   append parents
;; 		   else collect child-par)))))

;;;
;;; Property Access
;;;

(defgeneric remove-property (sheep property-name)
  (:documentation "Removes a property from a particular sheep."))
(defmethod remove-property ((sheep standard-sheep-class) property-name)
  "Simply removes the hash value from the sheep. Leaves parents intact."
  (remhash property-name (sheep-direct-properties sheep)))

(defgeneric (setf get-property) (new-value sheep property-name)
  (:documentation "Sets a SLOT-VALUE with PROPERTY-NAME in SHEEP's properties."))
(defmethod (setf get-property) (new-value (sheep standard-sheep-class) property-name)
  "Default behavior is to only set it on a specific sheep. This will override its parents'
property values for that same property name, and become the new value for its children."
  (setf (gethash property-name (sheep-direct-properties sheep))
	new-value))

(defgeneric has-direct-property-p (sheep property-name)
  (:documentation "Returns NIL if PROPERTY-NAME is not set in this particular sheep."))
(defmethod has-direct-property-p ((sheep standard-sheep-class) property-name)
  "Simply catches the second value from gethash, which tells us if the hash exists or not.
This returns T if the value is set to NIL for that property-name."
  (multiple-value-bind (value has-p) (gethash property-name (sheep-direct-properties sheep))
    value
    has-p))

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
     finally (error "No such slot")))

(defgeneric who-sets (sheep property-name)
  (:documentation "Returns the sheep defining the value of SHEEP's property-name."))
(defmethod who-sets ((sheep standard-sheep-class) property-name)
  (loop for sheep in (compute-sheep-hierarchy-list sheep)
       if (has-direct-slot-p sheep property-name)
       return sheep))

(defgeneric available-properties (sheep)
  (:documentation "Returns a list of property-names available to SHEEP."))
(defmethod available-properties ((sheep standard-sheep-class))
  (let ((obj-keys (loop for keys being the hash-keys of (sheep-direct-properties sheep)
		     collect keys)))
    (remove-duplicates
     (flatten
      (append obj-keys (mapcar #'available-properties (sheep-direct-parents sheep)))))))


;;; Need access to children to use this.
;;;
;; (defun push-down-properties (sheep)
;;   "Pushes sheep's slot-values down to its children, unless the children have 
;; overridden the values."
;;   (with-accessors ((properties sheep-direct-properties)
;; 		   (children proto-children))
;;       sheep
;;     (loop for property-name being the hash-keys of properties
;;        using (hash-value value)
;;        do (loop for child in children
;; 	       do (unless (has-direct-slot-p child property-name)
;; 		    (set-slot-value child property-name value))))
;;     sheep))

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

(defun compute-sheep-hierarchy-list (sheep)
  (let ((sheeple-to-order (collect-parents sheep)))
    (topological-sort sheeple-to-order
		      (remove-duplicates
		       (mapappend #'local-precedence-ordering
				  sheeple-to-order))
		      #'std-tie-breaker-rule)))

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

