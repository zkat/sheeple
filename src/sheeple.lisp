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

(defpackage #:sheeple
  (:use :cl))

(in-package :sheeple)

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

;;;
;;; Cloning
;;;

;; Example clone call
;;
;; (clone (obj1 obj2)
;;   ((face "value")
;;    (gut "value")))

(let ((dolly (make-instance 'standard-sheep-class :id 'dolly)))

  (defun clone (&rest sheeple)
    "Clones a set of SHEEPLE and returns a new SHEEP with them as its parents."
    (set-up-inheritance
     (make-instance 'standard-sheep-class)
     sheeple))

;;;   WIP
;;;
;;;   (defun macro-clone (sheeple properties)
;;;     "Clones a set of sheeple and returns a new sheep with them as its parents."
;;;     `(let ((obj 
;;; 	    (set-up-inheritance
;;; 	     (make-instance 'standard-sheep-class)
;;; 	     ,sheeple))))
;;;     ,@())

  (defun inspect-dolly ()
    "Returns the standard sheep."
    dolly)
  
  (defun set-up-inheritance (new-sheep sheeple)
    "If SHEEPLE is non-nil, adds them in order to "
    (let ((obj new-sheep))
      (if sheeple
	  (loop for sheep in (nreverse sheeple)
	     do (inherit-from sheep obj))
	  (inherit-from dolly obj))
      obj))
  )

;;;
;;; Inheritance management
;;;

(defun inherit-from (child new-parent)
  "Adds NEW-PARENT as a parent of CHILD. Checks to make sure NEW-PARENT and CHILD are not the same,
and that they are both of the same class."
  (cond ((eql new-parent child)
	 (error "Can't inherit from self."))
	((not (eql (class-of new-parent)
		   (class-of child)))
	 (error "Wrong metaclass for parent"))
	(t
	 (pushnew new-parent (sheep-direct-parents child))
	 child)))

(defun remove-parent (parent child &key (keep-properties nil))
  "Deletes PARENT from CHILD's parent list."
  (setf (sheep-direct-parents child)
	(delete parent (sheep-direct-parents child)))
  (when keep-properties
    (with-accessors ((parent-properties sheep-direct-properties))
	parent
      (loop for slot-name being the hash-keys of parent-properties
	   using (hash-value value)
	   do (unless (has-direct-slot-p child slot-name)
		(set-slot-value child slot-name value)))))
  child)

;; (defun push-down-properties (sheep)
;;   "Pushes sheep's slot-values down to its children, unless the children have 
;; overridden the values."
;;   (with-accessors ((properties sheep-direct-properties)
;; 		   (children proto-children))
;;       sheep
;;     (loop for slot-name being the hash-keys of properties
;;        using (hash-value value)
;;        do (loop for child in children
;; 	       do (unless (has-direct-slot-p child slot-name)
;; 		    (set-slot-value child slot-name value))))
;;     sheep))


;;; unusable for now (need references to children)

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

(defmethod print-object ((sheep standard-sheep-class) stream)
  (print-unreadable-object (sheep stream :identity t)
    (format stream "std-sheep ID: ~d" (sheep-id sheep))))

;;;
;;; Property Access
;;;

(defgeneric remove-property (sheep slot-name)
  (:documentation "Removes a property from a particular sheep."))
(defmethod remove-property ((sheep standard-sheep-class) slot-name)
  "Simply removes the hash value from the sheep. Leaves parents intact."
  (remhash slot-name (sheep-direct-properties sheep)))

(defgeneric set-slot-value (sheep slot-name slot-value)
  (:documentation "Sets a SLOT-VALUE with SLOT-NAME in SHEEP's properties."))
(defmethod set-slot-value ((sheep standard-sheep-class) slot-name slot-value)
  "Default behavior is to only set it on a specific sheep. This will override its parents'
property values for that same property name, and become the new value for its children."
  (setf (gethash slot-name (sheep-direct-properties sheep))
	slot-value))

(defgeneric has-direct-slot-p (sheep slot-name)
  (:documentation "Returns NIL if SLOT-NAME is not set in this particular sheep."))
(defmethod has-direct-slot-p ((sheep standard-sheep-class) slot-name)
  "Simply catches the second value from gethash, which tells us if the hash exists or not.
This returns T if the value is set to NIL for that slot-name."
  (multiple-value-bind (value has-p) (gethash slot-name (sheep-direct-properties sheep))
    value
    has-p))

(defgeneric get-slot-value (sheep slot-name)
  (:documentation "Gets the property value under SLOT-NAME for an sheep, if-exists."))
(defmethod get-slot-value ((sheep standard-sheep-class) slot-name)
  "Default behavior is differential inheritance: It will look for that slot-name up the entire 
sheep hierarchy."
  (get-slot-value-with-hierarchy-list (compute-sheep-hierarchy-list sheep) slot-name))

(defun get-slot-value-with-hierarchy-list (list slot-name)
  "Finds a property value under SLOT-NAME using a hierarchy list."
  (loop for sheep in list
     do (multiple-value-bind (value has-p) 
	    (gethash slot-name (sheep-direct-properties sheep))
	  (when has-p
	    (return-from get-slot-value-with-hierarchy-list value)))
     finally (error "No such slot")))

(defgeneric who-sets (sheep slot-name)
  (:documentation "Returns the sheep defining the value of SHEEP's slot-name."))
(defmethod who-sets ((sheep standard-sheep-class) slot-name)
  (loop for sheep in (compute-sheep-hierarchy-list sheep)
       if (has-direct-slot-p sheep slot-name)
       return sheep))

(defgeneric available-properties (sheep)
  (:documentation "Returns a list of slot-names available to SHEEP."))
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
;;     (loop for slot-name being the hash-keys of properties
;;        using (hash-value value)
;;        do (loop for child in children
;; 	       do (unless (has-direct-slot-p child slot-name)
;; 		    (set-slot-value child slot-name value))))
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

;;;
;;; Utils
;;;

(defun pushend (obj list)
  (setf list (nconc list (cons obj nil))))

(defun mapappend (fun &rest args)
  (if (some #'null args)
      ()
      (append (apply fun (mapcar #'car args))
              (apply #'mapappend fun (mapcar #'cdr args)))))

(defun flatten (x)
  "Flattens a list."
  (labels ((rec (x acc)
	     (cond ((null x) acc)
		   ((atom x) (cons x acc))
		   (t (rec (car x) (rec (cdr x) acc))))))
    (rec x nil)))

(defun topological-sort (elements constraints tie-breaker)
  (let ((remaining-constraints constraints)
        (remaining-elements elements)
        (result ())) 
    (loop
     (let ((minimal-elements 
            (remove-if
             #'(lambda (sheep)
                 (member sheep  remaining-constraints
                         :key #'cadr))
             remaining-elements)))
       (when (null minimal-elements)
             (if (null remaining-elements)
                 (return-from topological-sort result)
		 (error "Inconsistent precedence graph.")
		 ))
       (let ((choice (if (null (cdr minimal-elements))
                         (car minimal-elements)
                       (funcall tie-breaker
                                minimal-elements
                                result))))
         (setf result (append result (list choice)))
         (setf remaining-elements
               (remove choice remaining-elements))
         (setf remaining-constraints
               (remove choice
                       remaining-constraints
                       :test #'member)))))))
