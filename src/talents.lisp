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

;; talents.lisp
;;
;; Implementation of Sheeple's generic functions (talents)
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :sheeple)

;;;
;;; Talent and participation base classes
;;;

(defclass standard-generic-ability ()
  ((name
    :initarg :name
    :accessor standard-generic-ability-name)
   (lambda-list
    :initarg :lambda-list
    :accessor standard-generic-ability-lambda-list))
  (:metaclass sb-mop:funcallable-standard-class))

(defclass standard-talent ()
  ((name
    :initarg :name
    :accessor standard-talent-name)
   (lambda-list
    :initarg :lambda-list
    :accessor standard-talent-lambda-list)
   (specializers
    :initarg :specializers
    :accessor standard-talent-specializers)
   (body
    :initarg :body
    :accessor standard-talent-body)
   (function
    :initarg :function
    :accessor standard-talent-function))
  (:metaclass sb-mop:funcallable-standard-class))

(defclass standard-talent-participation ()
  ((name
    :initarg :name
    :accessor name)
   (role
    :initarg :role
    :accessor role)
   (talent-pointer
    :initarg :talent-pointer
    :accessor talent-pointer)
   documentation))


;;;
;;; Talent definition
;;;

(defmacro deftalent (name lambda-list &body body)
  `(create-talent
    )
  )

(defun create-talent (&key name lambda-list specializers body)
  ;; What does this have to do?:
  ;; - Check if a function is bound to NAME
  ;; -- If the function bound is not a talent, signal error
  ;; -- Otherwise generate a new talent object
  ;; -- when the function is bound to talent, redefine the talent**** (this involves some messy stuff)
  ;; -- create participations for all the relevant specializers
  ;; Finally, return the talent object.
  (if (and (fboundp name)
	   (not (ability-p name)))
      (error "Trying to override a function that isn't a talent.")
      (let ((talent (make-instance 'standard-talent
				   :name name
				   :lambda-list lambda-list
				   :specializers specializers
				   :body body
				   :function body)))
	(when (ability-p name)
	  (remove-talents-with-name-and-specializers name specializers))
	(add-talent-to-sheeple name talent specializers)
	talent)))

(defun ability-p (name)
  (eql (class-of (fdefinition name))
       (find-class 'standard-generic-ability)))

(defun remove-talents-with-name-and-specializers (name specializers)
  ;; Keep a watchful eye on this. It only *seems* to work.
  (mapc (lambda (sheep) 
	    (mapc (lambda (participation) 
		    (when (and (eql name (name participation))
			       (equal specializers
				      (standard-talent-specializers
				       (talent-pointer participation))))
		      (setf (sheep-direct-participations sheep)
			    (remove participation (sheep-direct-participations sheep)))))
		  (sheep-direct-participations sheep)))
	specializers))

(defun add-talent-to-sheeple (name talent sheeple)
  (loop 
     for sheep in sheeple
     for i upto (1- (length sheeple))
     do (push (make-instance 'standard-talent-participation
			     :name name
			     :role i
			     :talent-pointer talent) 
	      (sheep-direct-participations sheep))))

;;;
;;; Talent dispatch
;;;

;; dispatch(selector, args, n)
;;  for each index below n
;;    position := 0
;;    push args[index] on ordering stack
;;    while ordering stack is not empty
;;      arg := pop ordering stack
;;      for each talent-property on arg with selector and index
;;        rank[talent-property's talent][index] := position
;;        if rank[talent-property's talent] is fully specified
;;          if no most specific talent
;;             or rank[talent-property's talent] < rank[most specific talent]
;;            most specific talent := talent-property's method
;;      for each ancestor on arg's hierarchy-list
;;        push ancestor on ordering stack
;;      position := position + 1
;;  return most specific talent-property
;; FUCK YOU SLATE

(defun find-most-specific-talent (selector &rest args)
  "Returns the most specific talent using SELECTOR and ARGS."
  ;; This shit is bugged to all hell and it's a huge, disgusting algorithm. Fix that shit.
  (let ((n (length args))
	(most-specific-talent nil)
	(ordering-stack nil)
	(discovered-talents nil))
    (loop 
       for index upto (1- n)
       for position upto (1- n)
       do (let ((position 0))
	    (push (elt args index) ordering-stack)
	    (loop 
	       while ordering-stack
	       do (let ((arg (pop ordering-stack)))
		    (loop
		       for participation in (sheep-direct-participations arg)
		       when (and (eql selector (name participation))
				 (eql index (role participation)))
		       do (pushnew (talent-pointer participation) 
				   discovered-talents)
		       do (setf (elt (standard-talent-rank (talent-pointer participation)) index)
				position)
		       if (or (fully-specified-p (standard-talent-rank (talent-pointer participation)))
			      (< (calculate-rank (standard-talent-rank (talent-pointer participation)))
				 (calculate-rank (standard-talent-rank most-specific-talent))))
		       do (setf most-specific-talent (talent-pointer participation)))
		    (add-ancestors-to-ordering-stack arg ordering-stack)))))
    (mapcar #'reset-talent-rank discovered-talents)
    most-specific-talent))

(defun add-ancestors-to-ordering-stack (arg ordering-stack)
  (loop 
     for ancestor in (remove arg (compute-sheep-hierarchy-list arg))
     do (push ancestor ordering-stack)))

(defun fully-specified-p (rank)
  (loop for item across rank
     do (when (eql item nil)
	  (return-from fully-specified-p nil)))
  t)

(defun calculate-rank (rank)
  (let ((total 0))
    (loop for item across rank
       do (when (numberp item)
	    (incf total item)))))

(defun reset-talent-rank (talent)
  (loop for item across (standard-talent-rank talent)
     do (setf item nil)))
