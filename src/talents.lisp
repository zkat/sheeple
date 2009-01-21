;; Copyright 2008 Josh Marchan

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

(defclass standard-talent ()
  ((name
    :initarg :name
    :accessor standard-talent-name)
   (lambda-list
    :initarg :lambda-list
    :accessor standard-talent-lambda-list)
   (body
    :initarg :body
    :accessor standard-talent-body)
   (function
    :initarg :function
    :accessor standard-talent-function)
   (rank
    :initarg :rank
    :initform (vector nil)
    :accessor standard-talent-rank))
  (:metaclass sb-mop:funcallable-standard-class))

(defclass standard-talent-property ()
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

(defun define-talent (&key name lambda-list specializers body)
  (let ((talent (make-instance 'standard-talent
			       :name name
			       :lambda-list lambda-list
			       :body body
			       :function body
			       :rank (make-array (length lambda-list) :initial-element nil))))
    (loop 
       for specializer in specializers
       for i upto (1- (length specializers))
       do (pushnew (make-instance 'standard-talent-property
				  :name name
				  :role i
				  :talent-pointer talent) 
		   (sheep-direct-participations specializer)))
    talent))

(defun find-most-specific-talent (selector &rest args)
  (let ((n (length args))
	(most-specific-talent nil)
	(ordering-stack nil))
    (loop 
       for index upto n
       for position upto n
       do (let ((position 0))
	    (push (elt args index) ordering-stack)
	    (loop while ordering-stack
	       do (let ((arg (pop ordering-stack)))
		    (loop for participation in (sheep-direct-participations arg)
		       when (and (eql selector (name participation))
				 (eql index (role participation)))
		       do (setf (elt (standard-talent-rank (talent-pointer participation)) index)
				position)
		       if (or (fully-specified-p (standard-talent-rank (talent-pointer participation)))
			      (< (calculate-rank (standard-talent-rank (talent-pointer participation)))
				 (calculate-rank (standard-talent-rank most-specific-talent))))
		       do (setf most-specific-talent (talent-pointer participation)))
		    (loop for ancestor in (compute-sheep-hierarchy-list arg)
		       do (push ancestor ordering-stack))))))
    most-specific-talent))

(defun fully-specified-p (rank)
  (loop for item across rank
     do (when (eql item nil)
	  (return-from fully-specified-p nil)))
  t)

(defun calculate-rank (rank)
  (reduce #'+ rank))

;dispatch(selector, args, n)
;  for each index below n
;    position := 0
;    push args[index] on ordering stack
;    while ordering stack is not empty
;      arg := pop ordering stack
;      for each talent-property on arg with selector and index
;        rank[talent-property's talent][index] := position
;        if rank[talent-property's talent] is fully specified
;          if no most specific talent
;             or rank[talent-property's talent] < rank[most specific talent]
;            most specific talent := talent-property's method
;      for each ancestor on arg's hierarchy-list
;        push ancestor on ordering stack
;      position := position + 1
;  return most specific talent-property
;FUCK YOU SLATE

(defun dispatch-talent (talent-name &rest args)
  ;; step 1 - find all talents with talent-name that the first arg 
  ;; has role position 1 in. (incl. parents), push them into a list.
  ;; 
  ;; step 2 -

  args
  )

(defmacro deftalent (name lambda-list &body body)
  `(create-talent )
  )


;;;
;;; Tests
;;;
