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
    :initform nil
    :accessor rank))
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

(defun applicable-talent-list)
(defun generate-applicable-talent-list (selector &rest args)
  (let ((n (length args))
	(most-specific-method nil)
	(ordering-stack nil))
    (loop 
       for index upto n
       fof position upto n
       do (let ((position 0))
	    (push (elt args index) ordering-stack)
	    (loop while ordering-stack
	       do (let ((arg (pop ordering-stack)))
		    (loop for role in (sheep-direct-roles arg)
		       when (and (eql selector (name role))
				 (eql index (role role)))
		       do (setf (elt (rank (method role)) index)
				position)
		       if (or (fully-specified-p (rank (method role)))
			      (< (calculate-rank (rank (method role)))
				 (calculate-rank (rank most-specific-method))))
		       do (setf most-specific-method (talent-pointer role)))
		    (loop for delegation in (sheep-direct-parents arg)
		       do (push delegation ordering-stack))))))
    most-specific-method))

;dispatch(selector, args, n)
;  for each index below n
;    position := 0
;    push args[index] on ordering stack
;    while ordering stack is not empty
;      arg := pop ordering stack
;      for each role on arg with selector and index
;        rank[role's method][index] := position
;        if rank[role's method] is fully specified
;          if no most specific method
;             or rank[role's method] < rank[most specific method]
;            most specific method := role's method
;      for each delegation on arg
;        push delegation on ordering stack
;      position := position + 1
;  return most specific method
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
