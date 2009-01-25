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

;; buzzwords.lisp
;;
;; Implementation of Sheeple's buzzwords+messages (generic functions + methods)
;;
;; TODO:
;; * Figure out the basic framework for message definition before going over to dispatch again
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :sheeple)

;;;
;;; Message and participation base classes
;;;
(defclass standard-buzzword ()
  ((name
    :initarg :name
    :accessor buzzword-name)
   (messages
    :initform nil
    :accessor buzzword-messages)
   (documentation
    :initarg :documentation
    :accessor buzzword-documentation)))

(defclass standard-message ()
  ((name
    :initarg :name
    :accessor message-name)
   (lambda-list
    :initarg :lambda-list
    :accessor message-lambda-list)
   (participants
    :initarg :participants
    :accessor message-participants)
   (body
    :initarg :body
    :accessor message-body)
   (function
    :initarg :function
    :accessor message-function)
   (documentation
    :initarg :documentation
    :accessor message-documentation)))

(defclass standard-message-role ()
  ((name
    :initarg :name
    :accessor role-name)
   (position
    :initarg :position
    :accessor role-position)
   (message-pointer
    :initarg :message-pointer
    :accessor message-pointer)))

;;;
;;; Message definition
;;;

(let ((buzzword-table (make-hash-table :test #'equal)))

  (defun find-buzzword (name &optional (errorp t))
    (let ((buzz (gethash name buzzword-table nil)))
      (if (and (null buzz) errorp)
	  (error "No buzzword named ~s." name)
	  buzz)))
  
  (defun (setf find-buzzword) (new-value name)
    (setf (gethash name buzzword-table) new-value))
  
  (defun forget-all-buzzwords ()
    (clrhash buzzword-table)
    t)

) ; end buzzword-table closure

(defmacro defbuzzword (name &optional (docstring ""))
  `(ensure-buzzword
    :name ',name
    :documentation ,docstring))

(defun ensure-buzzword (&key name documentation)
  (if (find-buzzword name nil)
      (find-buzzword name)
      (let ((buzzword (make-instance 'standard-buzzword
				     :name name
				     :documentation documentation)))
	(setf (find-buzzword name) buzzword)
	(setf (fdefinition name) #'dispatch-message)
	buzzword)))

(defmacro defmessage (name lambda-list &body body)
  `(ensure-message
    :name ',name
    :lambda-list (extract-lambda-list ',lambda-list)
    :participants (extract-participants ',lambda-list)
    :body (block ,name ',body)))

(defun extract-lambda-list (lambda-list)
  (mapcar #'extract-var-name lambda-list))
(defun extract-var-name (item)
  (if (atom item)
      item
      (car item)))

(defun extract-participants (lambda-list)
  (mapcar #'extract-participant-sheep lambda-list))
(defun extract-participant-sheep (item)
  (if (atom item)
      dolly
      (eval (second item))))

(defun ensure-message (&key name lambda-list participants body)
  (if (not (find-buzzword name nil))
      (error "There is no buzzword defined for ~S" name)
      (let ((message (make-instance 'standard-message
				    :name name
				    :lambda-list lambda-list
				    :participants participants
				    :body body
				    :function body))
	    (target-sheeple (ensure-sheeple participants)))
	(add-message-to-buzzword message (find-buzzword name))
	(remove-messages-with-name-and-participants name target-sheeple)
	(add-message-to-sheeple name message target-sheeple)
	message)))

(defun ensure-sheeple (maybe-sheeple)
  (mapcar #'ensure-sheep maybe-sheeple))

(defun ensure-sheep (sheep)
  (if (not (sheep-p sheep))
      (error "~S is not a sheep." sheep)
      sheep))

(defun add-message-to-buzzword (message buzzword)
  (pushnew message (buzzword-messages buzzword)))

(defun remove-messages-with-name-and-participants (name participants)
  ;; Keep a watchful eye on this. It only *seems* to work.
  (mapc (lambda (sheep) 
	    (mapc (lambda (role) 
		    (when (and (eql name (name role))
			       (equal participants
				      (message-participants
				       (message-pointer role))))
		      (setf (sheep-direct-roles sheep)
			    (remove role (sheep-direct-roles sheep)))))
		  (sheep-direct-roles sheep)))
	participants))

(defun add-message-to-sheeple (name message sheeple)
  (loop 
     for sheep in sheeple
     for i upto (1- (length sheeple))
     do (push (make-instance 'standard-message-role
			     :name name
			     :position i
			     :message-pointer message) 
	      (sheep-direct-roles sheep))))

;;;
;;; Message dispatch
;;;

;; dispatch(selector, args, n)
;;  for each index below n
;;    position := 0
;;    push args[index] on ordering stack
;;    while ordering stack is not empty
;;      arg := pop ordering stack
;;      for each message-property on arg with selector and index
;;        rank[message-property's message][index] := position
;;        if rank[message-property's message] is fully specified
;;          if no most specific message
;;             or rank[message-property's message] < rank[most specific message]
;;            most specific message := message-property's method
;;      for each ancestor on arg's hierarchy-list
;;        push ancestor on ordering stack
;;      position := position + 1
;;  return most specific message-property
;; FUCK YOU SLATE

(defun dispatch-message (selector &rest args)
  (apply (message-function `(find-most-specific-message ,selector ,@args)) args))

(defun find-most-specific-message (selector &rest args)
  "Returns the most specific message using SELECTOR and ARGS."
  ;; This shit is bugged to all hell and it's a huge, disgusting algorithm. Fix that shit.
  (let ((n (length args))
	(most-specific-message nil)
	(ordering-stack nil)
	(discovered-messages nil))
    (loop 
       for index upto (1- n)
       for position upto (1- n)
       do (let ((position 0))
	    (push (elt args index) ordering-stack)
	    (loop 
	       while ordering-stack
	       do (let ((arg (pop ordering-stack)))
		    (loop
		       for role in (sheep-direct-roles arg)
		       when (and (eql selector (name role))
				 (eql index (position role)))
		       do (pushnew (message-pointer role) 
				   discovered-messages)
		       do (setf (elt (message-rank (message-pointer role)) index)
				position)
		       if (or (fully-specified-p (message-rank (message-pointer role)))
			      (< (calculate-rank (message-rank (message-pointer role)))
				 (calculate-rank (message-rank most-specific-message))))
		       do (setf most-specific-message (message-pointer role)))
		    (add-ancestors-to-ordering-stack arg ordering-stack)))))
    (mapcar #'reset-message-rank discovered-messages)
    most-specific-message))

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

(defun reset-message-rank (message)
  (loop for item across (message-rank message)
     do (setf item nil)))
