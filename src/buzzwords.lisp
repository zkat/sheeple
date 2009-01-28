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
;; * Write unit tests
;; * AFTER unit tests... clean up code, run tests
;; * AFTER cleanup... --omg-optimized, run tests
;; * Write utilities to make management of dynamic definitions easier
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
;;; Buzzword/message definition
;;;

;;; Macros
(defmacro defbuzzword (name &optional (docstring ""))
  `(ensure-buzzword
    :name ',name
    :documentation ,docstring))

(defmacro defmessage (name lambda-list &body body)
  `(ensure-message
    :name ',name
    :lambda-list ,(extract-lambda-list lambda-list)
    :participants ,(extract-participants lambda-list)
    :function (lambda ,(eval (extract-lambda-list lambda-list))
		 (block ,name ,@body))
    :body '(block ,name ,@body)))

;;; Buzzword table
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

;;; Buzzword definition
(defun ensure-buzzword (&key name documentation)
  (if (find-buzzword name nil)
      (find-buzzword name)
      (let ((buzzword (make-instance 'standard-buzzword
				     :name name
				     :documentation documentation)))
	(setf (find-buzzword name) buzzword)
	(setf (fdefinition name) (lambda (&rest args) (apply-message name args)))
	buzzword)))

;;; Message definition
(defun ensure-message (&key name lambda-list participants function body)
  (when (not (find-buzzword name nil))
    (progn
      (warn "Automatically defining a buzzword for ~S" name)
      (ensure-buzzword
       :name name)))
  (let* ((target-sheeple (sheepify-list participants))
	 (message (make-instance 'standard-message
				 :name name
				 :lambda-list lambda-list
				 :participants participants
				 :body body
				 :function function)))
    (add-message-to-buzzword message (find-buzzword name))
    (remove-messages-with-name-and-participants name target-sheeple)
    (add-message-to-sheeple name message target-sheeple)
    message))

(defun extract-lambda-list (lambda-list)
  `(list ,@(mapcar #'extract-var-name lambda-list)))
(defun extract-var-name (item)
  (if (listp item)
      `',(car item)
      `(confirm-var-name ',item)))

(defun confirm-var-name (var-name)
  (if (symbolp var-name)
      var-name
      (error "Invalid variable name ~s. Variables must be symbols." var-name)))

(defun extract-participants (lambda-list)
  `(list ,@(mapcar #'extract-participant-sheep lambda-list)))
(defun extract-participant-sheep (item)
  (if (listp item)
      `(confirm-sheep ,(cadr item))
      `=dolly=))

(defun get-sheep-from-lambda-item (item)
  (cond ((symbolp item)
	 =dolly=)
	((listp item)
	 (cadr item))
	(t
	 (error "Invalid variable name: ~a. Must be either a list or a symbol" item))))

(defun sheepify-list (sheeple)
  (mapcar #'sheepify sheeple))

(defun sheepify (sheep)
   (if (not (sheep-p sheep))
       (find-fleeced-wolf sheep)
       sheep))

(defun add-message-to-buzzword (message buzzword)
  (pushnew message (buzzword-messages buzzword)))

(defun remove-messages-with-name-and-participants (name participants)
  ;; Keep a watchful eye on this. It only *seems* to work.
  ;; THIS IS UGLY AS ALL FUCK
  (mapc (lambda (sheep) 
	    (mapc (lambda (role) 
		    (when (and (eql name (role-name role))
			       (equal participants
				      (message-participants
				       (message-pointer role))))
		      (setf (sheep-direct-roles sheep)
			    (remove role (sheep-direct-roles sheep)))
		      (setf (buzzword-messages (find-buzzword name))
			    (remove (message-pointer role) (buzzword-messages (find-buzzword name))))))
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

(defun apply-message (selector args)
  (let ((function (message-function (find-most-specific-message selector (sheepify-list args)))))
    (apply function args)))

(defun find-most-specific-message (selector args &optional not-this-message-please)
  "Returns the most specific message using SELECTOR and ARGS."
  ;; This shit is bugged to all hell and it's a huge, disgusting algorithm. Fix that shit.
  ;; taken almost verbatim from Slate's algorithm
  (let ((n (length args))
	(most-specific-message nil))
    (loop 
       for arg in args
       for index upto (1- n)
       do (let ((curr-sheep-list (compute-sheep-hierarchy-list arg)))
	    (loop
	       for curr-sheep in curr-sheep-list
	       for hierarchy-position upto (1- (length curr-sheep-list))
	       do (dolist (role (sheep-direct-roles curr-sheep))
		    (when (and (equal selector (role-name role))
			       (eql index (role-position role)))
			  (let ((curr-message (message-pointer role)))
			    (maybe-add-message-to-table curr-message)
			    (setf (elt (message-rank curr-message) index) hierarchy-position)
			    (when (fully-specified-p (message-rank curr-message)) 
			      (when (or (not most-specific-message)
					(eql not-this-message-please most-specific-message)
					(< (calculate-rank-score (message-rank curr-message))
					   (calculate-rank-score (message-rank most-specific-message))))
				(setf most-specific-message curr-message)))))))))
    (reset-message-ranks)
    (if most-specific-message
	most-specific-message
	(error "No most specific message for buzzword ~a when given args:~%~a" selector args))))

(defun fully-specified-p (rank)
  (loop for item across rank
     do (when (eql item nil)
	  (return-from fully-specified-p nil)))
  t)

(defun calculate-rank-score (rank)
  (let ((total 0))
    (loop for item across rank
       do (when (numberp item)
	    (incf total item)))
    total))

;; Message table
(let ((message-table (make-hash-table :test #'equal)))

  (defun maybe-add-message-to-table (message)
    (unless (gethash message message-table)
      (add-message-to-table message)))
  
  (defun add-message-to-table (message)
    (setf (gethash message message-table) 
	  (make-array (length (message-lambda-list message))
		      :initial-element nil)))
  
  (defun message-rank (message)
    (gethash message message-table))

  (defun reset-message-ranks ()
    (clrhash message-table))
      
  ) ; end message table closure

