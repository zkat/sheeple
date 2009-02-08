;; This file is part of Sheeple

;; buzzwords.lisp
;;
;; Implementation of Sheeple's buzzwords+messages (generic functions + methods)
;;
;; TODO:
;; * There should be an error if defbuzzword tries to clobber a function or generic-func
;; * Keep trying to write (call-next-message) and :around
;; * Write more unit tests
;; * AFTER unit tests... clean up code, run tests
;; * AFTER cleanup... --omg-optimized, run tests
;; * DOCUMENTATION!!1
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :sheeple)
;(declaim (optimize (speed 3) (safety 1)))
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
   (qualifiers
    :initarg :qualifiers
    :initform nil
    :accessor message-qualifiers)
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

(defgeneric buzzword-p (buzzword?))
(defmethod buzzword-p (anything-else)
  (declare (ignore anything-else))
  nil)
(defmethod buzzword-p ((buzzword standard-buzzword))
  (declare (ignore buzzword))
  t)

(defgeneric message-p (message?))
(defmethod message-p (anything-else)
  (declare (ignore anything-else))
  nil)
(defmethod message-p ((message standard-message))
  (declare (ignore message))
  t)

(defun participant-p (sheep message-name)
  (when (member-if (lambda (role) (equal message-name (role-name role)))
		   (sheep-direct-roles sheep))
    t))

;;;
;;; Buzzword/message definition
;;;

;;; Buzzword table
(let ((buzzword-table (make-hash-table :test #'equal)))

  (defun find-buzzword (name &optional (errorp t))
    (let ((buzz (gethash name buzzword-table nil)))
      (if (and (null buzz) errorp)
	  (error 'no-such-buzzword)
	  buzz)))
  
  (defun (setf find-buzzword) (new-value name)
    (setf (gethash name buzzword-table) new-value))
  
  (defun forget-all-buzzwords ()
    (clrhash buzzword-table)
    t)
  
  (defun forget-buzzword (name)
    (remhash name buzzword-table))
  
) ; end buzzword-table closure
(define-condition no-such-buzzword (sheeple-error) ())

(defgeneric available-messages (sheep)
  (:documentation "Returns a list of message-names that SHEEP can participate in."))
(defmethod available-messages ((sheep standard-sheep))
  (let ((personal-role-names (mapcar (lambda (role) (role-name role))
				     (sheep-direct-roles sheep))))
    (remove-duplicates
     (flatten
      (append personal-role-names (mapcar #'available-messages (sheep-direct-parents sheep)))))))

;;; Buzzword definition
(define-condition clobbering-function-definition (warning) ())

(defun ensure-buzzword (&key name documentation)
  (if (find-buzzword name nil)
      (find-buzzword name)
      (let ((buzzword (make-instance 'standard-buzzword
				     :name name
				     :documentation documentation)))
	(setf (find-buzzword name) buzzword)
	(when (fboundp name)
	  (warn 'clobbering-function-definition))
	(setf (fdefinition name) (lambda (&rest args) (apply-buzzword name args)))
	buzzword)))

;;; Message definition
(defun ensure-message (&key name qualifiers lambda-list participants function body)
  (when (not (find-buzzword name nil))
    (progn
      (warn 'style-warning)
      (ensure-buzzword
       :name name)))
  (let* ((target-sheeple (sheepify-list participants))
	 (message (make-instance 'standard-message
				 :name name
				 :qualifiers qualifiers
				 :lambda-list lambda-list
				 :participants participants
				 :body body
				 :function function)))
    (add-message-to-buzzword message (find-buzzword name))
    (remove-messages-with-name-qualifiers-and-participants name qualifiers target-sheeple)
    (add-message-to-sheeple name message target-sheeple)
    message))

(defun add-message-to-buzzword (message buzzword)
  (pushnew message (buzzword-messages buzzword)))

(defun remove-messages-with-name-qualifiers-and-participants (name qualifiers participants)
  (loop for sheep in participants
       do (loop for role in (sheep-direct-roles sheep)
	       do (when (and (equal name (role-name role))
			     (equal participants
				    (message-participants
				     (message-pointer role)))
			     (equal qualifiers
				    (message-qualifiers
				     (message-pointer role))))
		    (delete-role role sheep)))))

(defun delete-role (role sheep)
  (setf (sheep-direct-roles sheep)
	(remove role (sheep-direct-roles sheep)))
  (setf (buzzword-messages (find-buzzword (role-name role)))
	(remove (message-pointer role) (buzzword-messages (find-buzzword (role-name role))))))

(defun add-message-to-sheeple (name message sheeple)
  (loop 
     for sheep in sheeple
     for i upto (1- (length sheeple))
     do (push (make-instance 'standard-message-role
			     :name name
			     :position i
			     :message-pointer message) 
	      (sheep-direct-roles sheep))))

(defun undefine-message (&key name qualifiers participants)
  (remove-messages-with-name-qualifiers-and-participants name qualifiers participants))

(defun undefine-buzzword (&key name)
  (let ((buzzword (find-buzzword name nil)))
    (when buzzword
     (loop for message in (buzzword-messages buzzword)
	do (loop for participant in (message-participants message)
	      do (loop for role in (sheep-direct-roles participant)
		    do (delete-role role participant))))
     (forget-buzzword name)
     (fmakunbound name)
     buzzword)))

;;; Macros
(defmacro defbuzzword (name &optional (docstring ""))
  `(ensure-buzzword
    :name ',name
    :documentation ,docstring))

(defmacro defmessage (&rest args)
  (multiple-value-bind (name qualifiers lambda-list participants body)
      (parse-defmessage args)
    `(ensure-message
      :name ',name
      :qualifiers ',qualifiers
      :lambda-list ,(extract-lambda-list lambda-list)
      :participants ,(extract-participants participants)
      :function (block ,name ,(make-message-lambda lambda-list body))
      :body '(block ,name ,@body))))

(defun make-message-lambda (lambda-list body)
  `(lambda (args next-messages)
     (apply
      (lambda ,(eval (extract-lambda-list lambda-list))
	(flet ((next-message-p ()
		 (not (null next-messages)))
	       (call-next-message (&rest cnm-args)
		 (funcall (message-function (car next-messages))
			  (or cnm-args
			      args)
			  (cdr next-messages))))
	  ,@body)) args)))

(defun parse-defmessage (args)
  (let ((name (car args))
	(qualifiers nil)
	(lambda-list nil)
	(body nil)
	(parse-state :qualifiers))
    (dolist (arg (cdr args))
      (ecase parse-state
	(:qualifiers
	 (if (and (atom arg) (not (null arg)))
	     (push arg qualifiers)
	     (progn (setf lambda-list arg)
		    (setf parse-state :body))))
	(:body (setf body (list arg)))))
    (values name
	    qualifiers
	    lambda-list
	    lambda-list
	    body)))

(defmacro undefmessage (&rest args)
  (multiple-value-bind (name qualifiers lambda-list)
      (parse-undefmessage args)
    `(undefine-message
      :name ',name
      :qualifiers ',qualifiers
      :participants ,(extract-participants lambda-list))))

(defun parse-undefmessage (args)
  (let ((name (car args))
	(qualifiers nil)
	(lambda-list nil))
    (dolist (arg (cdr args))
      (if (and (atom arg) (not (null arg)))
	  (push arg qualifiers)
	  (setf lambda-list arg)))
    (values name
	    qualifiers
	    lambda-list)))

(defmacro undefbuzzword (name)
  `(undefine-buzzword
    :name ',name))

;;; Macro tools
(defun extract-lambda-list (lambda-list)
  `(list ,@(mapcar #'extract-var-name lambda-list)))
(defun extract-var-name (item)
  (if (listp item)
      `',(car item)
      `(confirm-var-name ',item)))

(defun confirm-var-name (var-name)
  (if (symbolp var-name)
      var-name
      (error "Invalid var name.")))

(defun extract-participants (lambda-list)
  `(list ,@(mapcar #'extract-participant-sheep lambda-list)))
(defun extract-participant-sheep (item)
  (if (listp item)
      `(confirm-sheep ,(cadr item))
      `=dolly=))

;;;
;;; Message dispatch
;;;

(defun primary-message-p (message)
  (null (message-qualifiers message)))

(defun before-message-p (message)
  (when (member :before (message-qualifiers message))
    t))

(defun after-message-p (message)
  (when (member :after (message-qualifiers message))
    t))

(defun around-message-p (message)
  (when (member :around (message-qualifiers message))
    t))

(defun apply-buzzword (selector args)
  (let ((messages (find-applicable-messages selector
					    (sheepify-list args))))
    (apply-messages messages args)))

(defun apply-messages (messages args)
  (let ((around (find-if #'around-message-p messages)))
    (if around
	(apply-message around args (remove around messages))
    	(let ((primaries (remove-if-not #'primary-message-p messages))
	      (befores (remove-if-not #'before-message-p messages))
	      (afters (remove-if-not #'after-message-p messages)))
	  (when (null primaries)
	    (error "No primary messages"))
	  (dolist (before befores)
	    (apply-message before args nil))
	  (multiple-value-prog1
	      (apply-message (car primaries) args (cdr primaries))
	    (dolist (after (reverse afters))
	      (apply-message after args nil)))))))

(defun apply-message (message args next-messages)
  (let ((function (message-function message)))
    (funcall function args next-messages)))

(defun find-applicable-messages  (selector args)
  "Returns the most specific message using SELECTOR and ARGS."
  (let ((n (length args))
	(discovered-messages nil)
	(contained-applicable-messages nil))
    (loop 
       for arg in args
       for index upto (1- n)
       do (let ((curr-sheep-list (sheep-hierarchy-list arg)))
	    (loop
	       for curr-sheep in curr-sheep-list
	       for hierarchy-position upto (1- (length curr-sheep-list))
	       do (dolist (role (sheep-direct-roles curr-sheep))
		    (when (and (equal selector (role-name role))
			       (eql index (role-position role)))
			  (let ((curr-message (message-pointer role)))
			    (when (= n (length (message-lambda-list curr-message)))
			      (when (not (member curr-message
						 discovered-messages
						 :key #'message-container-message))
				(pushnew (contain-message curr-message) discovered-messages))
			      (let ((contained-message (find curr-message
							     discovered-messages
							     :key #'message-container-message)))
				(setf (elt (message-container-rank contained-message) index) 
				      hierarchy-position)
				(when (fully-specified-p (message-container-rank contained-message))
				  (pushnew contained-message contained-applicable-messages :test #'equalp))))))))))
    (if contained-applicable-messages
	(unbox-messages (sort-applicable-messages contained-applicable-messages))
	(error 'no-applicable-messages))))

(defun unbox-messages (messages)
  (mapcar #'message-container-message messages))

(defun sort-applicable-messages (message-list &key (rank-key #'<))
  (sort message-list rank-key
	:key (lambda (contained-message)
	       (calculate-rank-score (message-container-rank contained-message)))))

(defun contain-message (message)
  (make-message-container
   :message message
   :rank (make-array (length (message-lambda-list message))
		     :initial-element nil)))

(defstruct message-container
  message
  rank)

(define-condition no-applicable-messages (sheeple-error) ())
(define-condition no-most-specific-message (sheeple-error) ())

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