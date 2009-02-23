;; This file is part of Sheeple

;; message-generation.lisp
;;
;; Message and role metasheeple, message definition and undefinition, role management.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :sheeple)

(defparameter the-standard-message-metasheep-form
  '(clone ()
    ((name
      'standard-message-metasheep
      :cloneform 'standard-message)
     (buzzword
      nil
      :cloneform nil)
     (qualifiers
      nil
      :cloneform nil)
     (lambda-list
      nil
      :cloneform nil)
     (participants
      nil
      :cloneform nil)
     (body
      nil
      :cloneform '(lambda () nil))
     (function
      nil
      :cloneform (lambda () nil))
     (documentation
      "standard message metasheep"
      :cloneform ""))))

(defun message-name (message)
  (property-value message 'name))
(defun (setf message-name) (new-value message)
  (setf (property-value message 'name) new-value))
(defun message-buzzword (message)
  (property-value message 'buzzword))
(defun (setf message-buzzword) (new-value message)
  (setf (property-value message 'buzzword) new-value))
(defun message-qualifiers (message)
  (property-value message 'qualifiers))
(defun (setf message-qualifiers) (new-value message)
  (setf (property-value message 'qualifiers) new-value))
(defun message-lambda-list (message)
  (property-value message 'lambda-list))
(defun (setf message-lambda-list) (new-value message)
  (setf (property-value message 'lambda-list) new-value))
(defun message-participants (message)
  (property-value message 'participants))
(defun (setf message-participants) (new-value message)
  (setf (property-value message 'participants) new-value))
(defun message-body (message)
  (property-value message 'body))
(defun (setf message-body) (new-value message)
  (setf (property-value message 'body) new-value))
(defun message-function (message)
  (property-value message 'function))
(defun (setf message-function) (new-value message)
  (setf (property-value message 'function) new-value))
(defun message-documentation (message)
  (property-value message 'documentation))
(defun (setf message-documentation) (new-value message)
  (setf (property-value message 'documentation) new-value))

(defparameter the-standard-role-metasheep-form
  '(clone ()
    ((name
      'standard-role-metasheep
      :cloneform 'standard-role)
     (position
      0
      :cloneform nil)
     (message-pointer
      nil
      :cloneform nil))))

(defun role-name (role)
  (property-value role 'name))
(defun (setf role-name) (new-value role)
  (setf (property-value role 'name) new-value))

(defun role-position (role)
  (property-value role 'position))
(defun (setf role-position) (new-value role)
  (setf (property-value role 'position) new-value))

(defun message-pointer (role)
  (property-value role 'message-pointer))
(defun (setf message-pointer) (new-value role)
  (setf (property-value role 'message-pointer) new-value))

(defun spawn-sheep-standard-message (metasheep
				     &key name
				     qualifiers
				     lambda-list
				     participants
				     function
				     body
				     (documentation "")
				     &allow-other-keys)
  (let* ((buzzword (find-buzzword name))
	 (message (clone (metasheep)
			 ((name name)
			  (buzzword buzzword)
			  (qualifiers qualifiers)
			  (lambda-list lambda-list)
			  (participants participants)
			  (function function)
			  (body body)
			  (documentation documentation)))))
    (add-message-to-buzzword message buzzword)
    (remove-messages-with-name-qualifiers-and-participants name qualifiers participants)
    (add-message-to-sheeple name message participants)
    message))

(defun ensure-message (name &rest all-keys
		       &key participants
		       &allow-other-keys)
  
  (when (not (find-buzzword name nil))
    (progn
      (warn 'style-warning)
      (ensure-buzzword
       name)))
  (let* ((buzzword (find-buzzword name))
	 (target-sheeple (sheepify-list participants))
	 (message (apply
		   (if (eql (buzzword-message-metasheep buzzword) =standard-message-metasheep=)
		       #'spawn-sheep-standard-message
		       #'spawn-sheep-using-metasheep-prototype)
		   (buzzword-message-metasheep buzzword)
		   :name name
		   :participants target-sheeple
		   all-keys)))
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

;; TODO: make a role factory thing
(defun add-message-to-sheeple (name message sheeple)
  (let ((role-metasheep (buzzword-role-metasheep (find-buzzword name))))
    (loop 
       for sheep in sheeple
       for i upto (1- (length sheeple))
       do (let ((role (clone (role-metasheep)
			     ((name name)
			      (position i)
			      (message-pointer message)))))
	    (push role
		  (sheep-direct-roles sheep))))))

(defun undefine-message (name &key qualifiers participants)
  (remove-messages-with-name-qualifiers-and-participants name qualifiers participants))

(defun available-messages (sheep)
  (if (std-sheep-p sheep)
      (std-available-messages sheep)
      (available-messages-using-metasheep (sheep-metasheep sheep) sheep)))
(defun std-available-messages (sheep)
  (let ((personal-role-names (mapcar (lambda (role) (role-name role))
				     (sheep-direct-roles sheep))))
    (remove-duplicates
     (flatten
      (append personal-role-names (mapcar #'available-messages (sheep-direct-parents sheep)))))))

;; TODO
;(defun find-message (selector qualifiers participants &optional (errorp t)))

(defun add-readers-to-sheep (readers prop-name sheep)
  (loop for reader in readers
     do (ensure-buzzword reader)
     do (ensure-message reader
			:lambda-list '(sheep)
			:participants (list sheep)
			:body `(property-value sheep ',prop-name)
			:function (eval (make-message-lambda '(sheep) 
							     `((property-value sheep ',prop-name)))))))

(defun add-writers-to-sheep (writers prop-name sheep)
  (loop for writer in writers
     do (ensure-buzzword writer)
     do (ensure-message writer
			:lambda-list '(new-value sheep)
			:participants (list =t= sheep)
			:body `(setf (property-value sheep ',prop-name) new-value)
			:function (eval (make-message-lambda '(new-value sheep) 
							     `((setf (property-value sheep ',prop-name)
								     new-value)))))))

;;; macro
(defmacro defmessage (&rest args)
  (multiple-value-bind (name qualifiers lambda-list body)
      (parse-defmessage args)
    (eval-when (:compile-toplevel :load-toplevel :execute)
      `(ensure-message
	',name
	:qualifiers ',qualifiers
	:lambda-list ,(extract-lambda-list lambda-list)
	:participants ,(extract-participants lambda-list)
	:function ,(make-message-lambda lambda-list `((block ,name ,@body)))
	:body '(block ,name ,@body)))))

(defun make-message-lambda (lambda-list body)
  `(lambda (args next-messages)
     (flet ((next-message-p ()
		 (not (null next-messages)))
	       (call-next-message (&rest cnm-args)
		 (funcall (message-function (car next-messages))
			  (or cnm-args
			      args)
			  (cdr next-messages))))
	  (declare (ignorable #'next-message-p #'call-next-message))
	  (apply
	   (lambda ,(eval (extract-lambda-list lambda-list))
	     ,@body) args))))

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
	(:body (push arg body))))
    (values name
	    qualifiers
	    lambda-list
	    (nreverse body))))

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
      `=t=))

(defmacro undefmessage (&rest args)
  (multiple-value-bind (name qualifiers lambda-list)
      (parse-undefmessage args)
    `(undefine-message
      ',name
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

