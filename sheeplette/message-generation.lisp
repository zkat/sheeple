;; message-generation.lisp
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :sheeplette)

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
  (get-property message 'name))
(defun (setf message-name) (new-value message)
  (setf (get-property message 'name) new-value))
(defun message-buzzword (message)
  (get-property message 'buzzword))
(defun (setf message-buzzword) (new-value message)
  (setf (get-property message 'buzzword) new-value))
(defun message-qualifiers (message)
  (get-property message 'qualifiers))
(defun (setf message-qualifiers) (new-value message)
  (setf (get-property message 'qualifiers) new-value))
(defun message-lambda-list (message)
  (get-property message 'lambda-list))
(defun (setf message-lambda-list) (new-value message)
  (setf (get-property message 'lambda-list) new-value))
(defun message-participants (message)
  (get-property message 'participants))
(defun (setf message-participants) (new-value message)
  (setf (get-property message 'participants) new-value))
(defun message-body (message)
  (get-property message 'body))
(defun (setf message-body) (new-value message)
  (setf (get-property message 'body) new-value))
(defun message-function (message)
  (get-property message 'function))
(defun (setf message-function) (new-value message)
  (setf (get-property message 'function) new-value))
(defun message-documentation (message)
  (get-property message 'documentation))
(defun (setf message-documentation) (new-value message)
  (setf (get-property message 'documentation) new-value))

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
  (get-property role 'name))
(defun (setf role-name) (new-value role)
  (setf (get-property role 'name) new-value))

(defun role-position (role)
  (get-property role 'position))
(defun (setf role-position) (new-value role)
  (setf (get-property role 'position) new-value))

(defun message-pointer (role)
  (get-property role 'message-pointer))
(defun (setf message-pointer) (new-value role)
  (setf (get-property role 'message-pointer) new-value))

(defun generate-sheep-standard-message (metasheep
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
    (add-message-to-sheeple name message participants)))

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
		   (if (eql (sheep-metasheep buzzword) =standard-buzzword-metasheep=)
		       #'generate-sheep-standard-message
		       #'generate-sheep)
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

(defun undefine-message (&key name qualifiers participants)
  (remove-messages-with-name-qualifiers-and-participants name qualifiers participants))

(defun available-messages (sheep)
  (if (eql =standard-sheep-metasheep= (sheep-metasheep sheep))
      (std-available-messages sheep)
      (available-messages-using-metasheep (sheep-metasheep sheep))))
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
			:body `(get-property sheep ',prop-name)
			:function (eval (make-message-lambda '(sheep) 
							     `((get-property sheep ',prop-name)))))))

(defun add-writers-to-sheep (writers prop-name sheep)
  (loop for writer in writers
     do (ensure-buzzword writer)
     do (ensure-message writer
			:lambda-list '(new-value sheep)
			:participants (list =dolly= sheep)
			:body `(setf (get-property sheep ',prop-name) new-value)
			:function (eval (make-message-lambda '(new-value sheep) 
							     `((setf (get-property sheep ',prop-name)
								     new-value)))))))

;;; Macro
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
	  (declare (ignorable #'next-message-p #'call-next-message))
	  (funcall 
	   (lambda ()
	     ,@body)))) args)))

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

