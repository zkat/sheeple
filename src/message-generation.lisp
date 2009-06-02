;; This file is part of Sheeple

;; message-generation.lisp
;;
;; Message and role metasheeple, message definition and undefinition, role management.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :sheeple)

(declaim (optimize (speed 3) (safety 1) (debug 1)))
(defstruct (message (:constructor %make-message))
  (name nil)
  (qualifiers nil)
  (lambda-list nil)
  (body '(lambda () nil))
  (function (lambda () nil))
  (documentation ""))

(defun message-buzzword (message)
  (find-buzzword (message-name message) nil))

(defstruct (role (:constructor %make-role))
  (name nil)
  (position 0)
  (message nil))

(defun role-buzzword (role)
  (find-buzzword (role-name role) nil))

(defun participant-p (sheep message-name)
  (when (member-if (lambda (role) (equal message-name (role-name role)))
		   (sheep-direct-roles sheep))
    t))

(defun ensure-message (name &rest all-keys
		       &key participants
		       lambda-list
		       &allow-other-keys)
  (when (not (find-buzzword name nil))
    (progn
      (warn 'style-warning)
      ;; FIXME: can't just give the lambda-list over. Should prepare it for buzzwords
      (ensure-buzzword
       name :lambda-list (create-bw-lambda-list lambda-list))))
  (let* ((buzzword (find-buzzword name))
	 (target-sheeple (sheepify-list participants))
	 (message (apply
		   #'generate-message
		   :buzzword buzzword
		   :lambda-list lambda-list
		   :participants target-sheeple
		   all-keys)))
    (clear-memo-table buzzword)
    message))

(defun generate-message (&key qualifiers
			 lambda-list
			 participants
			 buzzword
			 function
			 body
			 (documentation ""))
  (let ((message (%make-message
		   :name (buzzword-name buzzword)
		   :lambda-list lambda-list
		   :qualifiers qualifiers
		   :function function
		   :body body
		   :documentation documentation)))
    (add-message-to-buzzword message buzzword)
    (remove-specific-message buzzword qualifiers participants)
    (add-message-to-sheeple buzzword message participants)
    message))

(defun create-bw-lambda-list (lambda-list)
  ;;; Create a buzzword lambda list from a message lambda list
  (loop for x in lambda-list
     collect (if (consp x) (list (car x)) x)
     if (eq x '&key) do (loop-finish)))

(defun add-message-to-buzzword (message buzzword)
  (set-arg-info buzzword :new-message message)
  (push message (buzzword-messages buzzword)))

(defun remove-specific-message (buzzword qualifiers participants)
  (let ((message (find-if (lambda (msg)
			    (equal (message-qualifiers msg)
				   qualifiers))
			  (%find-applicable-messages
			   buzzword participants :errorp nil))))
    (when (and message
	       (every (lambda (sheep)
			(participant-p sheep (message-name message)))
		      participants))
      (loop for sheep in participants
	 for i from 0
	 do (loop for role in (sheep-direct-roles sheep)
	       do (let ((role-message (role-message role)))
		    (when (and
			   (equal message role-message)
			   (= i (role-position role)))
		      (delete-role role sheep)))))
      (delete-message message))))

(defun delete-message (message)
  (let ((buzzword (message-buzzword message)))
    (setf (buzzword-messages buzzword)
	  (delete message (buzzword-messages buzzword)))))

(defun delete-role (role sheep)
  (setf (sheep-direct-roles sheep)
	(remove role (sheep-direct-roles sheep))))

(defun add-message-to-sheeple (buzzword message sheeple)
  (loop 
     for sheep in sheeple
     for i upto (1- (length sheeple))
     do (let ((role (%make-role
		     :name (buzzword-name buzzword)
		     :position i
		     :message message)))
	  (push role
		(sheep-direct-roles sheep)))))

(defun undefine-message (name &key qualifiers participants)
  (let ((bw (find-buzzword name nil)))
    (when bw
     (remove-applicable-message bw qualifiers participants)
     (clear-memo-table bw)
     t)))

(defun remove-applicable-message (buzzword qualifiers participants)
  (let ((message (find-if (lambda (msg)
			    (equal (message-qualifiers msg)
				   qualifiers))
			  (%find-applicable-messages
			   buzzword participants :errorp nil))))
    (when message
      (loop for sheep in participants
	 for i from 0
	 do (loop for role in (sheep-direct-roles sheep)
	       do (let ((role-message (role-message role)))
		    (when (and
			   (equal message role-message)
			   (= i (role-position role)))
		      (delete-role role sheep)))))
      (delete-message message))))

(defun available-messages (sheep)
  (let ((roles (loop for role in (sheep-direct-roles sheep)
		    collect (vector (role-name role) (role-position role)))))
    (remove-duplicates
     (flatten
      (append roles (mapcar #'available-messages (sheep-direct-parents sheep)))))))

(defun add-readers-to-sheep (readers prop-name sheep)
  (loop for reader in readers
     do (ensure-buzzword reader :lambda-list '(sheep))
     do (ensure-message reader
			:lambda-list '(sheep)
			:participants (list sheep)
			:body `(property-value sheep ',prop-name)
			:function (eval (make-message-lambda reader
							     '(sheep) 
							     `((property-value sheep ',prop-name)))))))

(defun add-writers-to-sheep (writers prop-name sheep)
  (loop for writer in writers
     do (ensure-buzzword writer :lambda-list '(new-value sheep))
     do (ensure-message writer
			:lambda-list '(new-value sheep)
			:participants (list =t= sheep)
			:body `(setf (property-value sheep ',prop-name) new-value)
			:function (eval (make-message-lambda writer
							     '(new-value sheep) 
							     `((setf (property-value sheep ',prop-name)
								     new-value)))))))

;;; macro
(defmacro defmessage (&rest args)
  (multiple-value-bind (name qualifiers specialized-lambda-list docstring body)
      (parse-defmessage args)
    (multiple-value-bind (parameters ll participants required)
	(parse-specialized-lambda-list specialized-lambda-list)
      (declare (ignore parameters))
      (declare (ignore required))
      `(ensure-message
	',name
	:qualifiers ',qualifiers
	;; TODO - use the new stuff
	:lambda-list ',ll
	:participants (list ,@participants)
	:documentation ,docstring
	:function ,(make-message-lambda name ll body)
	:body '(block ,name ,@body)))))

(defun make-message-lambda (name lambda-list body)
  (let* ((bw (find-buzzword name nil))
	 (key/restp (when bw (arg-info-key/rest-p (buzzword-arg-info bw))))
	 (ll (if key/restp
		 (append lambda-list '(&allow-other-keys))
		 lambda-list)))
    `(lambda (args next-emfun)
       (declare (ignorable next-emfun))
       (flet ((next-message-p ()
		(not (null next-emfun)))
	      (call-next-message (&rest cnm-args)
		(if (null next-emfun)
		    (error "No next message")
		    (funcall next-emfun (or cnm-args args)))))
	 (declare (ignorable #'next-message-p #'call-next-message))
	 (block ,(if (listp name)
		     (cadr name)
		     name)
	   (apply
	    (lambda ,ll
	      ,@body) args))))))

(defun parse-defmessage (args)
  (let ((name (car args))
	(qualifiers nil)
	(lambda-list nil)
	(docstring nil)
	(body nil)
	(parse-state :qualifiers))
    (dolist (arg (cdr args))
      (ecase parse-state
	(:qualifiers
	 (if (and (atom arg) (not (null arg)))
	     (push arg qualifiers)
	     (progn (setf lambda-list arg)
		    (setf parse-state :docstring))))
	(:docstring
	 (if (stringp arg)
	     (setf docstring arg)
	     (push arg body))
	 (setf parse-state :body))
	(:body (push arg body))))
    (values name
	    qualifiers
	    lambda-list
	    docstring
	    (nreverse body))))

(defun extract-var-name (item)
  (if (listp item)
      `',(car item)
      `(confirm-var-name ',item)))

(defun confirm-var-name (var-name)
  (if (symbolp var-name)
      var-name
      (error "Invalid var name.")))

(defmacro undefmessage (&rest args)
  (multiple-value-bind (name qualifiers lambda-list)
      (parse-undefmessage args)
    (multiple-value-bind (iggy1 iggy2 participants iggy3)
	(parse-specialized-lambda-list lambda-list)
      (declare (ignore iggy1 iggy2 iggy3))
      `(undefine-message
	',name
	:qualifiers ',qualifiers
	:participants (list ,@participants)))))

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
