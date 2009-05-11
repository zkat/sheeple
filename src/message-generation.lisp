;; This file is part of Sheeple

;; message-generation.lisp
;;
;; Message and role metasheeple, message definition and undefinition, role management.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :sheeple)

(declaim (optimize (speed 1) (safety 3) (debug 1)))
(defstruct (message (:constructor %make-message))
  (name nil)
  (buzzword nil)
  (qualifiers nil)
  (lambda-list nil)
  (body '(lambda () nil))
  (function (lambda () nil))
  (documentation ""))

(defstruct (role (:constructor %make-role))
  (name nil)
  (position 0)
  (message nil))

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
		   :buzzword buzzword
		   :lambda-list lambda-list
		   :qualifiers qualifiers
		   :function function
		   :body body
		   :documentation documentation)))
    (add-message-to-buzzword message buzzword)
    (remove-applicable-messages buzzword qualifiers participants)
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

(defun remove-applicable-messages (buzzword qualifiers participants)
  (let ((applicable-messages (%find-applicable-messages buzzword participants :errorp nil)))
    (when applicable-messages
      (loop for message in applicable-messages
	 do (loop for sheep in participants
	       do (loop for role in (sheep-direct-roles sheep)
		     do (let ((role-message (role-message role)))
			  (when (and (equal message role-message)
				     (equal qualifiers
					    (message-qualifiers role-message)))
			    (delete-role role sheep)
			    (delete-message message)))))))))

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
  (remove-applicable-messages (find-buzzword name) qualifiers participants)
  (clear-memo-table (find-buzzword name))
  t)

(defun available-messages (sheep)
  (let ((personal-role-names (mapcar (lambda (role) (role-name role))
				     (sheep-direct-roles sheep))))
    (remove-duplicates
     (flatten
      (append personal-role-names (mapcar #'available-messages (sheep-direct-parents sheep)))))))

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
  (multiple-value-bind (name qualifiers specialized-lambda-list body)
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
	:function ,(make-message-lambda name ll body)
	:body '(block ,name ,@body)))))

(defun make-message-lambda (name lambda-list body)
  (let* ((bw (find-buzzword name nil))
	 (key/restp (when bw (arg-info-key/rest-p (buzzword-arg-info bw))))
	 (ll (if key/restp
		 (append lambda-list '(&allow-other-keys))
		 lambda-list)))
    `(lambda (args next-messages)
       (declare (ignorable next-messages))
       (flet ((next-message-p ()
		(not (null next-messages)))
	      (call-next-message (&rest cnm-args)
		(funcall (message-function (car next-messages))
			 (or cnm-args
			     args)
			 (cdr next-messages))))
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

