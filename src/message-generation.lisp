;; This file is part of Sheeple

;; message-generation.lisp
;;
;; Message and role metasheeple, message definition and undefinition, role management.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :sheeple)

(defstruct (message (:constructor %make-message))
  (name nil)
  (buzzword nil)
  (qualifiers nil)
  (lambda-list nil)
  (participants nil)
  (body '(lambda () nil))
  (function (lambda () nil))
  (documentation ""))

(defstruct (role (:constructor %make-role))
  (name nil)
  (position 0)
  (message-pointer nil))

(defun generate-message (&key name
			 qualifiers
			 lambda-list
			 buzzword
			 participants
			 function
			 body
			 (documentation ""))
  (let ((message (%make-message
		   :name name
		   :buzzword buzzword
		   :lambda-list lambda-list
		   :qualifiers qualifiers
		   :participants participants
		   :function function
		   :body body
		   :documentation documentation)))
    (add-message-to-buzzword message buzzword)
    (remove-messages-with-name-qualifiers-and-participants name qualifiers participants)
    (add-message-to-sheeple name message participants)
    message))

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
		   :name name
		   :buzzword buzzword
		   :lambda-list lambda-list
		   :participants target-sheeple
		   all-keys)))
    message))

(defun create-bw-lambda-list (lambda-list)
  ;;; Create a buzzword lambda list from a message lambda list
  (loop for x in lambda-list
     collect (if (consp x) (list (car x)) x)
     if (eq x '&key) do (loop-finish)))

(defun add-message-to-buzzword (message buzzword)
  (set-arg-info buzzword :new-message message)
  (pushnew message (buzzword-messages buzzword)))

(defun remove-messages-with-name-qualifiers-and-participants (name qualifiers participants)
  (loop for sheep in participants
       do (loop for role in (sheep-direct-roles sheep)
	       do (when (and (equal name (role-name role))
			     (equal participants
				    (message-participants
				     (role-message-pointer role)))
			     (equal qualifiers
				    (message-qualifiers
				     (role-message-pointer role))))
		    (delete-role role sheep)))))

(defun delete-role (role sheep)
  (setf (sheep-direct-roles sheep)
	(remove role (sheep-direct-roles sheep)))
  (setf (buzzword-messages (find-buzzword (role-name role)))
	(remove (role-message-pointer role) (buzzword-messages (find-buzzword (role-name role))))))

(defun add-message-to-sheeple (name message sheeple)
  (loop 
     for sheep in sheeple
     for i upto (1- (length sheeple))
     do (let ((role (%make-role
		     :name name
		     :position i
		     :message-pointer message)))
	  (push role
		(sheep-direct-roles sheep)))))

(defun undefine-message (name &key qualifiers participants)
  (remove-messages-with-name-qualifiers-and-participants name qualifiers participants))

(defun available-messages (sheep)
  (let ((personal-role-names (mapcar (lambda (role) (role-name role))
				     (sheep-direct-roles sheep))))
    (remove-duplicates
     (flatten
      (append personal-role-names (mapcar #'available-messages (sheep-direct-parents sheep)))))))

;; TODO
;(defun find-message (selector qualifiers participants &optional (errorp t)))

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
  `(lambda (args next-messages)
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
	    (lambda ,lambda-list
	      ,@body) args)))))

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

