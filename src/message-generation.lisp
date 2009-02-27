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
  (let ((buzzword-number-required-args (length (buzzword-required-arglist buzzword)))
	(message-provided-number-of-args (length (message-specialized-portion message))))
    (cond ((> buzzword-number-required-args message-provided-number-of-args)
	   (error "Message does not have enough required arguments"))
	  ((< buzzword-number-required-args message-provided-number-of-args)
	   (error "Message defines too many arguments"))
	  (t
	   (pushnew message (buzzword-messages buzzword))))))

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
			:function (eval (make-message-lambda reader
							     '(sheep) 
							     `((property-value sheep ',prop-name)))))))

(defun add-writers-to-sheep (writers prop-name sheep)
  (loop for writer in writers
     do (ensure-buzzword writer)
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
  (multiple-value-bind (name qualifiers lambda-list body)
      (parse-defmessage args)
    (eval-when (:compile-toplevel :load-toplevel :execute)
      `(ensure-message
	',name
	:qualifiers ',qualifiers
	:lambda-list ,(extract-lambda-list lambda-list)
	:participants ,(extract-participants lambda-list)
	:function ,(make-message-lambda name lambda-list body)
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
	    (lambda ,(eval (extract-lambda-list lambda-list))
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

(defun extract-participants (specialized-lambda-list)
  (let ((plist (analyze-lambda-list specialized-lambda-list)))
    `(list ,@(getf plist :participants))))

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

;;; Yoinked from closette...
;;; Several tedious functions for analyzing lambda lists
(defun buzzword-required-arglist (buzzword)
  (let ((plist
          (analyze-lambda-list 
            (buzzword-lambda-list buzzword))))
    (getf plist :required-args)))

(defun message-specialized-portion (message)
  (let ((plist
	 (analyze-lambda-list
	  (message-lambda-list message))))
    (getf plist :required-args)))

(defun extract-lambda-list (specialized-lambda-list)
  (let* ((plist (analyze-lambda-list specialized-lambda-list))
         (requireds (getf plist ':required-names))
         (rv (getf plist ':rest-var))
         (ks (getf plist ':key-args))
         (aok (getf plist ':allow-other-keys))
         (opts (getf plist ':optional-args))
         (auxs (getf plist ':auxiliary-args)))
    `(,@requireds 
      ,@(if rv `(&rest ,rv) ())
      ,@(if (or ks aok) `(&key ,@ks) ())
      ,@(if aok '(&allow-other-keys) ())
      ,@(if opts `(&optional ,@opts) ())
      ,@(if auxs `(&aux ,@auxs) ()))))

(defun analyze-lambda-list (lambda-list)
  (labels ((make-keyword (symbol)
              (intern (symbol-name symbol)
                      (find-package 'keyword)))
           (get-keyword-from-arg (arg)
              (if (listp arg)
                  (if (listp (car arg))
                      (caar arg)
                      (make-keyword (car arg)))
                  (make-keyword arg))))
    (let ((keys ())           ; Just the keywords
          (key-args ())       ; Keywords argument specs
          (required-names ()) ; Just the variable names
          (required-args ())  ; Variable names & participants
          (participants ())   ; Just the participants
          (rest-var nil)
          (optionals ())
          (auxs ())
          (allow-other-keys nil)
          (state :parsing-required))
      (dolist (arg lambda-list)
        (if (member arg lambda-list-keywords)
	    (ecase arg
	      (&optional
	       (setq state :parsing-optional))
	      (&rest
	       (setq state :parsing-rest))
	      (&key
	       (setq state :parsing-key))
	      (&allow-other-keys
	       (setq allow-other-keys t))
	      (&aux
	       (setq state :parsing-aux)))
	    (case state
	      (:parsing-required 
	       (pushend arg required-args)
	       (if (listp arg)
		   (progn (pushend (car arg) required-names)
			  (pushend `(confirm-sheep ,(cadr arg)) participants))
		   (progn (pushend arg required-names)
			  (pushend '=t= participants))))
	      (:parsing-optional (pushend arg optionals))
	      (:parsing-rest (setq rest-var arg))
	      (:parsing-key
	       (pushend (get-keyword-from-arg arg) keys)
	       (pushend arg key-args))
	      (:parsing-aux (pushend arg auxs)))))
      (list  :required-names required-names
             :required-args required-args
             :participants participants
             :rest-var rest-var
             :keywords keys
             :key-args key-args
             :auxiliary-args auxs
             :optional-args optionals
             :allow-other-keys allow-other-keys))))
