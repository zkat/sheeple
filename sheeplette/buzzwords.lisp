;; buzzwords.lisp
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :sheeplette)

(defparameter the-standard-buzzword-metasheep-form
  '(clone ()
    ((name
      'buzzword-metasheep
      :cloneform nil)
     (message-metasheep
      =standard-message-metasheep=
      :cloneform =standard-message-metasheep=)
     (role-metasheep
      =standard-role-metasheep=
      :cloneform =standard-role-metasheep=)
     (messages
      nil
      :cloneform nil)
     (documentation
      "standard buzzword"
      :cloneform ""))))

(defun buzzword-name (buzzword)
  (get-property buzzword 'name))
(defun (setf buzzword-name) (new-value buzzword)
  (setf (get-property buzzword 'name) new-value))

(defun buzzword-message-metasheep (buzzword)
  (get-property buzzword 'message-metasheep))
(defun (setf buzzword-message-metasheep) (new-value buzzword)
  (setf (get-property buzzword 'message-metasheep) new-value))

(defun buzzword-role-metasheep (buzzword)
  (get-property buzzword 'role-metasheep))
(defun (setf buzzword-role-metasheep) (new-value buzzword)
  (setf (get-property buzzword 'role-metasheep) new-value))

(defun buzzword-messages (buzzword)
  (get-property buzzword 'messages))
(defun (setf buzzword-messages) (new-value buzzword)
  (setf (get-property buzzword 'messages) new-value))

(defun buzzword-documentation (buzzword)
  (get-property buzzword 'documentation))
(defun (setf buzzword-documentation) (new-value buzzword)
  (setf (get-property buzzword 'documentation) new-value))

;;;
;;; Buzzword definition
;;;

;;; Buzzword table
(define-condition no-such-buzzword (sheeple-error) ())
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
) ; end buzzword table closure

(defun std-finalize-buzzword (buzzword)
  (let ((name (buzzword-name buzzword)))
    (when (fboundp name)
     (warn 'clobbering-function-definition))
    (setf (fdefinition name) (lambda (&rest args) (apply-buzzword name args)))))

(define-condition clobbering-function-definition (warning) ())
(defun generate-sheep-standard-buzzword (metasheep 
					 &key name
					 message-metasheep
					 (documentation "") 
					 &allow-other-keys)
  (let ((buzzword (clone (metasheep) 
			 ((name name)
			  (message-metasheep
			   message-metasheep)
			  (documentation documentation)))))
    (std-finalize-buzzword buzzword)
    buzzword))

(defun ensure-buzzword (name
			&rest all-keys 
			&key (buzzword-metasheep =standard-buzzword-metasheep=)
			(message-metasheep =standard-message-metasheep=)
			(role-metasheep =standard-role-metasheep=)
			&allow-other-keys)
  (if (find-buzzword name nil)
      (find-buzzword name)
      (let ((buzzword (apply (if (eql buzzword-metasheep =standard-buzzword-metasheep=)
				 #'generate-sheep-standard-buzzword
				 #'generate-sheep)
			     buzzword-metasheep
			     :name name
			     :message-metasheep message-metasheep
			     :role-metasheep role-metasheep
			     all-keys)))
	(setf (find-buzzword name) buzzword)
	buzzword)))

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

(defun canonize-buzzword-options (options)
  (mapappend #'canonize-buzzword-option options))

(defun canonize-buzzword-option (option)
  (list `',(car option) `',(cadr option)))

(defmacro defbuzzword (name &rest options)
  `(ensure-buzzword
    :name ',name
    ,@(canonize-buzzword-options options)))

(defmacro undefbuzzword (name)
  `(undefine-buzzword
    :name ',name))

