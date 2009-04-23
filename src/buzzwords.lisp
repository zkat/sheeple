;; This file is part of Sheeple

;; buzzwords.lisp
;;
;; Buzzword metasheep, buzzword definition and management
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :sheeple)

(defstruct (buzzword (:constructor %make-buzzword))
  (name nil)
  (lambda-list nil)
  (messages nil)
  (documentation ""))

;;;
;;; Buzzword definition
;;;

;;; Buzzword table
(let ((buzzword-table (make-hash-table :test #'equal)))

  (defun find-buzzword (name &optional (errorp t))
    (let ((buzz (gethash name buzzword-table)))
      (if (and (null buzz) errorp)
	  (error 'no-such-buzzword
		 :format-control "There is no buzzword named ~A"
		 :format-args (list name))
	  buzz)))
  
  (defun (setf find-buzzword) (new-value name)
    (setf (gethash name buzzword-table) new-value))
  
  (defun forget-all-buzzwords ()
    (clrhash buzzword-table)
    t)
  
  (defun forget-buzzword (name)
    (remhash name buzzword-table))
) ; end buzzword table closure

(defun finalize-buzzword (buzzword)
  (let ((name (buzzword-name buzzword)))
    (when (and (fboundp name))
     (warn 'clobbering-function-definition
	   :format-control "Clobbering regular function or generic function definition for ~A"
	   :format-args (list name)))
    (setf (fdefinition name) (lambda (&rest args) (apply-buzzword buzzword args)))))

(defun generate-buzzword (&key name
			  lambda-list
			  (documentation ""))
  (let ((buzzword (%make-buzzword 
		   :name name
		   :lambda-list lambda-list
		   :documentation documentation)))
    (finalize-buzzword buzzword)
    buzzword))

(defun ensure-buzzword (name
			&rest all-keys)
  (let ((buzzword (apply #'generate-buzzword
			 :name name
			 all-keys)))
    (setf (find-buzzword name) buzzword)
    buzzword))

(defun undefine-buzzword (name &optional (errorp nil))
  (let ((buzzword (find-buzzword name errorp)))
    (when buzzword
     (loop for message in (buzzword-messages buzzword)
	do (loop for participant in (message-participants message)
	      do (loop for role in (sheep-direct-roles participant)
		    do (when (equal (role-name role) name)
			 (delete-role role participant)))))
     (forget-buzzword name)
     (fmakunbound name)
     buzzword)))

(defun canonize-buzzword-options (options)
  (mapappend #'canonize-buzzword-option options))

(defun canonize-buzzword-option (option)
  (list `',(car option) `',(cadr option)))

(defmacro defbuzzword (name lambda-list &rest options)
  (eval-when (:compile-toplevel :load-toplevel :execute)
   `(ensure-buzzword
     ',name
     :lambda-list ',lambda-list
     ,@(canonize-buzzword-options options))))

(defmacro undefbuzzword (name &optional (errorp t))
  `(undefine-buzzword
    ',name
    ,errorp))
