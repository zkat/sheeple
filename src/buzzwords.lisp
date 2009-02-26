;; This file is part of Sheeple

;; buzzwords.lisp
;;
;; Buzzword metasheep, buzzword definition and management
;;
;; TODO:
;; * Screw this variable-length LL thing. Turn defbuzzword into a defgeneric clone.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :sheeple)

(defparameter the-standard-buzzword-metasheep-form
  '(clone ()
    ((name
      'buzzword-metasheep
      :cloneform nil)
     (lambda-list
      nil
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
  (property-value buzzword 'name))
(defun (setf buzzword-name) (new-value buzzword)
  (setf (property-value buzzword 'name) new-value))

(defun buzzword-message-metasheep (buzzword)
  (property-value buzzword 'message-metasheep))
(defun (setf buzzword-message-metasheep) (new-value buzzword)
  (setf (property-value buzzword 'message-metasheep) new-value))

(defun buzzword-role-metasheep (buzzword)
  (property-value buzzword 'role-metasheep))
(defun (setf buzzword-role-metasheep) (new-value buzzword)
  (setf (property-value buzzword 'role-metasheep) new-value))

(defun buzzword-messages (buzzword)
  (property-value buzzword 'messages))
(defun (setf buzzword-messages) (new-value buzzword)
  (setf (property-value buzzword 'messages) new-value))

(defun buzzword-documentation (buzzword)
  (property-value buzzword 'documentation))
(defun (setf buzzword-documentation) (new-value buzzword)
  (setf (property-value buzzword 'documentation) new-value))

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
  (let ((name (buzzword-name buzzword))
	(fn (std-make-buzzword-lambda buzzword)))
    (when (fboundp name)
     (warn 'clobbering-function-definition))
    (setf (fdefinition name) fn)))

;; TODO - this is gonna be pretty tricky ;|
(defun std-make-buzzword-lambda (buzzword)
  (let* ((lambda-list (buzzword-lambda-list buzzword))
	 (requireds (extract-requireds lambda-list))
	 (others (extract-other-args lambda-list)))
    (eval `(lambda ,lambda-list (apply-buzzword buzzword requireds others)))))

(defun extract-arguments (lambda-list)
  ;...
  )

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
			&key 
			(buzzword-metasheep =standard-buzzword-metasheep=)
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

(defun undefine-buzzword (name)
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
    ',name
    ,@(canonize-buzzword-options options)))

(defmacro undefbuzzword (name)
  `(undefine-buzzword
    ',name))

;;; Yoinked from closette...
;;; Several tedious functions for analyzing lambda lists

(defun required-portion (gf args)
  (let ((number-required (length (gf-required-arglist gf))))
    (when (< (length args) number-required)
      (error "Too few arguments to generic function ~S." gf))
    (subseq args 0 number-required)))

(defun gf-required-arglist (gf)
  (let ((plist
          (analyze-lambda-list 
            (generic-function-lambda-list gf))))
    (getf plist ':required-args)))

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

(defun extract-specializers (specialized-lambda-list)
  (let ((plist (analyze-lambda-list specialized-lambda-list)))
    (getf plist ':specializers)))

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
          (required-args ())  ; Variable names & specializers
          (specializers ())   ; Just the specializers
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
	       (setq allow-other-keys 't))
	      (&aux
	       (setq state :parsing-aux)))
	    (case state
	      (:parsing-required 
	       (push-on-end arg required-args)
	       (if (listp arg)
		   (progn (push-on-end (car arg) required-names)
			  (push-on-end (cadr arg) specializers))
		   (progn (push-on-end arg required-names)
			  (push-on-end 't specializers))))
	      (:parsing-optional (push-on-end arg optionals))
	      (:parsing-rest (setq rest-var arg))
	      (:parsing-key
	       (push-on-end (get-keyword-from-arg arg) keys)
	       (push-on-end arg key-args))
	      (:parsing-aux (push-on-end arg auxs)))))
      (list  :required-names required-names
             :required-args required-args
             :specializers specializers
             :rest-var rest-var
             :keywords keys
             :key-args key-args
             :auxiliary-args auxs
             :optional-args optionals
             :allow-other-keys allow-other-keys))))
