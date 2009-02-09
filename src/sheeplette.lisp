;; This file is part of Sheeple

;; sheeplette.lisp
;;
;; An attempt at implementing a version of sheeple with a MOP.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :sheeple)
(defvar *max-sheep-id* 0)
(defparameter secret-unbound-value (gensym))
(defparameter secret-sheep-identifier (gensym))
(define-condition unbound-property (sheeple-error)
  ())

;;;
;;; Slot access
;;;
(defun get-property (sheep property-name)
  (if (eql (sheep-metaobject sheep) =standard-sheep-metaobject=)
      (std-get-property sheep property-name)
      (get-property-using-metaobject (sheep-metaobject sheep) sheep property-name)))
(defun std-get-property (sheep property-name)
  (gethash property-name sheep))

(defun (setf get-property) (new-value sheep property-name)
  (if (eql (sheep-metaobject sheep) =standard-sheep-metaobject=)
      (setf (std-get-property sheep property-name) new-value)
      (setf-get-property-using-metaobject 
       new-value (sheep-metaobject sheep) sheep property-name)))
(defun (setf std-get-property) (new-value sheep property-name)
  (setf (gethash property-name sheep) new-value))

;;;
;;; sheep storage
;;;
(defun std-generate-sheep-instance ()
  "Ex Nihilo creation of a standard sheep instance."
  (let ((table (make-hash-table :test #'equal)))
    (setf (gethash secret-sheep-identifier table)
	  secret-sheep-identifier)
    table))

(defun std-instance-p (sheep)
  (and (hash-table-p sheep)
       (eql (gethash secret-sheep-identifier sheep)
	    secret-sheep-identifier)))

(defun sheep-metaobject (sheep)
  (if (std-instance-p sheep)
      (gethash 'metaobject sheep)
      (sheep-metaobject-with-metaobject sheep)))

;;;
;;; Bootstrap
;;;
(format t "Bootstrapping Sheeplette...")

(defvar =standard-sheep-metaobject=
  (let ((object (std-generate-sheep-instance)))
    (setf (gethash 'metaobject object) object)
    (setf (gethash 'nickname object) nil)
    (setf (gethash 'parents object) nil)
    (setf (gethash 'children object) nil)
    (setf (gethash 'properties object) (make-hash-table :test #'equal))
    (setf (gethash 'property-owners object) (make-hash-table :test #'equal))
    (setf (gethash 'roles object) nil)
    (setf (gethash 'hierarchy-list object) nil)
    object))

(defun generate-sheep-object (&key (metaobject =standard-sheep-metaobject=))
  (if (eql metaobject =standard-sheep-object=)
      (let ((object (make-hash-table :test #'equal)))
	(setf (gethash 'metaobject object) metaobject)
	object)
      (generate-sheep-object-using-metaobject metaobject)))

(defvar =dolly= (generate-sheep-object))

;;;
;;; Cloning
;;;

;;; The Macro
;; Example: (clone (sheep1 sheep2 sheep3) ((property1 value1) (property2 value2)))
(defmacro clone (sheeple properties &rest options)
  "Standard sheep-generation macro"
  `(spawn-sheep
    ,(canonize-sheeple sheeple)
    ,(canonize-properties properties)
    ,@(canonize-options options)))

;;;
;;; Canonizers
;;;
(defun canonize-sheeple (sheeple)
  `(list ,@(mapcar #'canonize-sheep sheeple)))

(defun canonize-sheep (sheep)
  `(confirm-sheep ,sheep))

(defun confirm-sheep (sheep)
  (if (eql (class-of sheep)
	   (find-class 'standard-sheep))
      sheep
      (sheepify sheep)))

(defun canonize-properties (properties)
  `(list ,@(mapcar #'canonize-property properties)))

(defun canonize-property (property)
  (if (symbolp property)
      `(list :name ',property)
      (let ((name (car property))
	    (value (cadr property))
            (readers nil)
            (writers nil)
	    (locked-p nil)
	    (cloneform *secret-unbound-value*)
            (other-options nil))
        (do ((olist (cddr property) (cddr olist)))
            ((null olist))
	  (case (car olist)
	    (:value
	     (setf value (cadr olist)))
	    (:val
	     (setf value (cadr olist)))
            (:reader 
             (pushnew (cadr olist) readers))
            (:writer 
             (pushnew (cadr olist) writers))
            (:manipulator
             (pushnew (cadr olist) readers)
             (pushnew `(setf ,(cadr olist)) writers))
	    (otherwise 
             (pushnew (cadr olist) other-options)
             (pushnew (car olist) other-options))))
	(if other-options
	    (error "Invalid property option(s)")
	    `(list
	      :name ',name
	      :value ,value
	      ,@(when readers `(:readers ',readers))
	      ,@(when writers `(:writers ',writers)))))))

(defun canonize-options (options)
  `(list ,@(mapcar #'canonize-option options)))

(defun canonize-option (option)
  `(list ,@option))

;;;
;;; Create-a-sheep
;;;
(defun spawn-sheep (sheeple properties
		    &rest all-keys
		    &key (metaobject =standard-sheep-metaobject=)
		    &allow-other-keys)
  "Creates a new sheep with SHEEPLE as its parents, and PROPERTIES as its properties"
  (let ((sheep (generate-sheep-object :metaobject metaobject)))
    (set-up-inheritance sheep sheeple)
    (set-up-properties sheep properties)
    (set-up-other-options sheep options)
    (finalize-sheep sheep)
    sheep))

(defun set-up-inheritance (sheep the-parents)
  (if (eql (sheep-metaobject sheep) =standard-sheep-metaobject=)
      (std-set-up-inheritance sheep the-parents)
      (set-up-inheritance-using-metaobject (sheep-metaobject sheep) sheep the-parents)))
(defun std-set-up-inheritance (sheep parents)
  (let ((obj new-sheep))
    (if sheeple
	(loop for sheep in (nreverse sheeple)
	   do (add-parent sheep obj))
	(add-parent =dolly= obj))
    obj))

(defun set-up-properties (sheep properties)
  (loop for property-list in properties
     do (set-up-property property-list sheep)))
(defun set-up-property (sheep property)
  (if (eql (sheep-metaobject sheep) =standard-sheep-metaobject=)
      (std-set-up-property sheep property)
      (set-up-property-using-metaobject 
       (sheep-metaobject sheep) sheep property)))
(defun std-set-up-property (sheep property)
  (defmethod set-up-property (property-list (sheep standard-sheep))
  (let ((name (getf property-list :name))
	(value (getf property-list :value))
	(readers (getf property-list :readers))
	(writers (getf property-list :writers))
	(locked-p (getf property-list :lock)))
    (when (keywordp name)
      (error 'probably-meant-to-be-option))
    (setf (get-property sheep name) value)
    (when locked-p
      (lock-property sheep name))
    (add-readers-to-sheep readers name sheep)
    (add-writers-to-sheep writers name sheep))))

(define-condition invalid-option-error (sheeple-error) ())
(defun set-up-other-options (options sheep)
  (loop for option in options
     do (set-up-option option sheep)))
(defun set-up-option (sheep option)
  (if (eql (sheep-metaobject sheep) =standard-sheep-metaobject=)
      (std-set-up-option sheep option)
      (set-up-option-using-metaobject
       (sheep-metaobject sheep) sheep option)))
(defun std-set-up-option (sheep option)
  (let ((option (car option))
	(value (cadr option)))
    (case option
      (:copy-all-values (when (eql value t)
			  (copy-all-values sheep)))
      (:copy-direct-values (when (eql value t)
			     (copy-direct-parent-values sheep)))
      (:deep-copy (when (eql value t)
		    (copy-all-values sheep)))
      (:nickname (setf (sheep-nickname sheep) value))
      (:lock (when (eql value t)
	       (lock-sheep sheep)))
      (:mitosis (warn "Mitosis successful."))
      (otherwise (error 'invalid-option-error)))))

(defun finalize-sheep (sheep)
  (if (eql (sheep-metaobject sheep) =standard-sheep-metaobject=)
      (std-finalize-sheep sheep)
      (finalize-sheep-using-metaobject (sheep-metaobject sheep) sheep)))
(defun std-finalize-sheep (sheep)
  (memoize-sheep-hierarchy-list sheep)
  (memoize-property-access sheep)
  (loop for child-pointer in (sheep-direct-children sheep)
     do (memoize-property-access (weak-pointer-value child-pointer))))

(define-condition probably-meant-to-be-option (sheeple-error) ())

(defun copy-all-values (sheep)
  (let ((all-property-names (available-properties (if (weak-pointer-p sheep)
						      (weak-pointer-value sheep)
						      sheep))))
    (mapc (lambda (pname)
	    (setf (get-property sheep pname)
		  (get-property sheep pname)))
	  all-property-names)))

(defun copy-direct-parent-values (sheep)
  (mapc (lambda (parent)
	  (maphash 
	   (lambda (key value) 
	     (setf (get-property sheep key) value)) (sheep-direct-properties parent)))
	(sheep-direct-parents sheep)))

(defun add-readers-to-sheep (readers prop-name sheep)
  (loop for reader in readers
     do (ensure-buzzword :name reader)
     do (ensure-message :name reader
			:lambda-list '(sheep)
			:participants (list sheep)
			:body `(get-property sheep ',prop-name)
			:function (eval (make-message-lambda '(sheep) 
							     `((get-property sheep ',prop-name)))))))

(defun add-writers-to-sheep (writers prop-name sheep)
  (loop for writer in writers
     do (ensure-buzzword :name writer)
     do (ensure-message :name writer
			:lambda-list '(new-value sheep)
			:participants (list =dolly= sheep)
			:body `(setf (get-property sheep ',prop-name) new-value)
			:function (eval (make-message-lambda '(new-value sheep) 
							     `((setf (get-property sheep ',prop-name)
								     new-value)))))))

;;;
;;; Buzzword setup
;;;
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

(defun available-messages (sheep)
  (let ((personal-role-names (mapcar (lambda (role) (role-name role))
				     (sheep-direct-roles sheep))))
    (remove-duplicates
     (flatten
      (append personal-role-names (mapcar #'available-messages (sheep-direct-parents sheep)))))))

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

;;;
;;; Bootstrap
;;;
(defvar =standard-buzzword-metaobject= 
  (let ((obj (generate-sheep-object)))
    (setf (get-property obj 'name) '=standard-buzzword-metaobject=)
    (setf (get-property obj 'messages) nil)
    (setf (get-property obj 'documentation) "Standard buzzword metaobject")))
(defvar =standard-message-metaobject=
  (let ((obj (generate-sheep-object)))
    (setf (get-property obj 'name) '=standard-message-metaobject=)
    (setf (get-property obj 'qualifiers) nil)
    (setf (get-property obj 'lambda-list) nil)
    (setf (get-property obj 'participants) nil)
    (setf (get-property obj 'body) '(values))
    (setf (get-property obj 'function) (lambda () (values)))
    (setf (get-property obj 'documentation) "Standard message metaobject")))
(defvar =standard-role-metaobject=
  (let ((obj (generate-sheep-object)))
    (setf (get-property obj 'name) '=standard-role-metaobject=)
    (setf (get-property obj 'position) secret-unbound-value)
    (setf (get-property obj 'message-pointer) secret-unbound-value)))

