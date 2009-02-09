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
  (if (eql (sheep-metasheep sheep) =standard-sheep-metasheep=)
      (std-get-property sheep property-name)
      (get-property-using-metasheep (sheep-metasheep sheep) sheep property-name)))
(defun std-get-property (sheep property-name)
  (get-property-with-memoized-owner sheep property-name))

(defun get-property-with-memoized-owner (sheep property-name)
  (multiple-value-bind (prop-owner has-p)
      (gethash property-name (sheep-property-owners sheep))
    (if has-p
	(multiple-value-bind (value has-p)
	    (gethash property-name (sheep-direct-properties prop-owner))
	  (if has-p
	      value
	      (error 'unbound-property)))
	(error 'unbound-property))))

(defun (setf get-property) (new-value sheep property-name)
  (if (eql (sheep-metasheep sheep) =standard-sheep-metasheep=)
      (setf (std-get-property sheep property-name) new-value)
      (setf-get-property-using-metasheep 
       new-value (sheep-metasheep sheep) sheep property-name)))
(defun (setf std-get-property) (new-value sheep property-name)
  (setf (gethash property-name sheep) new-value))

(defun remove-property (sheep property-name)
  (if (eql (sheep-metasheep sheep) =standard-sheep-metasheep=)
      (std-remove-property sheep property-name)
      (remove-property-using-metasheep (sheep-metasheep sheep) sheep property-name)))
(defun std-remove-property (sheep property-name)
  (remhash property-name (sheep-direct-propertes sheep)))

(defun has-property-p (sheep property-name)
  "Returns T if a property with PROPERTY-NAME is available to SHEEP."
  (handler-case
      (when (get-property sheep property-name)
	t)
    (unbound-property () nil)))

(defun has-direct-property-p (sheep property-name)
  (if (eql (sheep-metasheep sheep) =standard-sheep-metasheep=)
      (std-has-direct-property-p sheep property-name)
      (has-direct-property-p-using-metasheep 
       (sheep-metasheep sheep) sheep property-name)))
(defun std-has-direct-property-p (sheep property-name)
  (multiple-value-bind (value has-p)
      (gethash property-name (sheep-direct-properties sheep))
    value
    has-p))

(defun who-sets (sheep property-name)
  (if (eql (sheep-metasheep sheep) =standard-sheep-metasheep=)
      (std-who-sets sheep property-name)
      (who-sets-using-metasheep (sheep-metasheep sheep) sheep property-name)))
(defun std-who-sets (sheep property-name)
  (multiple-value-bind (owner has-p)
      (gethash property-name (sheep-property-owners sheep))
    (if has-p
	owner
	(error 'unbound-property))))

(defun available-properties (sheep)
  (if (eql (sheep-metaobjct sheep) =standard-sheep-metasheep=)
      (std-available-properties sheep)
      (available-properties-using-metasheep (sheep-metasheep sheep) sheep)))
(defun std-available-properties (sheep)
  (let ((obj-keys (loop for keys being the hash-keys of (sheep-direct-properties sheep)
		     collect keys)))
    (remove-duplicates
     (flatten
      (append obj-keys (mapcar #'available-properties (sheep-direct-parents sheep)))))))

;;; Memoization
(defun memoize-property-access (sheep)
  (loop for property in (available-properties sheep)
     do (let ((owner (%get-property-owner sheep property)))
	  (setf (gethash property (sheep-property-owners sheep))
		owner))))

(defun %get-property-owner (sheep property-name)
  (let ((hierarchy-list (sheep-hierarchy-list sheep)))
    (loop for sheep in hierarchy-list
       do (multiple-value-bind (value has-p)
	      (%get-property-object sheep property-name)
	    (declare (ignore value))
	    (when has-p
	      (return-from %get-property-owner sheep)))
       finally (error 'unbound-property))))

(defun memoize-sheep-hierarchy-list (sheep)
  (let ((list (compute-sheep-hierarchy-list sheep)))
    (setf (sheep-hierarchy-list sheep)
	  list)
    (mapc (lambda (descendant) 
	    (memoize-sheep-hierarchy-list (weak-pointer-value descendant)))
	  (sheep-direct-children sheep))))

;;;
;;; Hierarchy Resolution
;;; - blatantly taken from Closette.
;;;
(defun collect-parents (sheep)
  (labels ((all-parents-loop (seen parents)
	     (let ((to-be-processed
		    (set-difference parents seen)))
	       (if (null to-be-processed)
		   parents
		   (let ((sheep-to-process
			  (car to-be-processed)))
		     (all-parents-loop
		      (cons sheep-to-process seen)
		      (union (sheep-direct-parents sheep-to-process)
			     parents)))))))
    (all-parents-loop () (list sheep))))

(defun compute-sheep-hierarchy-list (sheep)
  (handler-case 
      (let ((sheeple-to-order (collect-parents sheep)))
  	(topological-sort sheeple-to-order
			  (remove-duplicates
			   (mapappend #'local-precedence-ordering
				      sheeple-to-order))
			  #'std-tie-breaker-rule))
    (simple-error ()
      (error 'sheep-hierarchy-error))))

(define-condition sheep-hierarchy-error (sheeple-error) ()
  (:documentation "Signaled whenever there is a problem computing the hierarchy list."))

(defun local-precedence-ordering (sheep)
  (mapcar #'list
	  (cons sheep
		(butlast (sheep-direct-parents sheep)))
	  (sheep-direct-parents sheep)))

(defun std-tie-breaker-rule (minimal-elements cpl-so-far)
  (dolist (cpl-constituent (reverse cpl-so-far))
    (let* ((supers (sheep-direct-parents cpl-constituent))
           (common (intersection minimal-elements supers)))
      (when (not (null common))
        (return-from std-tie-breaker-rule (car common))))))

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

(defun sheep-p (sheep)
  (when (std-instance-p sheep)
    t))

;;;
;;; Bootstrap
;;;
(defvar =standard-sheep-metasheep=
  (let ((object (std-generate-sheep-instance)))
    (setf (gethash 'metasheep object) object)
    (setf (gethash 'nickname object) nil)
    (setf (gethash 'parents object) nil)
    (setf (gethash 'children object) nil)
    (setf (gethash 'properties object) (make-hash-table :test #'equal))
    (setf (gethash 'property-owners object) (make-hash-table :test #'equal))
    (setf (gethash 'roles object) nil)
    (setf (gethash 'hierarchy-list object) nil)
    object))

;; NOTE: the setf for this should really reinitialize the sheep
(defun sheep-metasheep (sheep)
  (get-property sheep 'metasheep))
(defun (setf sheep-metasheep) (new-mo sheep)
  (error "Changing metasheeps is not supported right now"))

(defun sheep-nickname (sheep)
  (get-property sheep 'nickname))
(defun (setf sheep-nickname) (new-value sheep)
  (setf (get-property sheep 'nickname) new-value))

(defun sheep-direct-parents (sheep)
  (get-property sheep 'parents))
(defun (setf sheep-direct-parents) (new-value sheep)
  (setf (get-property sheep 'parents) new-value))

(defun sheep-direct-children (sheep)
  (get-property sheep 'children))
(defun (setf sheep-direct-children) (new-value sheep)
  (setf (get-property sheep 'children) new-value))

(defun sheep-direct-properties (sheep)
  (get-property sheep 'properties))
(defun (setf sheep-direct-properties) (new-value sheep)
  (setf (get-property sheep 'properties) new-value))

(defun sheep-property-owners (sheep)
  (get-property sheep 'property-owners))
(defun (setf sheep-property-owners) (new-value sheep)
  (setf (get-property sheep 'property-owners) new-value))

(defun sheep-direct-roles (sheep)
  (get-property sheep 'roles))
(defun (setf sheep-direct-roles) (new-value sheep)
  (setf (get-property sheep 'roles) new-value))

(defun sheep-hierarchy-list (sheep)
  (get-property sheep 'hierarchy-list))
(defun (setf sheep-hierarchy-list) (new-value sheep)
  (setf (get-property sheep 'hierarchy-list) new-value))

(defun std-generate-sheep (metasheep &key parents options &allow-other-keys)
  (declare (ignore metasheep))
  (let ((sheep (make-hash-table :test #'equal)))
    ;; First we actually set up all the slots
    (setf (gethash sheep 'metasheep) =standard-sheep-metasheep=) ; (setf sheep-metasheep) is
    (setf (sheep-nickname sheep) nil)	                            ; unsupported right now
    (setf (sheep-direct-parents sheep) nil)
    (setf (sheep-direct-children sheep) nil)
    (setf (sheep-direct-properties sheep) (make-hash-table :test #'equal))
    (setf (sheep-property-owners sheep) (make-weak-hash-table :weakness :value :test #'equal))
    (setf (sheep-direct-roles sheep) nil)
    (setf (sheep-hierarchy-list) nil)
    ;; Then we deal with the options
    (set-up-inheritance sheep sheeple)
    (set-up-properties sheep properties)
    (set-up-other-options sheep options)
    sheep))

(defvar =dolly= 
  (let ((obj (std-generate-sheep =standard-sheep-metasheep=
					:parents nil
					:options nil)))
    (setf (get-property obj 'nickname) "=dolly=")
    obj))


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
		    &key (metasheep =standard-sheep-metasheep=)
		    &allow-other-keys)
  "Creates a new sheep with SHEEPLE as its parents, and PROPERTIES as its properties"
  (let ((sheep (apply (if (eql metasheep =standard-sheep-metasheep=)
			  #'std-generate-sheep
			  #'generate-sheep
			  metasheep sheeple properties all-keys))))
    (if (eql metasheep =standard-sheep-metasheep=)
	(std-finalize-sheep sheep)
	(finalize-sheep sheep))
    sheep))

(defun set-up-inheritance (sheep the-parents)
  (if (eql (sheep-metasheep sheep) =standard-sheep-metasheep=)
      (std-set-up-inheritance sheep the-parents)
      (set-up-inheritance-using-metasheep (sheep-metasheep sheep) sheep the-parents)))
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
  (if (eql (sheep-metasheep sheep) =standard-sheep-metasheep=)
      (std-set-up-property sheep property)
      (set-up-property-using-metasheep 
       (sheep-metasheep sheep) sheep property)))
(defun std-set-up-property (sheep property)
  (defmethod set-up-property (property-list (sheep standard-sheep))
  (let ((name (getf property-list :name))
	(value (getf property-list :value))
	(readers (getf property-list :readers))
	(writers (getf property-list :writers)))
    (when (keywordp name)
      (error 'probably-meant-to-be-option))
    (setf (get-property sheep name) value)
    (add-readers-to-sheep readers name sheep)
    (add-writers-to-sheep writers name sheep))))

(define-condition invalid-option-error (sheeple-error) ())
(defun set-up-other-options (options sheep)
  (loop for option in options
     do (set-up-option option sheep)))
(defun set-up-option (sheep option)
  (if (eql (sheep-metasheep sheep) =standard-sheep-metasheep=)
      (std-set-up-option sheep option)
      (set-up-option-using-metasheep
       (sheep-metasheep sheep) sheep option)))
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
;;; Inheritance setup
;;;
(defun add-parent (new-parent child)
  (cond ((equal new-parent child)
	 (error "Can't inherit from self."))
	((not (equal (class-of new-parent)
		     (class-of child)))
	 (error "Wrong metaclass for parent"))
	(t
	 (handler-case
	     (progn
	       (pushnew new-parent (sheep-direct-parents child))
	       (let ((pointer (make-weak-pointer child)))
		 (pushnew pointer (sheep-direct-children new-parent))
		 (finalize child (lambda () (setf (sheep-direct-children new-parent)
						  (delete pointer 
							  (sheep-direct-children new-parent))))))
	       child)
	   (sheep-hierarchy-error ()
	     (progn
	       (setf (sheep-direct-parents child) (delete new-parent (sheep-direct-parents child)))
	       (error 'sheep-hierarchy-error))))))
  (finalize-sheep child))

(defun remove-parent (parent child)
  (setf (sheep-direct-parents child)
	(delete parent (sheep-direct-parents child)))
  (setf (sheep-direct-children parent)
	(delete child (sheep-direct-children parent) :key #'weak-pointer-value))
  (when keep-properties
    (with-accessors ((parent-properties sheep-direct-properties))
	parent
      (loop for property-name being the hash-keys of parent-properties
	 using (hash-value value)
	 do (unless (has-direct-property-p child property-name)
	      (setf (get-property child property-name) value)))))
  (finalize-sheep child)
  child)

;;; Inheritance predicates
(defun direct-parent-p (maybe-parent child)
  (when (member maybe-parent (sheep-direct-parents child))
    t))

(defun ancestor-p (maybe-ancestor descendant)
  (when (and (not (equal maybe-ancestor descendant))
	     (member maybe-ancestor (collect-parents descendant)))
    t))

(defun direct-child-p (maybe-child parent)
  (direct-parent-p parent maybe-child))

(defun descendant-p (maybe-descendant ancestor)
  (ancestor-p ancestor maybe-descendant))

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

