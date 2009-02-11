;; This file is part of Sheeple

;; sheeplette.lisp
;;
;; An attempt at implementing a version of sheeple with a MOP.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :sheeplette)

(defparameter *secret-unbound-value* (gensym))
(defparameter *secret-sheep-identifier* (gensym))
(define-condition unbound-property (sheeple-error) ())

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
  (setf (gethash property-name (sheep-direct-properties sheep)) new-value))

(defun get-cloneform (sheep property-name)
  (if (eql (sheep-metasheep sheep) =standard-sheep-metasheep=)
      (std-get-cloneform sheep property-name)
      (get-cloneform-using-metasheep (sheep-metasheep sheep) sheep property-name)))
(defun std-get-cloneform (the-sheep property-name)
  (loop for sheep in (sheep-hierarchy-list sheep)
       do (let ((cloneform-table (sheep-direct-cloneforms sheep)))
	    (multiple-value-bind (value has-p)
		(gethash property-name cloneform-table)
	      (when has-p
		(return-from std-get-cloneform value))))
       finally *secret-unbound-value*))
(defun (setf get-cloneform) (new-value sheep property-name)
  (if (eql (sheep-metasheep sheep) =standard-sheep-metasheep=)
      (setf (std-get-cloneform sheep property-name) new-value)
      (setf-get-cloneform-using-metasheep
       new-value (sheep-metasheep sheep) sheep property-name)))
(defun (setf std-get-cloneform) (new-value sheep property-name)
  (setf (gethash property-name (sheep-direct-cloneforms sheep)) new-value))

(defun get-clonefunction (sheep property-name)
  (if (eql (sheep-metasheep sheep) =standard-sheep-metasheep=)
      (std-get-clonefunction sheep property-name)
      (get-clonefunction-using-metasheep (sheep-metasheep sheep) sheep property-name)))
(defun std-get-clonefunction (the-sheep property-name)
  (loop for sheep in (sheep-hierarchy-list sheep)
       do (let ((clonefunction-table (sheep-direct-clonefunctions sheep)))
	    (multiple-value-bind (value has-p)
		(gethash property-name clonefunctions-table)
	      (when has-p
		(return-from std-get-clonefunctions value))))
       finally *secret-unbound-value*))
(defun (setf get-clonefunction) (new-value sheep property-name)
  (if (eql (sheep-metasheep sheep) =standard-sheep-metasheep=)
      (setf (std-get-clonefunction sheep property-name) new-value)
      (setf-get-clonefunction-using-metasheep
       new-value (sheep-metasheep sheep) sheep property-name)))
(defun (setf std-get-clonefunction) (new-value sheep property-name)
  (setf (gethash property-name (sheep-direct-clonefunctions sheep)) new-value))

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
  (if (eql (sheep-metaobject sheep) =standard-sheep-metasheep=)
      (std-available-properties sheep)
      (available-properties-using-metasheep (sheep-metasheep sheep) sheep)))
(defun std-available-properties (sheep)
  (let ((obj-keys (loop for keys being the hash-keys of (sheep-direct-properties sheep)
		     collect keys)))
    (remove-duplicates
     (flatten
      (append obj-keys (mapcar #'available-properties (sheep-direct-parents sheep)))))))

(defun available-cloneforms (sheep)
  (if (eql (sheep-metaobject sheep) =standard-sheep-metasheep=)
      (std-available-cloneforms sheep)
      (available-cloneforms-using-metasheep (sheep-metasheep sheep) sheep)))
(defun std-available-cloneforms (sheep)
  (let ((obj-keys (loop for keys being the hash-keys of (sheep-direct-cloneforms sheep)
		     collect keys)))
    (remove-duplicates
     (flatten
      (append obj-keys (mapcar #'available-cloneforms (sheep-direct-parents sheep)))))))

;;; Memoization
(defun memoize-property-access (sheep)
  (loop for property in (available-properties sheep)
     do (let ((owner (%get-property-owner sheep property)))
	  (setf (gethash property (sheep-property-owners sheep))
		owner))))

(defun %get-property-owner (sheep property-name)
  (let ((hierarchy-list (sheep-hierarchy-list sheep)))
    (loop for sheep-obj in hierarchy-list
       do (when (handler-case
		    (get-property sheep-obj property-name)
		  (unbound-property () nil))
	    (return-from %get-property-owner
	      sheep-obj))
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

(defparameter the-standard-sheep-metasheep-form
  '(clone ()
    ((metasheep
      =standard-sheep-metasheep=
      :cloneform =standard-sheep-metasheep=)
     (nickname
      "=standard-sheep-metasheep="
      :cloneform nil)
     (parents
      nil
      :cloneform nil)
     (children
      nil
      :cloneform nil)
     (properties
      (make-hash-table :test #'equal)
      :cloneform (make-hash-table :test #'equal))
     (property-owners
      (make-weak-hash-table :weakness :value :test #'equal)
      :cloneform (make-weak-hash-table :test #'equal))
     (roles
      nil
      :cloneform nil)
     (hierarchy-list
      nil
      :cloneform nil))))

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

(defun sheep-direct-cloneforms (sheep)
  (get-property sheep 'cloneforms))
(defun (setf sheep-direct-cloneforms) (new-valuesheep)
  (setf (get-property sheep 'cloneforms) new-value))

(defun sheep-direct-clonefunctions (sheep)
  (get-property sheep 'clonefunctions))
(defun (setf sheep-direct-clonefunctions) (new-value sheep)
  (setf (get-property sheep 'clonefunctions) new-value))

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

;;;
;;; Cloning
;;;

;;; sheep storage
(defun std-generate-sheep-instance ((metasheep =standard-sheep-metasheep=))
  "Ex Nihilo creation of a standard sheep instance."
  (let ((table (make-hash-table :test #'equal)))
    (setf (gethash *secret-sheep-identifier* table)
	  *secret-sheep-identifier*)
    (setf (gethash 'metasheep table) metasheep)
    table))

(defun std-sheep-object-p (sheep)
  (and (hash-table-p sheep)
       (eql (gethash *secret-sheep-identifier* sheep)
	    *secret-sheep-identifier*)))

(defun sheep-p (sheep)
  (if (and (std-sheep-object-p)
	   (eql (sheep-metasheep sheep) =standard-sheep-metasheep))
      t
      (sheep-p-using-sheep sheep)))

(defun generate-sheep-std-metasheep (metasheep &key parents options &allow-other-keys)
  (declare (ignore metasheep))
  (let ((sheep (std-generate-sheep-instance metasheep)))
    ;; First we actually set up all the properties
    ;; The canonical required properties...
    (setf (sheep-direct-parents sheep) nil)
    (setf (sheep-direct-properties sheep) (make-hash-table :test #'equal))
    (setf (sheep-direct-roles sheep) nil)
    (setf (sheep-direct-cloneforms sheep) nil)
    (setf (sheep-direct-clonefunctions sheep) nil)
    ;; Additional =standard-sheep-metasheep= properties
    (setf (sheep-nickname sheep) nil)
    (setf (sheep-direct-children sheep) nil)
    (setf (sheep-property-owners sheep) (make-weak-hash-table :weakness :value :test #'equal))
    (setf (sheep-hierarchy-list) nil)
    ;; Then we deal with the options
    (std-add-parents sheep parents)
    (std-execute-clonefunctions sheep)
    (std-set-up-properties sheep properties)
    (std-set-up-options sheep options)
    (std-finalize-sheep sheep)
    sheep))

(defun spawn-sheep (sheeple properties
		    &rest all-keys
		    &key (metasheep =standard-sheep-metasheep=)
		    &allow-other-keys)
  "Creates a new sheep with SHEEPLE as its parents, and PROPERTIES as its properties"
  (let ((sheep (apply (if (eql metasheep =standard-sheep-metasheep=)
			  #'generate-sheep-std-metasheep
			  #'generate-sheep
			  metasheep sheeple properties all-keys))))
    sheep))

(defun std-add-parents (sheep parents)
  (let ((real-parents (or parents
			  (list =dolly=))))
    (setf (sheep-direct-parents sheep) real-parents)
    (memoize-sheep-hierarchy-list sheep)
    sheep))

(defun set-up-properties (sheep properties)
  (loop for property-list in properties
     do (set-up-property property-list sheep)))
(defun std-set-up-property (sheep property)
  (defmethod set-up-property (property-list (sheep standard-sheep))
  (let ((name (getf property-list :name))
	(value (getf property-list :value))
	(readers (getf property-list :readers))
	(writers (getf property-list :writers)))
    (when (not (symbolp name))
      (error "Property names must be symbols"))
    (setf (get-property sheep name) value)
    (add-readers-to-sheep readers name sheep)
    (add-writers-to-sheep writers name sheep))))


(defun execute-clonefunctions (sheep)
  (if (eql (sheep-metasheep sheep) =standard-sheep-metasheep=)
      (std-execute-clonefunctions sheep)
      (execute-clonefunctions-using-metasheep 
       (sheep-metasheep sheep) sheep)))
(defun std-execute-clonefunctions (sheep)
  (let ((functions (loop for propname in (available-cloneforms the-sheep)
		      collect (get-clonefunction sheep propname))))
    (loop for fn in functions
	 (unless (eql fn *secret-unbound-value*)
	   (setf (get-property sheep propname) (funcall fn))))))

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

;;; Inheritance setup
(defun add-parent (new-parent child)
  (if (and (eql =standard-sheep-metaobject= (sheep-metaobject child))
	   (eql =standard-sheep-metaobject= (sheep-metaobject new-parent)))
      (std-add-parent new-parent child)
      (add-parent-using-metaobjects 
       (sheep-metaobject new-parent) (sheep-metaobject child)
       new-parent child)))
(defun std-add-parent (new-parent child)
  (cond ((equal new-parent child)
	 (error "Can't inherit from self."))
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
	       (error 'sheep-hierarchy-error))))
	 (std-finalize-sheep child)
	 child)))

(defun remove-parent (parent child)
  (if (and (eql =standard-sheep-metaobject= (sheep-metaobject child))
	   (eql =standard-sheep-metaobject= (sheep-metaobject new-parent)))
      (std-remove-parent parent child)
      (remove-parent-using-metaobjects
       (sheep-metaobject parent) (sheep-metaobject child)
       parent child)))
(defun std-remove-parent (parent child)
  (setf (sheep-direct-parents child)
	(delete parent (sheep-direct-parents child)))
  (setf (sheep-direct-children parent)
	(delete child (sheep-direct-children parent) :key #'weak-pointer-value))
  (when keep-properties
    (loop for property-name being the hash-keys of (sheep-direct-properties parent)
       using (hash-value value)
       do (unless (has-direct-property-p child property-name)
	    (setf (get-property child property-name) value))))
  (finalize-sheep child)
  child)

;;;
;;; Hierarchy Resolution
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
;;; Buzzwords
;;;
(defparameter the-standard-buzzword-metaobject-form
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
  (get-property buzzword name))
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

(defun available-messages (sheep)
  (if (eql =standard-sheep-metaobject= (sheep-metaobject sheep))
      (std-available-messages sheep)
      (available-messages-using-metaobject (sheep-metaobject sheep))))

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
  (let (buzzword (clone (metasheep) 
		   ((name name)
		    (message-metasheep
		     metasheep)
		    (documentation documentation))))
    (std-finalize-buzzword buzzword)
    buzzword))

(defun ensure-buzzword (name
			&rest all-keys 
			&key (buzzword-metasheep =standard-buzzword-metasheep=)
			&key (message-metasheep =standard-message-metasheep=)
			&key (role-metasheep =standard-role-metasheep=)
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

(defmacro defbuzzword (name &rest options)
  `(ensure-buzzword
    :name ',name
    ,@(canonize-options options)))

(defmacro undefbuzzword (name)
  `(undefine-buzzword
    :name ',name))

;;;
;;; Messages
;;;
(defparameter the-standard-message-metaobject-form
  '(clone ()
    ((name
      'standard-message-metaobject
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
      "standard message metaobject"
      :cloneform ""))))

(defparameter the-standard-role-metaobject-form
  '(clone ()
    ((name
      'standard-role-metaobject
      :cloneform 'standard-role)
     (position
      0
      :cloneform nil)
     (message-pointer
      nil
      :cloneform nil))))

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

(defun generate-sheep-standard-message (metasheep
					&key name
					buzzword
					qualifiers
					lambda-list
					participants
					function
					body
					(documentation ""))
  (let ((message (clone (=standard-message-metasheep=)
			((name name)
			 (buzzword buzzword)
			 (qualifiers qualifiers)
			 (lambda-list lambda-list)
			 (participants participants)
			 (function function)
			 (body body)
			 (documentation documentation)))))
    (add-message-to-buzzword message buzzword)
    (remove-messages-with-name-qualifiers-and-participants name qualifiers target-sheeple)
    (add-message-to-sheeple name message target-sheeple)))

(defun ensure-message (name &rest all-keys)
  (when (not (find-buzzword name nil))
    (progn
      (warn 'style-warning)
      (ensure-buzzword
       :name name)))
  (let* ((buzzword (find-buzzword name))
	 (target-sheeple (sheepify-list participants))
	 (message (apply
		   (if (eql (sheep-metobject buzzword) =standard-buzzword-metasheep=)
		       #'generate-sheep-standard-message
		       #'generate-sheep
		       (buzzword-message-metasheep buzzword)
		       :name name
		       :buzzword buzzword
		       all-keys))))
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

(defun add-message-to-sheeple (name message sheeple)
  (let ((role-metasheep (buzzword-role-metasheep (find-buzzword name))))
    (loop 
       for sheep in sheeple
       for i upto (1- (length sheeple))
       do (push (clone (role-metasheep)
		       (name name)
		       (position i)
		       (message-pointer message))
		(sheep-direct-roles sheep)))))

(defun undefine-message (&key name qualifiers participants)
  (remove-messages-with-name-qualifiers-and-participants name qualifiers participants))

;; TODO
;(defun find-message (selector qualifiers participants &optional (errorp t)))

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
	  ,@body)) args)))

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

;;;
;;; Message dispatch
;;;

(defun primary-message-p (message)
  (null (message-qualifiers message)))

(defun before-message-p (message)
  (when (member :before (message-qualifiers message))
    t))

(defun after-message-p (message)
  (when (member :after (message-qualifiers message))
    t))

(defun around-message-p (message)
  (when (member :around (message-qualifiers message))
    t))

(defun apply-buzzword (selector args)
  (let ((messages (find-applicable-messages selector
					    (sheepify-list args))))
    (apply-messages messages args)))

(defun apply-messages (messages args)
  (let ((around (find-if #'around-message-p messages))
	(primaries (remove-if-not #'primary-message-p messages)))
	  (when (null primaries)
	    (error "No primary messages"))
    (if around
	(apply-message around args (remove around messages))
    	(let ((befores (remove-if-not #'before-message-p messages))
	      (afters (remove-if-not #'after-message-p messages)))
	  (dolist (before befores)
	    (apply-message before args nil))
	  (multiple-value-prog1
	      (apply-message (car primaries) args (cdr primaries))
	    (dolist (after (reverse afters))
	      (apply-message after args nil)))))))

(defun apply-message (message args next-messages)
  (let ((function (message-function message)))
    (funcall function args next-messages)))

(defun find-applicable-messages  (selector args &key (errorp t))
  "Returns the most specific message using SELECTOR and ARGS."
  (let ((n (length args))
	(discovered-messages nil)
	(contained-applicable-messages nil))
    (loop 
       for arg in args
       for index upto (1- n)
       do (let ((curr-sheep-list (sheep-hierarchy-list arg)))
	    (loop
	       for curr-sheep in curr-sheep-list
	       for hierarchy-position upto (1- (length curr-sheep-list))
	       do (dolist (role (sheep-direct-roles curr-sheep))
		    (when (and (equal selector (role-name role))
			       (eql index (role-position role)))
			  (let ((curr-message (message-pointer role)))
			    (when (= n (length (message-lambda-list curr-message)))
			      (when (not (member curr-message
						 discovered-messages
						 :key #'message-container-message))
				(pushnew (contain-message curr-message) discovered-messages))
			      (let ((contained-message (find curr-message
							     discovered-messages
							     :key #'message-container-message)))
				(setf (elt (message-container-rank contained-message) index) 
				      hierarchy-position)
				(when (fully-specified-p (message-container-rank contained-message))
				  (pushnew contained-message contained-applicable-messages :test #'equalp))))))))))
    (if contained-applicable-messages
	(unbox-messages (sort-applicable-messages contained-applicable-messages))
	(when errorp
	  (error 'no-applicable-messages)))))

(defun unbox-messages (messages)
  (mapcar #'message-container-message messages))

(defun sort-applicable-messages (message-list &key (rank-key #'<))
  (sort message-list rank-key
	:key (lambda (contained-message)
	       (calculate-rank-score (message-container-rank contained-message)))))

(defun contain-message (message)
  (make-message-container
   :message message
   :rank (make-array (length (message-lambda-list message))
		     :initial-element nil)))

(defstruct message-container
  message
  rank)

(define-condition no-applicable-messages (sheeple-error) ())
(define-condition no-most-specific-message (sheeple-error) ())

(defun fully-specified-p (rank)
  (loop for item across rank
     do (when (eql item nil)
	  (return-from fully-specified-p nil)))
  t)

(defun calculate-rank-score (rank)
  (let ((total 0))
    (loop for item across rank
       do (when (numberp item)
	    (incf total item)))
    total))

;;;
;;; Bootstrap
;;;

;;; These two help set up all the initial cloneform/clonefunction pairs.
(defun set-up-standard-sheep-metasheep-cloneforms (cloneform-table)
  (setf (gethash 'metasheep cloneform-table) '=standard-sheep-metasheep=)
  (setf (gethash 'nickname cloneform-table) 'nil)
  (setf (gethash 'parents cloneform-table) 'nil)
  (setf (gethash 'children cloneform-table) 'nil)
  (setf (gethash 'properties cloneform-table) '(make-hash-table :test #'equal))
  (setf (gethash 'property-owners cloneform-table) '(make-weak-hash-table :weakness :value :test #'equal))
  (setf (gethash 'roles cloneform-table) 'nil)
  (setf (gethash 'hierarchy-list cloneform-table) 'nil)
  (setf (gethash 'cloneforms cloneform-table) '(make-hash-table :test #'equal))
  (setf (gethash 'clonefunctions cloneform-table) '(make-hash-table :test #'equal)))

(defun set-up-standard-sheep-metasheep-clonefunctions (clonefun-table)
  (setf (gethash 'metasheep clonefun-table) (lambda () =standard-sheep-metasheep=))
  (setf (gethash 'nickname clonefun-table) (lambda () nil))
  (setf (gethash 'parents clonefun-table) (lambda () nil))
  (setf (gethash 'children clonefun-table) (lambda () nil))
  (setf (gethash 'properties clonefun-table) (lambda () (make-hash-table :test #'equal)))
  (setf (gethash 'property-owners clonefun-table) (lambda () (make-weak-hash-table :weakness :value :test #'equal)))
  (setf (gethash 'roles clonefun-table) (lambda () nil))
  (setf (gethash 'hierarchy-list clonefun-table) (lambda () nil))
  (setf (gethash 'cloneforms clonefun-table) (lambda () (make-hash-table :test #'equal)))
  (setf (gethash 'clonefunctions clonefun-table) (lambda () (make-hash-table :test #'equal))))

(defvar =standard-sheep-metasheep=
  (let ((object (std-generate-sheep-instance)))
    (setf (gethash 'metasheep object) object)
    (setf (gethash 'nickname object) nil)
    (setf (gethash 'parents object) nil)
    (setf (gethash 'children object) nil)
    (setf (gethash 'properties object) (make-hash-table :test #'equal))
    (setf (gethash 'property-owners object) (make-weak-hash-table :weakness :value :test #'equal))
    (setf (gethash 'roles object) nil)
    (setf (gethash 'hierarchy-list object) nil)
    (setf (gethash 'cloneforms object) 
	  (set-up-standard-sheep-metasheep-cloneforms (make-hash-table :test #'equal)))
    (setf (gethash 'clonefunctions object) 
	  (set-up-standard-sheep-metasheep-clonefunctions (make-hash-table :test #'equal)))    
    object))

(defvar =t=
  (let ((obj (generate-sheep-std-metasheep =standard-sheep-metasheep=
					   :parents nil
					   :options nil)))
    (setf (get-property obj 'nickname) "=t=")
    obj))

(defvar =dolly= 
  (clone (=t=)
	 ()
	 (:nickname "=dolly=")))

(add-parent =dolly= =standard-sheep-metasheep=)

;;; Wolves and wolf-handling
(defvar =white-fang= (clone (=t=) () (:nickname "=white-fang=")))
(defvar =symbol= (clone (=white-fang=)()(:nickname "=symbol=")))
(defvar =sequence= (clone (=white-fang=)()(:nickname "=sequence=")))
(defvar =array= (clone (=white-fang=)()(:nickname "=array=")))
(defvar =number= (clone (=white-fang=) () (:nickname "=number=")))
(defvar =character= (clone (=white-fang=) () (:nickname "=character=")))
(defvar =function= (clone (=white-fang=) () (:nickname "=function=")))
(defvar =hash-table= (clone (=white-fang=) () (:nickname "=hash-table=")))
(defvar =package= (clone (=white-fang=) () (:nickname "=package=")))
(defvar =pathname= (clone (=white-fang=) () (:nickname "=pathname=")))
(defvar =readtable= (clone (=white-fang=) () (:nickname "=readtable=")))
(defvar =stream= (clone (=white-fang=) () (:nickname "=stream=")))
(defvar =list= (clone (=sequence=) () (:nickname "=list=")))
(defvar =null= (clone (=symbol= =list=) () (:nickname "=null=")))
(defvar =cons= (clone (=list=) () (:nickname "=cons=")))
(defvar =vector= (clone (=array= =sequence=) () (:nickname "=vector=")))
(defvar =bit-vector= (clone (=vector=) () (:nickname "=bit-vector=")))
(defvar =string= (clone (=vector=) () (:nickname "=string=")))
(defvar =complex= (clone (=number=) () (:nickname "=complex=")))
(defvar =integer= (clone (=number=) () (:nickname "=integer=")))
(defvar =float= (clone (=number=) () (:nickname "=float=")))

(defun fleece-of (x)
  (if (sheep-p x)
      (progn
	(warn "This is already a sheep!")
	x)
      (typecase x
	(null                                          =null=)
	((and symbol (not null))                       =symbol=)
	((complex *)                                   =complex=)
	((integer * *)                                 =integer=)
	((float * *)                                   =float=)
	(cons                                          =cons=)
	(character                                     =character=)
	(hash-table                                    =hash-table=)
	(package                                       =package=)
	(pathname                                      =pathname=)
	(readtable                                     =readtable=)
	(stream                                        =stream=)
	((and number (not (or integer complex float))) =number=)
	((string *)                                    =string=)
	((bit-vector *)                                =bit-vector=)
	((and vector (not string))                     =vector=)
	((and array (not vector))                      =array=)
	((and sequence (not (or vector list)))         =sequence=)
	(function                                      =function=)
	(t                                             =white-fang=))))

;; Boxed object table
(let ((boxed-object-table (make-hash-table :test #'equal)))

  (defun find-fleeced-wolf (wolf)
    (if (sheep-p wolf)
	(error "~S seems to already be a sheep." wolf)
	(if (gethash wolf boxed-object-table)
	    (gethash wolf boxed-object-table)
	    (values (wear-wool wolf) nil))))

  (defun wear-wool (wolf)
    "Autoboxes WOLF"
    (setf (gethash wolf boxed-object-table) (clone ((fleece-of wolf)) ((wolf wolf)))))

  (defun shoot-wolf (wolf)
    "Kills wolf dead"
    (remhash wolf boxed-object-table))
    
  ) ; end boxed object table

(defun sheepify-list (obj-list)
  "Converts OBJ-LIST to a list where each item is either a sheep or a fleeced wolf."
  (mapcar #'sheepify obj-list))

(defun sheepify (sheep)
  "Returns SHEEP or fleeces it."
   (if (not (sheep-p sheep))
       (find-fleeced-wolf sheep)
       (values sheep nil)))

;;;
;;; Safe to define buzzwords and messages now, as well as use CLONE normally
;;;
(defbuzzword sheep-p-using-sheep)
(defmessage sheep-p-using-sheep (sheep)
)

(defbuzzword print-sheep
    (:documentation "Defines the expression print-object uses."))
(defmessage print-sheep (sheep stream)
  (print-unreadable-object (sheep stream :identity t)
    (format stream "Standard Sheep SID: ~a~@[ AKA: ~a~]" (sid sheep) (sheep-nickname sheep))))
(defmethod print-object :around (sheep stream)
  (if (sheep-p sheep)
      (print-sheep sheep stream)
      (call-next-method)))

;;; Property access again
(defbuzzword get-property-using-metasheep)
(defmessage get-property-using-metasheep ((metasheep =standard-sheep-metasheep=) sheep property-name)
  (declare (ignore metasheep))
  (std-get-property sheep property-name))

(defbuzzword setf-get-property-using-metasheep
    (:documentation "Sets the property, dispatching on the metasheep."))
(defmessage setf-get-property-using-metasheep (new-value
					       (metasheep =standard-sheep-metasheep=) 
					       sheep property-name)
  (declare (ignore metasheep))
  (setf (std-get-property sheep property-name) new-value))


(defbuzzword get-cloneform-using-metasheep)
(defmessage get-cloneform-using-metasheep ((metasheep =standard-sheep-metasheep=) sheep property-name)
  (declare (ignore metasheep))
  (std-get-cloneform sheep property-name))

(defbuzzword setf-get-cloneform-using-metasheep
    (:documentation "Sets the cloneform, dispatching on the metasheep."))
(defmessage setf-get-cloneform-using-metasheep (new-value
					       (metasheep =standard-sheep-metasheep=) 
					       sheep property-name)
  (declare (ignore metasheep))
  (setf (std-get-cloneform sheep property-name) new-value))

(defbuzzword get-clonefunction-using-metasheep)
(defmessage get-clonefunction-using-metasheep ((metasheep =standard-sheep-metasheep=) sheep property-name)
  (declare (ignore metasheep))
  (std-get-clonefunction sheep property-name))

(defbuzzword setf-get-clonefunction-using-metasheep
    (:documentation "Sets the clonefunction, dispatching on the metasheep."))
(defmessage setf-get-clonefunction-using-metasheep (new-value
					       (metasheep =standard-sheep-metasheep=) 
					       sheep property-name)
  (declare (ignore metasheep))
  (setf (std-get-clonefunction sheep property-name) new-value))

(defbuzzword remove-property-using-metasheep
    (:documentation "Locally removes the specified property"))
(defmessage remove-property-using-metasheep ((metasheep =standard-sheep-metasheep=)
					     sheep property-name)
  (declare (ignore metasheep))
  (std-remove-property sheep property-name))

(defbuzzword has-direct-property-p-using-metasheep
    (:documentation "Returns T if the specified property is present locally."))
(defmessage has-direct-property-p-using-metasheep ((metasheep =standard-sheep-metasheep=)
						   sheep property-name)
  (declare (ignore metasheep))
  (std-has-direct-property-p sheep property-name))

(defbuzzword who-sets-using-metasheep
    (:documentation "Returns the sheep object that SHEEP inherits the property-value from."))
(defmessage who-sets-using-metasheep ((metasheep =standard-sheep-metasheep=)
				      sheep property-name)
  (declare (ignore metasheep))
  (std-who-sets sheep property-name))

(defbuzzword available-properties-using-metasheep
    (:documentation "Returns a list of symbols of available properties."))
(defmessage available-properties-using-metasheep ((metasheep =standard-sheep-metasheep=)
						  sheep)
  (declare (ignore metasheep))
  (std-available-properties sheep))

(defbuzzword available-cloneforms-using-metasheep
    (:documentation "Returns a list of symbols of available cloneforms."))
(defmessage available-cloneforms-using-metasheep ((metasheep =standard-sheep-metasheep=)
						  sheep)
  (declare (ignore metasheep))
  (std-available-cloneforms sheep))

;;; lol i dunno
(defbuzzword generate-sheep
    (:documentation "Creates a new sheep object."))
