;; This file is part of Sheeple

;; sheep-creation.lisp
;;
;; Sheep creation, cloning, inspection
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :sheeple)

(defstruct (sheep (:constructor %make-sheep))
  (nickname nil)
  (direct-parents nil)
  (direct-children nil)
  (direct-properties (make-hash-table :test #'eq))
  (property-owners (make-weak-hash-table :weakness :value :test #'eq))
  (direct-roles nil)
  (clonefunctions (make-hash-table :test #'eq))
  (cloneforms (make-hash-table :test #'eq))
  (hierarchy-list nil))

;;;
;;; Standard sheeple
;;;
(defvar =t=)
(defvar =dolly=)
(defvar =white-fang=)
(defvar =symbol=)
(defvar =sequence=)
(defvar =array=)
(defvar =number=)
(defvar =character=)
(defvar =function=)
(defvar =hash-table=)
(defvar =package=)
(defvar =pathname=)
(defvar =readtable=)
(defvar =stream=)
(defvar =list=)
(defvar =null=)
(defvar =cons=)
(defvar =vector=)
(defvar =bit-vector=)
(defvar =string=)
(defvar =complex=)
(defvar =integer=)
(defvar =float=)
(defvar =standard-sheep-metasheep=)
(defvar =standard-role-metasheep=)
(defvar =standard-message-metasheep=)
(defvar =standard-buzzword-metasheep=)

;;;
;;; Cloning
;;;

;;; sheep storage
(defun initialize-sheep (sheep
			 &key parents
			 properties
			 nickname
			 deep-copy
			 shallow-copy)
    (add-parents sheep parents)
    (set-up-properties sheep properties)
    (execute-clonefunctions sheep)
    (setf (sheep-nickname sheep) nickname)
    (finalize-sheep sheep)
    (when shallow-copy
      (shallow-copy sheep))
    (when deep-copy
      (deep-copy sheep))
    sheep)

(defun spawn-sheep (sheeple properties
		    &rest all-keys)
  "Creates a new sheep with SHEEPLE as its parents, and PROPERTIES as its properties"
  (let ((sheep (apply #'initialize-sheep
		      (%make-sheep)
		      :parents sheeple 
		      :properties properties
		      all-keys)))
    sheep))

(defun reinitialize-sheep (sheep new-parents new-properties &key nickname deep-copy shallow-copy)
  ;; cleanup
  (loop for parent in (sheep-direct-parents sheep)
       do (remove-parent parent sheep))
  (clrhash (sheep-cloneforms sheep))
  (clrhash (sheep-clonefunctions sheep))
  (clrhash (sheep-direct-properties sheep))
  ;; initialize again
  (initialize-sheep sheep 
		    :parents new-parents
		    :properties new-properties
		    :nickname nickname
		    :deep-copy deep-copy
		    :shallow-copy shallow-copy))

(defun swap-sheep (old-sheep new-sheep)
  "swaps stuff from new-sheep into old-sheep while maintaining old-sheep's identity"
  (let ((new-parents (sheep-direct-parents new-sheep))
	(new-properties (sheep-direct-properties new-sheep)))
    (loop for parent in (sheep-direct-parents old-sheep)
	 do (remove-parent parent old-sheep))
    (setf (sheep-direct-parents old-sheep) new-parents)
    (setf (sheep-direct-properties old-sheep) new-properties)
    old-sheep))

(defun swap-sheep-or-make-new (old-sheep new-sheep)
  (if (sheep-p old-sheep)
      (swap-sheep old-sheep new-sheep)
      new-sheep))

(defun mitosis (model)
  (let* ((parents (sheep-direct-parents model))
	 (properties (sheep-direct-properties model))
	 (roles (sheep-direct-roles model))
	 (new-sheep (clone () ())))
    (setf (sheep-direct-parents new-sheep)
	  parents)
    (setf (sheep-direct-properties new-sheep)
	  properties)
    (setf (sheep-direct-roles new-sheep)
	  roles)
    new-sheep))

(defun add-parents (sheep parents)
  (let ((real-parents (or parents
			  (list =dolly=))))
    (setf (sheep-direct-parents sheep) real-parents)
    (loop for parent in parents
       do (let ((pointer (make-weak-pointer sheep)))
    	    (pushnew pointer (sheep-direct-children parent))
    	    (finalize sheep (lambda () (setf (sheep-direct-children parent)
    					     (delete pointer
    						     (sheep-direct-children parent)))))))
    (memoize-sheep-hierarchy-list sheep)
    sheep))

(defun set-up-properties (sheep properties)
  (loop for property-list in properties
     do (set-up-property sheep property-list)))
(defun set-up-property (sheep property-list)
  (let ((name (getf property-list :name))
	(value (getf property-list :value))
	(readers (getf property-list :readers))
	(writers (getf property-list :writers))
	(cloneform-present-p (member :cloneform property-list))
	(cloneform (getf property-list :cloneform))
	(clonefunction (getf property-list :clonefunction)))
    (when (not (symbolp name))
      (error "Property names must be symbols"))
    (when cloneform-present-p
      (set-up-cloneform sheep name cloneform clonefunction))
    (setf (property-value sheep name) value)
    (add-readers-to-sheep readers name sheep)
    (add-writers-to-sheep writers name sheep)))

(defun set-up-cloneform (sheep pname form function)
  (setf (get-cloneform sheep pname) form)
  (setf (get-clonefunction sheep pname) function))

(defun execute-clonefunctions (sheep)
  (let* ((available-cloneforms (available-cloneforms sheep))
	 (functions (loop for property in available-cloneforms
		       collect (get-clonefunction sheep property))))
    (loop
       for fn in functions
       for propname in available-cloneforms
       do (unless (or (eql fn *secret-unbound-value*)
		      (has-direct-property-p sheep propname))
	    (setf (property-value sheep propname) (funcall fn))))))

(defun finalize-sheep (sheep)
  (memoize-sheep-hierarchy-list sheep)
  (memoize-property-access sheep)
  (loop for child-pointer in (sheep-direct-children sheep)
     do (memoize-property-access (weak-pointer-value child-pointer)))
  sheep)

(defun deep-copy (sheep)
  (let ((all-property-names (available-properties sheep)))
    (mapc (lambda (pname)
	    (let ((value (property-value sheep pname)))
	      (setf (property-value sheep pname)
		    value)))
	  all-property-names)))

(defun shallow-copy (sheep)
  (mapc (lambda (parent)
	  (maphash 
	   (lambda (key value) 
	     (setf (property-value sheep key) value))
	   (sheep-direct-properties parent)))
	(sheep-direct-parents sheep)))

;;; Inheritance setup
(defun add-parent (new-parent child)
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
	 (finalize-sheep child)
	 child)))

(defun remove-parent (parent child &key (keep-properties nil))
  (setf (sheep-direct-parents child)
	(delete parent (sheep-direct-parents child)))
  (setf (sheep-direct-children parent)
	(delete child (sheep-direct-children parent) :key #'weak-pointer-value))
  (when keep-properties
    (loop for property-name being the hash-keys of (sheep-direct-properties parent)
       using (hash-value value)
       do (unless (has-direct-property-p child property-name)
	    (setf (property-value child property-name) value))))
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

;;; Inheritance predicates
(defun direct-parent-p (maybe-parent child)
  (when (member maybe-parent (sheep-direct-parents child))
    t))

(defun ancestor-p (maybe-ancestor descendant)
  (when (and (not (eql maybe-ancestor descendant))
	     (member maybe-ancestor (collect-parents descendant)))
    t))

(defun direct-child-p (maybe-child parent)
  (direct-parent-p parent maybe-child))

(defun descendant-p (maybe-descendant ancestor)
  (ancestor-p ancestor maybe-descendant))

;; Memoization
(defun memoize-sheep-hierarchy-list (sheep)
  (let ((list (compute-sheep-hierarchy-list sheep)))
    (setf (sheep-hierarchy-list sheep)
	  list)
    (mapc (lambda (descendant) 
	    (memoize-sheep-hierarchy-list (weak-pointer-value descendant)))
	  (sheep-direct-children sheep))))