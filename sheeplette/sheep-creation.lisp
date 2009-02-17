;; sheep-creation.lisp
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :sheeplette)

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

(defun metasheep-nickname (sheep)
  (property-value sheep 'nickname))
(defun (setf metasheep-nickname) (new-value sheep)
  (setf (property-value sheep 'nickname) new-value))

(defun metasheep-direct-parents (sheep)
  (property-value sheep 'parents))
(defun (setf metasheep-direct-parents) (new-value sheep)
  (setf (property-value sheep 'parents) new-value))

(defun metasheep-direct-children (sheep)
  (property-value sheep 'children))
(defun (setf metasheep-direct-children) (new-value sheep)
  (setf (property-value sheep 'children) new-value))

(defun metasheep-direct-properties (sheep)
  (property-value 'properties sheep))
(defun (setf metasheep-direct-properties) (new-value sheep)
  (setf (property-value 'properties sheep) new-value))

(defun metasheep-direct-cloneforms (sheep)
  (property-value sheep 'cloneforms))
(defun (setf metasheep-direct-cloneforms) (new-value sheep)
  (setf (property-value sheep 'cloneforms) new-value))

(defun metasheep-direct-clonefunctions (sheep)
  (property-value sheep 'clonefunctions))
(defun (setf metasheep-direct-clonefunctions) (new-value sheep)
  (setf (property-value	sheep 'clonefunctions) new-value))

(defun metasheep-property-owners (sheep)
  (property-value sheep 'property-owners))
(defun (setf metasheep-property-owners) (new-value sheep)
  (setf (property-value sheep 'property-owners) new-value))

(defun metasheep-direct-roles (sheep)
  (property-value sheep 'roles))
(defun (setf metasheep-direct-roles) (new-value sheep)
  (setf (property-value sheep 'roles) new-value))

(defun metasheep-hierarchy-list (sheep)
  (property-value sheep 'hierarchy-list))
(defun (setf metasheep-hierarchy-list) (new-value sheep)
  (setf (property-value sheep 'hierarchy-list) new-value))

;;;
;;; Cloning
;;;

;;; sheep storage
(defun std-generate-sheep-instance (metasheep)
  "Ex Nihilo creation of a standard sheep instance."
  (let ((table (make-hash-table :test #'equal)))
    (setf (gethash *secret-sheep-identifier* table)
	  *secret-sheep-identifier*)
    (setf (gethash 'metasheep table) metasheep)
    table))



(defun std-spawn-sheep (metasheep-prototype 
				  &key parents properties options 
				  &allow-other-keys)
  (declare (ignore metasheep))
  (let ((sheep (std-generate-sheep-instance nil)))
    ;; First we actually set up all the properties
    ;; The canonical required properties...
    (setf (sheep-direct-parents sheep) nil)
    (setf (sheep-direct-properties sheep) (make-hash-table :test #'equal))
    (setf (sheep-direct-roles sheep) nil)
    (setf (sheep-direct-cloneforms sheep) (make-hash-table :test #'equal))
    (setf (sheep-direct-clonefunctions sheep) (make-hash-table :test #'equal))
    ;; Additional =standard-sheep-metasheep= properties
    (setf (sheep-nickname sheep) nil)
    (setf (sheep-direct-children sheep) nil)
    (setf (sheep-property-owners sheep) (make-weak-hash-table :weakness :value :test #'equal))
    (setf (sheep-hierarchy-list sheep) nil)
    ;; Then we deal with the options
    (std-add-parents sheep parents)
    (std-execute-clonefunctions sheep)
    (std-set-up-properties sheep properties)
    (set-up-other-options options sheep)
    (std-finalize-sheep sheep)
    sheep))

(defun spawn-sheep (sheeple properties
		    &rest all-keys
		    &key (metasheep-prototype nil)
		    &allow-other-keys)
  "Creates a new sheep with SHEEPLE as its parents, and PROPERTIES as its properties"
  (let ((sheep (apply (if (eql metasheep nil)
			  #'std-spawn-sheep
			  #'spawn-sheep-using-metasheep-prototype)
		      metasheep-prototype
		      :parents sheeple 
		      :properties properties
		      all-keys)))
    sheep))

(defun std-add-parents (sheep parents)
  (let ((real-parents (or parents
			  (list =dolly=))))
    (setf (sheep-direct-parents sheep) real-parents)
    (memoize-sheep-hierarchy-list sheep)
    sheep))

(defun std-set-up-properties (sheep properties)
  (loop for property-list in properties
     do (set-up-property sheep property-list)))
(defun set-up-property (sheep property-list)
  (if (std-sheep-p sheep)
      (std-set-up-property sheep property-list)
      (set-up-property-using-metasheep (sheep-metasheep sheep)
				       sheep property-list)))
(defun std-set-up-property (sheep property-list)
  (let ((name (getf property-list :name))
	(value (getf property-list :value))
	(readers (getf property-list :readers))
	(writers (getf property-list :writers)))
    (when (not (symbolp name))
      (error "Property names must be symbols"))
    (setf (get-property sheep name) value)
    (add-readers-to-sheep readers name sheep)
    (add-writers-to-sheep writers name sheep)))

(defun execute-clonefunctions (sheep)
  (if (std-sheep-p sheep)
      (std-execute-clonefunctions sheep)
      (execute-clonefunctions-using-metasheep 
       (sheep-metasheep sheep) sheep)))
(defun std-execute-clonefunctions (sheep)
  (let* ((available-cloneforms (available-cloneforms sheep))
	 (functions (loop for property in available-cloneforms
		       collect (get-clonefunction sheep property))))
    (loop
       for fn in functions
       for propname in available-cloneforms
       do (unless (eql fn *secret-unbound-value*)
	    (setf (get-property sheep propname) (funcall fn))))))

(define-condition invalid-option-error (sheeple-error) ())
(defun set-up-other-options (options sheep)
  (loop for option in options
     do (set-up-option option sheep)))
(defun set-up-option (sheep option)
  (if (std-sheep-p)
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
      (otherwise (error 'invalid-option-error)))))

(defun std-finalize-sheep (sheep)
  (memoize-sheep-hierarchy-list sheep)
  (memoize-property-access sheep)
  (loop for child-pointer in (gethash 'children sheep)
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
  (if (and (std-sheep-p new-parent)
	   (std-sheep-p child))
      (std-add-parent new-parent child)
      (add-parent-using-metasheeps
       (sheep-metasheep new-parent) (sheep-metasheep child)
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
  (if (and (std-sheep-p parent)
	   (std-sheep-p child))
      (std-remove-parent parent child)
      (remove-parent-using-metasheeps
       (sheep-metasheep parent) (sheep-metasheep child)
       parent child)))
(defun std-remove-parent (parent child &key (keep-properties nil))
  (setf (sheep-direct-parents child)
	(delete parent (sheep-direct-parents child)))
  (setf (sheep-direct-children parent)
	(delete child (sheep-direct-children parent) :key #'weak-pointer-value))
  (when keep-properties
    (loop for property-name being the hash-keys of (sheep-direct-properties parent)
       using (hash-value value)
       do (unless (has-direct-property-p child property-name)
	    (setf (get-property child property-name) value))))
  (std-finalize-sheep child)
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
		      (union (gethash 'parents sheep-to-process)
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
		(butlast (gethash 'parents sheep)))
	  (gethash 'parents sheep)))

(defun std-tie-breaker-rule (minimal-elements cpl-so-far)
  (dolist (cpl-constituent (reverse cpl-so-far))
    (let* ((supers (gethash 'parents cpl-constituent))
           (common (intersection minimal-elements supers)))
      (when (not (null common))
        (return-from std-tie-breaker-rule (car common))))))

;;;
;;; Wolves and fleecing
;;;

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
;;; Macro
;;;
(defun canonize-sheeple (sheeple)
  `(list ,@(mapcar #'canonize-sheep sheeple)))

(defun canonize-sheep (sheep)
  `(confirm-sheep ,sheep))

(defun confirm-sheep (sheep)
  sheep)

(defun canonize-properties (properties)
  `(list ,@(mapcar #'canonize-property properties)))

(defun canonize-property (property)
  (if (symbolp property)
      `(list :name ',property)
      (let ((name (car property))
	    (value (cadr property))
            (readers nil)
            (writers nil)
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
	    (:cloneform
	     (setf cloneform (cadr olist)))
	    (otherwise 
             (pushnew (cadr olist) other-options)
             (pushnew (car olist) other-options))))
	(if other-options
	    (error "Invalid property option(s)")
	    `(list
	      :name ',name
	      :value ,value
	      ,@(when (not (eql cloneform *secret-unbound-value*))
		      `(:cloneform ',cloneform))
	      ,@(when readers `(:readers ',readers))
	      ,@(when writers `(:writers ',writers)))))))

(defun canonize-clone-options (options)
  (mapappend #'canonize-clone-option options))

(defun canonize-clone-option (option)
  (list `',(car option) `',(cadr option)))

(defmacro clone (sheeple properties &rest options)
  "Standard sheep-generation macro"
  `(spawn-sheep
    ,(canonize-sheeple sheeple)
    ,(canonize-properties properties)
    ,@(canonize-clone-options options)))

;;; Inheritance predicates
(defun direct-parent-p (maybe-parent child)
  (when (member maybe-parent (gethash 'parents child))
    t))

(defun ancestor-p (maybe-ancestor descendant)
  (when (and (not (equal maybe-ancestor descendant))
	     (member maybe-ancestor (collect-parents descendant)))
    t))

(defun direct-child-p (maybe-child parent)
  (direct-parent-p parent maybe-child))

(defun descendant-p (maybe-descendant ancestor)
  (ancestor-p ancestor maybe-descendant))

