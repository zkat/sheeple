;; property-access.lisp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :sheeplette)

(defparameter *secret-unbound-value* (gensym))
(defparameter *secret-sheep-identifier* (gensym))
(define-condition unbound-property (sheeple-error) ())

(defun sheep-metasheep (sheep)
  (get-metasheep sheep))
(defun (setf sheep-metasheep) (new-mo sheep)
  (declare (ignore new-mo sheep))
  (error "Changing metasheeps is not supported right now"))

;;;
;;; property access
;;;

(defun get-property (sheep property-name)
  (if (eql (sheep-metasheep sheep) =standard-sheep-metasheep=)
      (std-get-property sheep property-name)
      (get-property-using-metasheep (sheep-metasheep sheep) sheep property-name)))
(defun std-get-property (sheep property-name)
  (get-property-with-memoized-owner sheep property-name))

(defun get-property-with-hierarchy-list (sheep property-name)
  "Finds a property value under PROPERTY-NAME using a hierarchy list."
  (let ((list (sheep-hierarchy-list sheep)))
   (loop for sheep in list
      do (multiple-value-bind (prop-obj has-p) 
	     (%get-property-object sheep property-name)
	   (when has-p
	     (return-from get-property-with-hierarchy-list (%value prop-obj))))
      finally (error 'unbound-property))))

(defun get-property-with-memoized-owner (sheep property-name)
  ;; Find who the owner is...
  (multiple-value-bind (prop-owner has-p)
      (gethash property-name (gethash 'property-owners sheep))
    (if has-p
	;; Get the actual value from that owner..
	(multiple-value-bind (value has-p)
	    (gethash property-name (gethash 'properties prop-owner))
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
  (let ((property-table (gethash 'properties sheep)))
    (setf (gethash property-name property-table) new-value)))

(defun get-cloneform (sheep property-name)
  (if (eql (sheep-metasheep sheep) =standard-sheep-metasheep=)
      (std-get-cloneform sheep property-name)
      (get-cloneform-using-metasheep (sheep-metasheep sheep) sheep property-name)))
(defun std-get-cloneform (sheep property-name)
  (loop for obj in (sheep-hierarchy-list sheep)
       do (let ((cloneform-table (gethash 'cloneforms obj)))
	    (multiple-value-bind (value has-p)
		(gethash property-name cloneform-table)
	      (when has-p
		(return-from std-get-cloneform value))))
       finally (return *secret-unbound-value*)))

(defun (setf get-cloneform) (new-value sheep property-name)
  (if (eql (sheep-metasheep sheep) =standard-sheep-metasheep=)
      (setf (std-get-cloneform sheep property-name) new-value)
      (setf-get-cloneform-using-metasheep
       new-value (sheep-metasheep sheep) sheep property-name)))
(defun (setf std-get-cloneform) (new-value sheep property-name)
  (setf (gethash property-name (gethash 'cloneforms sheep)) new-value))

(defun get-clonefunction (sheep property-name)
  (if (eql (sheep-metasheep sheep) =standard-sheep-metasheep=)
      (std-get-clonefunction sheep property-name)
      (get-clonefunction-using-metasheep (sheep-metasheep sheep) sheep property-name)))
(defun std-get-clonefunction (sheep property-name)
  (loop for obj in (sheep-hierarchy-list sheep)
       do (let ((clonefunction-table (gethash 'clonefunctions obj)))
	    (multiple-value-bind (value has-p)
		(gethash property-name clonefunction-table)
	      (when has-p
		(return-from std-get-clonefunction value))))
       finally (return *secret-unbound-value*)))

(defun (setf get-clonefunction) (new-value sheep property-name)
  (if (eql (sheep-metasheep sheep) =standard-sheep-metasheep=)
      (setf (std-get-clonefunction sheep property-name) new-value)
      (setf-get-clonefunction-using-metasheep
       new-value (sheep-metasheep sheep) sheep property-name)))
(defun (setf std-get-clonefunction) (new-value sheep property-name)
  (setf (gethash property-name (gethash 'clonefunctions sheep)) new-value))

(defun remove-property (sheep property-name)
  (if (eql (sheep-metasheep sheep) =standard-sheep-metasheep=)
      (std-remove-property sheep property-name)
      (remove-property-using-metasheep (sheep-metasheep sheep) sheep property-name)))
(defun std-remove-property (sheep property-name)
  (remhash property-name (gethash 'properties sheep))
  (memoize-property-access sheep))

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
      (gethash property-name (gethash 'properties sheep))
    value
    has-p))

(defun who-sets (sheep property-name)
  (if (eql (sheep-metasheep sheep) =standard-sheep-metasheep=)
      (std-who-sets sheep property-name)
      (who-sets-using-metasheep (sheep-metasheep sheep) sheep property-name)))
(defun std-who-sets (sheep property-name)
  (multiple-value-bind (owner has-p)
      (gethash property-name (gethash 'property-owners sheep))
    (if has-p
	owner
	(error 'unbound-property))))

(defun available-properties (sheep)
  (if (eql (sheep-metasheep sheep) =standard-sheep-metasheep=)
      (std-available-properties sheep)
      (available-properties-using-metasheep (sheep-metasheep sheep) sheep)))
(defun std-available-properties (sheep)
  (let ((obj-keys (loop for keys being the hash-keys of (gethash 'properties sheep)
		     collect keys)))
    (remove-duplicates
     (flatten
      (append obj-keys (mapcar #'available-properties (gethash 'parents sheep)))))))

(defun available-cloneforms (sheep)
  (if (eql (sheep-metasheep sheep) =standard-sheep-metasheep=)
      (std-available-cloneforms sheep)
      (available-cloneforms-using-metasheep (sheep-metasheep sheep) sheep)))
(defun std-available-cloneforms (sheep)
  (let ((obj-keys (loop for keys being the hash-keys of (gethash 'cloneforms sheep)
		     collect keys)))
    (remove-duplicates
     (flatten
      (append obj-keys (mapcar #'available-cloneforms (gethash 'parents sheep)))))))

;;; Memoization
(defun memoize-property-access (sheep)
  (loop for property in (available-properties sheep)
     do (let ((owner (%get-property-owner sheep property)))
	  (setf (gethash property (gethash 'property-owners sheep))
		owner))))

(defun %get-property-owner (sheep property-name)
  (let ((hierarchy-list (gethash 'hierarchy-list sheep)))
    (loop for sheep-obj in hierarchy-list
       do (multiple-value-bind (value has-p)
	      (gethash property-name sheep-obj)
	    (declare (ignore value))
	    (when has-p
	      (return-from %get-property-owner sheep-obj))) 
       finally (error 'unbound-property))))

(defun memoize-sheep-hierarchy-list (sheep)
  (let ((list (compute-sheep-hierarchy-list sheep)))
    (setf (gethash 'hierarchy-list sheep)
	  list)
    (mapc (lambda (descendant) 
	    (memoize-sheep-hierarchy-list (weak-pointer-value descendant)))
	  (gethash 'children sheep))))

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
