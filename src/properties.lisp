;; This file is part of Sheeple

;; properties.lisp
;;
;; Property access, inspection, and management stuff, for the most part.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :sheeple)

(declaim (optimize (speed 3) (debug 1) (safety 1)))
(defparameter *secret-unbound-value* (gensym))
;;;
;;; Property Access
;;;
(defgeneric direct-property-value (sheep property-name))
(defmethod direct-property-value ((sheep standard-sheep) property-name)
  (unless (symbolp property-name)
    (error "Property-name must be a symbol"))
  (gethash property-name (sheep-direct-properties sheep)))

(defgeneric property-value (sheep property-name))
(defmethod property-value ((sheep standard-sheep) property-name)
  (property-value-with-hierarchy-list sheep property-name))

(defun property-value-with-hierarchy-list (sheep property-name)
  (let ((hl (sheep-hierarchy-list sheep)))
    (or (loop for sheep in hl
           do (multiple-value-bind (value hasp)
                  (direct-property-value sheep property-name)
                (when hasp
                  (return value))))
        (error 'unbound-property
               :format-control "Property ~A is unbound for sheep ~S"
               :format-args (list property-name sheep)))))

(defgeneric (setf property-value) (new-value sheep property-name))
(defmethod (setf property-value) (new-value (sheep standard-sheep) property-name)
  (unless (symbolp property-name)
    (error "Property-name must be a symbol"))
  (if (sheep-locked-p sheep)
      (error "Cannot set property ~A: ~A is locked." property-name sheep)
      (let ((property-table (sheep-direct-properties sheep)))
	(setf (gethash property-name property-table) new-value))))

(defgeneric remove-property (sheep property-name))
(defmethod remove-property ((sheep standard-sheep) property-name)
  (if (sheep-locked-p sheep)
      (error "Cannot remove property ~A: ~A is locked." property-name sheep)
      (remhash property-name (sheep-direct-properties sheep))))

(defgeneric has-property-p (sheep property-name))
(defmethod has-property-p ((sheep standard-sheep) property-name)
  "Returns T if a property with PROPERTY-NAME is available to SHEEP."
  (handler-case
      (when (property-value sheep property-name)
	t)
    (unbound-property () nil)))

(defgeneric has-direct-property-p (sheep property-name))
(defmethod has-direct-property-p ((sheep standard-sheep) property-name)
  (multiple-value-bind (value has-p)
      (gethash property-name (sheep-direct-properties sheep))
    value
    has-p))

(defgeneric property-owner (sheep property-owner))
(defmethod property-owner ((sheep standard-sheep) property-name)
  (if (has-direct-property-p sheep property-name)
      sheep
      (let ((hl (sheep-hierarchy-list sheep)))
	(loop for ancestor in hl
	   do (multiple-value-bind (value has-p)
		  (gethash property-name (sheep-direct-properties ancestor))
		(declare (ignore value))
		(when has-p
		  (return-from property-owner ancestor)))
	   finally (error 'unbound-property
			  :format-control "Property ~A is unbound for sheep ~S"
			  :format-args (list property-name sheep))))))

(defgeneric available-properties (sheep))
(defmethod available-properties ((sheep standard-sheep))
  (let ((obj-keys (loop for keys being the hash-keys of (sheep-direct-properties sheep)
		     collect keys)))
    (remove-duplicates
     (flatten
      (append obj-keys (mapcar #'available-properties (sheep-direct-parents sheep)))))))

;;;
;;; Cloneforms/functions
;;;
(defgeneric get-cloneform (sheep property-name))
(defmethod get-cloneform ((sheep standard-sheep) property-name)
  (loop for obj in (sheep-hierarchy-list sheep)
     do (let ((cloneform-table (sheep-cloneforms obj)))
	  (multiple-value-bind (value has-p)
	      (gethash property-name cloneform-table)
	    (when has-p
	      (return-from get-cloneform value))))
     finally (return *secret-unbound-value*)))

(defgeneric inspect-cloneform (sheep property-name))
(defmethod inspect-cloneform ((sheep standard-sheep) property-name)
  (let ((form (get-cloneform sheep property-name)))
    (if (eq *secret-unbound-value* form)
	(error 'sheeple-error
	       :format-control "~A has no applicable cloneform for property ~A"
	       :format-args (list sheep property-name))
	form)))

(defgeneric (setf get-cloneform) (new-value sheep property-name))
(defmethod (setf get-cloneform) (new-value (sheep standard-sheep) property-name)
  (setf (gethash property-name (sheep-cloneforms sheep)) new-value))

(defgeneric get-clonefunction (sheep property-name))
(defmethod get-clonefunction ((sheep standard-sheep) property-name)
  (loop for obj in (sheep-hierarchy-list sheep)
       do (let ((clonefunction-table (sheep-clonefunctions obj)))
	    (multiple-value-bind (value has-p)
		(gethash property-name clonefunction-table)
	      (when has-p
		(return-from get-clonefunction value))))
       finally (return *secret-unbound-value*)))

(defgeneric (setf get-clonefunction) (new-value sheep property-name))
(defmethod (setf get-clonefunction) (new-value (sheep standard-sheep) property-name)
  (setf (gethash property-name (sheep-clonefunctions sheep)) new-value))

(defgeneric available-cloneforms (sheep))
(defmethod available-cloneforms ((sheep standard-sheep))
  (let ((obj-keys (loop for keys being the hash-keys of (sheep-cloneforms sheep)
		     collect keys)))
    (remove-duplicates
     (flatten
      (append obj-keys (mapcar #'available-cloneforms (sheep-direct-parents sheep)))))))

(defgeneric cloneform-owner (sheep property-name))
(defmethod cloneform-owner ((sheep standard-sheep) property-name)
  (unless (symbolp property-name)
    (error "Property-name must be a symbol"))
  (find-if (lambda (sheep-obj)
	     (multiple-value-bind (value has-p)
		 (gethash property-name (sheep-cloneforms sheep-obj))
	       (declare (ignore value))
	       has-p))
	   (sheep-hierarchy-list sheep)))

(defgeneric remove-cloneform (sheep property-name))
(defmethod remove-cloneform ((sheep standard-sheep) property-name)
  (unless (symbolp property-name)
    (error "Property-name must be a symbol"))
  (let ((cloneform-table (sheep-cloneforms sheep))
	(clonefun-table (sheep-clonefunctions sheep)))
    (multiple-value-bind (form has-p)
	(gethash property-name cloneform-table)
      (declare (ignore form))
      (if (not has-p)
	  (error 'sheeple-error
		 :format-control "~A has no direct cloneform for property ~A~% ~
                                  If there is an applicable cloneform, use CLONEFORM-OWNER~% ~
                                  To find who sets it."
		 :format-args (list sheep property-name))
	  (prog1 t
	    (remhash property-name cloneform-table)
	    (remhash property-name clonefun-table))))))

(defmacro add-cloneform (sheep property-name form)
  `(let ((clonefun (lambda () ,form)))
     (unless (symbolp ,property-name)
       (error "Property-name must be a symbol"))
     (setf (get-cloneform ,sheep ,property-name) ,form)
     (setf (get-clonefunction ,sheep ,property-name) clonefun)
     (values)))

;;;
;;; Memoization
;;;
;; TODO: Reimplement memoization of property-access, if I find a nice way to do it.
;;       Nice, of course, being "thread-compatible, and lightweight on the SETF front"
;;       Something similar to message caching might work, but I'm not sure anymore.

;; (defun property-value-with-memoized-owner (sheep property-name)
;;   ;; Find who the owner is...
;;   (multiple-value-bind (prop-owner has-p)
;;       (gethash property-name (sheep-property-owners sheep))
;;     (if has-p
;; 	;; Get the actual value from that owner..
;; 	(multiple-value-bind (value has-p)
;; 	    (gethash property-name (sheep-direct-properties prop-owner))
;; 	  (if has-p
;; 	      value
;; 	      (error 'unbound-property
;; 		     :format-control "Property ~A is unbound for sheep ~S"
;; 		     :format-args (list property-name sheep))))
;; 	(error 'unbound-property
;; 	       :format-control "Property ~A is unbound for sheep ~S"
;; 	       :format-args (list property-name sheep)))))

;; (defun %property-value-owner (sheep property-name)
;;   (let ((hierarchy-list (sheep-hierarchy-list sheep)))
;;     (loop for sheep-obj in hierarchy-list
;;        do (multiple-value-bind (value has-p)
;; 	      (gethash property-name (sheep-direct-properties sheep-obj))
;; 	    (declare (ignore value))
;; 	    (when has-p
;; 	      (return-from %property-value-owner sheep-obj))) 
;;        finally (error 'unbound-property
;; 		      :format-control "Property ~A is unbound for sheep ~S"
;; 		      :format-args (list property-name sheep)))))

;; (defun memoize-property-access (sheep property &optional owner)
;;   (let ((actual-owner (or owner
;; 			  (when (has-direct-property-p sheep property)
;; 			    sheep)
;; 			  (%property-value-owner sheep property))))
;;     (setf (gethash property (sheep-property-owners sheep))
;; 	  actual-owner)))

;; (defun memoization-update-descendants (sheep property &optional owner)
;;   (let ((actual-owner (or owner
;; 			  (when (has-direct-property-p sheep property)
;; 			    sheep)
;; 			  (%property-value-owner sheep property))))
;;     (loop for child-pointer in (sheep-direct-children sheep)
;;        do (progn
;; 	    (memoize-specific-property-access (weak-pointer-value child-pointer)
;; 					      property actual-owner)
;; 	    (memoization-update-descendants (weak-pointer-value child-pointer)
;; 					    property actual-owner)))))


