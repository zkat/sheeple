;; This file is part of Sheeple

;; properties.lisp
;;
;; Property access, inspection, and management stuff, for the most part.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :sheeple)

(declaim (optimize (speed 3) (debug 0) (safety 0)))
(defparameter *secret-unbound-value* (gensym))
;;;
;;; Property Access
;;;
(defun property-value (sheep property-name)
  (unless (symbolp property-name)
    (error "Property-name must be a symbol"))
  (multiple-value-bind (value has-p)
      (gethash property-name (sheep-direct-properties sheep))
    (if has-p
	value
	(property-value-with-hierarchy-list sheep property-name))))

(defun property-value-with-hierarchy-list (sheep property-name)
  (let ((hl (sheep-hierarchy-list sheep)))
    (loop for ancestor in hl
       do (multiple-value-bind (value has-p)
	      (gethash property-name (sheep-direct-properties ancestor))
	    (when has-p
	      (return-from property-value-with-hierarchy-list value)))
       finally (error 'unbound-property
		      :format-control "Property ~A is unbound for sheep ~S"
		      :format-args (list property-name sheep)))))

(defun (setf property-value) (new-value sheep property-name)
  (unless (symbolp property-name)
    (error "Property-name must be a symbol"))
  (let ((property-table (sheep-direct-properties sheep)))
    (setf (gethash property-name property-table) new-value)))

(defun remove-property (sheep property-name)
  (remhash property-name (sheep-direct-properties sheep)))

(defun has-property-p (sheep property-name)
  "Returns T if a property with PROPERTY-NAME is available to SHEEP."
  (handler-case
      (when (property-value sheep property-name)
	t)
    (unbound-property () nil)))

(defun has-direct-property-p (sheep property-name)
  (multiple-value-bind (value has-p)
      (gethash property-name (sheep-direct-properties sheep))
    value
    has-p))

(defun who-sets (sheep property-name)
  (if (has-direct-property-p sheep property-name)
      sheep
      (let ((hl (sheep-hierarchy-list sheep)))
	(loop for ancestor in hl
	   do (multiple-value-bind (value has-p)
		  (gethash property-name (sheep-direct-properties ancestor))
		(declare (ignore value))
		(when has-p
		  (return-from who-sets ancestor)))
	   finally (error 'unbound-property
			  :format-control "Property ~A is unbound for sheep ~S"
			  :format-args (list property-name sheep))))))

(defun available-properties (sheep)
  ;; TODO - According to SB-SPROF, this is a huge bottleneck when setfing properties
  (let ((obj-keys (loop for keys being the hash-keys of (sheep-direct-properties sheep)
		     collect keys)))
    (remove-duplicates
     (flatten
      (append obj-keys (mapcar #'available-properties (sheep-direct-parents sheep)))))))

;;;
;;; Cloneforms/functions
;;;
(defun get-cloneform (sheep property-name)
  (loop for obj in (sheep-hierarchy-list sheep)
     do (let ((cloneform-table (sheep-cloneforms obj)))
	  (multiple-value-bind (value has-p)
	      (gethash property-name cloneform-table)
	    (when has-p
	      (return-from get-cloneform value))))
     finally (return *secret-unbound-value*)))

(defun (setf get-cloneform) (new-value sheep property-name)
  (setf (gethash property-name (sheep-cloneforms sheep)) new-value))

(defun get-clonefunction (sheep property-name)
  (loop for obj in (sheep-hierarchy-list sheep)
       do (let ((clonefunction-table (sheep-clonefunctions obj)))
	    (multiple-value-bind (value has-p)
		(gethash property-name clonefunction-table)
	      (when has-p
		(return-from get-clonefunction value))))
       finally (return *secret-unbound-value*)))

(defun (setf get-clonefunction) (new-value sheep property-name)
  (setf (gethash property-name (sheep-clonefunctions sheep)) new-value))

(defun available-cloneforms (sheep)
  (let ((obj-keys (loop for keys being the hash-keys of (sheep-cloneforms sheep)
		     collect keys)))
    (remove-duplicates
     (flatten
      (append obj-keys (mapcar #'available-cloneforms (sheep-direct-parents sheep)))))))

(defun cloneform-owner (sheep property-name)
  (unless (symbolp property-name)
    (error "Property-name must be a symbol"))
  (find-if (lambda (sheep-obj)
	     (multiple-value-bind (value has-p)
		 (gethash property-name (sheep-cloneforms sheep-obj))
	       (declare (ignore value))
	       has-p))
	   (sheep-hierarchy-list sheep)))

(defun remove-cloneform (sheep property-name)
  (unless (symbolp property-name)
    (error "Property-name must be a symbol"))
  (let ((cloneform-table (sheep-cloneforms sheep))
	(clonefun-table (sheep-clonefunctions sheep)))
    (multiple-value-bind (form has-p)
	(gethash property-name cloneform-table)
      (declare (ignore form))
      (if (not has-p)
	  nil
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


