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
 ;; lulz

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

(defun generate-sheep-object (&key (metaobject =standard-sheep-metaobject=))
  (if (eql metaobject =standard-sheep-object=)
      (let ((object (make-hash-table :test #'equal)))
	(setf (gethash 'metaobject object) metaobject)
	object)
      (generate-sheep-object-using-metaobject metaobject)))

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

(defvar =dolly= (generate-sheep-object))


