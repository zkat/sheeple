;; This file is part of Sheeple

;; sheep-creation.lisp
;;
;; Sheep creation, cloning, inspection
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :sheeple)

(declaim (optimize (debug 1) (safety 1) (speed 3)))

(defvar *max-sheep-id* 0)
(defclass standard-sheep ()
  ((nickname :accessor sheep-nickname :initform nil)
   (documentation :accessor sheep-documentation :initform "")
   (direct-parents :accessor sheep-direct-parents :initform nil)
   (%direct-children :accessor %direct-children 
                     :initform  (make-weak-hash-table :weakness :key :test #'eq))
   (property-value-table :accessor sheep-property-value-table
                         :initform (make-hash-table :test #'eq))
   #+nil(property-owners :accessor sheep-property-owners
		    :initform (make-weak-hash-table :weakness :value :test #'eq))
   (readers :accessor %property-readers :initform (make-hash-table :test #'eq))
   (writers :accessor %property-writers :initform (make-hash-table :test #'eq))
   (direct-roles :accessor sheep-direct-roles :initform nil)
   (clonefunctions :accessor sheep-clonefunctions :initform (make-hash-table :test #'eq))
   (cloneforms :accessor sheep-cloneforms :initform (make-hash-table :test #'eq))
   (hierarchy-list :accessor sheep-hierarchy-list)
   (id :accessor sheep-id :initform (incf *max-sheep-id*))))

(defmethod initialize-instance :after ((sheep standard-sheep) &key &allow-other-keys))

(defun allocate-sheep (class &rest all-keys)
  (apply #'make-instance class all-keys))

(defgeneric sheep-p (obj))
(defmethod sheep-p (obj)
  (declare (ignore obj))
  nil)
(defmethod sheep-p ((obj standard-sheep))
  (declare (ignore obj))
  t)

;;;
;;; Cloning
;;;

;;; FIXME: SHIT SON... this only copies references. I'm an idort.
(defun mitosis (model)
  (let* ((parents (sheep-direct-parents model))
         (properties (sheep-property-value-table model))
         (roles (sheep-direct-roles model))
         (clonefuns (sheep-clonefunctions model))
         (cloneforms (sheep-cloneforms model))
         (new-sheep (clone () ())))
    (setf (sheep-direct-parents new-sheep)
          parents)
    (setf (sheep-property-value-table new-sheep)
          properties)
    (setf (sheep-direct-roles new-sheep)
          roles)
    (setf (sheep-clonefunctions new-sheep)
          clonefuns)
    (setf (sheep-cloneforms new-sheep)
          cloneforms)
    new-sheep))

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
       do (unless (or (eq fn *secret-unbound-value*)
		      (has-direct-property-p sheep propname))
	    (setf (property-value sheep propname) (funcall (the function fn)))))))

(defgeneric finalize-sheep (sheep))
(defmethod finalize-sheep ((sheep standard-sheep))
  (loop for parent in (sheep-direct-parents sheep)
     do (setf (gethash sheep (%direct-children parent)) t))
  (memoize-sheep-hierarchy-list sheep)
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
           (sheep-property-value-table parent)))
        (sheep-direct-parents sheep)))

;;; Inheritance setup
(defun add-parent (new-parent child)
  (cond ((equal new-parent child)
         (error "Can't inherit from self."))
        (t
         (handler-case
           (progn
             (pushnew new-parent (sheep-direct-parents child))
             (setf (gethash child (%direct-children new-parent)) t)
             child)
           (sheep-hierarchy-error ()
                                  (progn
                                    (setf (sheep-direct-parents child) (delete new-parent
                                                                               (sheep-direct-parents child)))
                                    (error 'sheep-hierarchy-error))))
         (finalize-sheep child)
         child)))

(defun remove-parent (parent child &key (keep-properties nil))
  (setf (sheep-direct-parents child)
        (delete parent (sheep-direct-parents child)))
  (remhash child (%direct-children parent))
  (when keep-properties
    (loop for property-name being the hash-keys of (sheep-property-value-table parent)
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

(defun std-tie-breaker-rule (minimal-elements hl-so-far)
  (dolist (hl-constituent (reverse hl-so-far))
    (let* ((supers (sheep-direct-parents hl-constituent))
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
    (maphash (lambda (descendant iggy) 
               (declare (ignore iggy))
               (memoize-sheep-hierarchy-list descendant))
             (%direct-children sheep))))

;; MOP
(defclass property-spec ()
  ((name :initarg :name :accessor property-spec-name)
   (value :initarg :value :accessor property-spec-value)
   (readers :initform nil :initarg :readers :accessor property-spec-readers)
   (writers :initform nil :initarg :writers :accessor property-spec-writers)))

(defmethod print-object ((property-spec property-spec) stream)
  (print-unreadable-object (property-spec stream :identity t)
    (format stream "Property-Spec ~~ Name: ~A" (property-spec-name property-spec))))
(defun sheep-direct-properties (sheep)
  "Returns a set of direct property-spec definition metaobjects."
  (loop for pname being the hash-keys of (sheep-property-value-table sheep)
     using (hash-value pvalue)
     collect (make-instance 'property-spec
                            :name pname
                            :value pvalue
                            :readers 
                            (loop for reader-name in (gethash pname (%property-readers sheep))
                                 collect reader-name)
                            :writers 
                            (loop for writer-name in (gethash pname (%property-writers sheep))
                               collect writer-name))))

