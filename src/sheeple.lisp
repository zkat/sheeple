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
   (direct-parents :accessor sheep-direct-parents :initform nil :initarg :direct-parents)
   (%direct-children :accessor %direct-children 
                     :initform  (make-weak-hash-table :weakness :key :test #'eq))
   (property-value-table :accessor sheep-property-value-table
                         :initform (make-hash-table :test #'eq))
   (readers :accessor %property-readers :initform (make-hash-table :test #'eq))
   (writers :accessor %property-writers :initform (make-hash-table :test #'eq))
   (direct-roles :accessor sheep-direct-roles :initform nil)
   (hierarchy-list :accessor sheep-hierarchy-list)
   (id :accessor sheep-id :initform (incf *max-sheep-id*))))

;;; How to build a full sheep object:
;;; 1. Allocate an instance of its metaclass
;;; 2. Add its direct parents
;;; --- Can now define and run replies on this object, since it has a hierarchy list.
;;; 3. Add any properties we want to it.
;;; 4. Define any replies, including accessors.
;;; 5. Free to go
(defmethod initialize-instance :after ((sheep standard-sheep) &key &allow-other-keys))

(defun allocate-sheep (class)
  (make-instance class))

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
(defun spawn-sheep (sheeple &rest all-keys &key (metaclass 'standard-sheep) &allow-other-keys)
  "Creates a new sheep with SHEEPLE as its parents, and PROPERTIES as its properties"
  (let ((sheep (allocate-sheep metaclass)))
    (if sheeple
        (loop for parent in sheeple do (add-parent parent sheep))
        (add-parent (find-sheep 'dolly) sheep))
    (apply #'initialize-sheep sheep all-keys)))

(defun clone (&rest sheeple)
  (spawn-sheep sheeple))

(defun copy-sheep (model)
  (let* ((parents (sheep-direct-parents model))
         (properties (sheep-property-value-table model))
         (roles (sheep-direct-roles model))
         (new-sheep (clone parents)))
    (setf (sheep-property-value-table new-sheep)
          properties)
    (setf (sheep-direct-roles new-sheep)
          roles)
    new-sheep))

(defun set-up-properties (sheep properties)
  (mapc (lambda (plist)
          (apply #'add-property sheep plist))
        properties))

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
(defgeneric add-parent (new-parent sheep))
(defmethod add-parent ((new-parent standard-sheep) (child standard-sheep))
  (cond ((equal new-parent child)
         (error "Sheeple cannot be parents of themselves."))
        (t
         (handler-case
             (progn
               (pushnew new-parent (sheep-direct-parents child))
               (setf (gethash child (%direct-children new-parent)) t)
               child)
           (sheep-hierarchy-error ()
             (progn
               (setf (sheep-direct-parents child) 
                     (delete new-parent
                             (sheep-direct-parents child)))
               (error 'sheep-hierarchy-error))))
         (finalize-sheep child)
         child)))

(defgeneric remove-parent (parent sheep))
(defmethod remove-parent ((parent standard-sheep) (child standard-sheep))
  (setf (sheep-direct-parents child)
        (delete parent (sheep-direct-parents child)))
  (remhash child (%direct-children parent))
  (finalize-sheep child)
  child)

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

;; Memoization
(defun memoize-sheep-hierarchy-list (sheep)
  (let ((list (compute-sheep-hierarchy-list sheep)))
    (setf (sheep-hierarchy-list sheep)
          list)
    (maphash (lambda (descendant iggy) 
               (declare (ignore iggy))
               (memoize-sheep-hierarchy-list descendant))
             (%direct-children sheep))))

