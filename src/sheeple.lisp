;; This file is part of Sheeple

;; sheep-creation.lisp
;;
;; Sheep creation, cloning, inspection
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :sheeple)
(declaim (optimize (debug 1) (safety 1) (speed 3)))

(defvar *max-sheep-id* 0)
(defvar =t=)
(defvar =dolly=)
(defclass standard-sheep ()
  ((nickname :accessor sheep-nickname :initform nil)
   (documentation :accessor sheep-documentation :initform "")
   (direct-parents :accessor sheep-parents :initform nil :initarg :direct-parents)
   (%direct-children :accessor %direct-children 
                     :initform  (make-weak-hash-table :weakness :key :test #'eq))
   (property-value-table :accessor sheep-property-value-table
                         :initform (make-hash-table :test #'eq))
   (property-spec-table :accessor sheep-property-spec-table
                        :initform (make-hash-table :test #'eq))
   (direct-roles :accessor sheep-direct-roles :initform nil)
   (hierarchy-list :accessor sheep-hierarchy-list :initform nil)
   (id :accessor sheep-id :initform (incf *max-sheep-id*))))

(defun allocate-sheep (&optional (class 'standard-sheep))
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
(defun spawn-sheep (sheeple &rest all-keys
		    &key (metaclass 'standard-sheep)
		    &allow-other-keys)
  "Creates a new sheep with SHEEPLE as its parents. METACLASS is used as the class when instantiating
the new sheep object. ALL-KEYS is passed on to INIT-SHEEP."
  (let ((sheep (allocate-sheep metaclass)))
    (if sheeple
        (add-parents sheeple sheep)
        (add-parent =dolly= sheep))
    (apply #'init-sheep sheep all-keys)))

(defun clone (&rest sheeple)
  "Creates a new standard-sheep object with SHEEPLE as its parents."
  (spawn-sheep sheeple))

(defgeneric copy-sheep (model))
(defmethod copy-sheep ((model standard-sheep))
  ;; TODO - this is bad.
  (let* ((parents (sheep-parents model))
         (properties (sheep-property-value-table model))
         (roles (sheep-direct-roles model))
         (new-sheep (clone parents)))
    (setf (sheep-property-value-table new-sheep)
          properties)
    (setf (sheep-direct-roles new-sheep)
          roles)
    new-sheep))

(defgeneric finalize-sheep (sheep))
(defmethod finalize-sheep ((sheep standard-sheep))
  (loop for parent in (sheep-parents sheep)
     do (setf (gethash sheep (%direct-children parent)) t))
  (memoize-sheep-hierarchy-list sheep)
  sheep)

;;; Inheritance setup
(defgeneric add-parent (new-parent sheep))
(defmethod add-parent (unsheepish-parent (child standard-sheep))
  (add-parent (sheepify unsheepish-parent) child))
(defmethod add-parent ((new-parent standard-sheep) (child standard-sheep))
  (cond ((equal new-parent child)
         (error "Sheeple cannot be parents of themselves."))
        ((member new-parent (sheep-parents child))
         (error "~A is already a parent of ~A." new-parent child))
        (t
         (handler-case
             (progn
               (push new-parent (sheep-parents child))
               (setf (gethash child (%direct-children new-parent)) t)
               (finalize-sheep child)
               child)
           (sheep-hierarchy-error ()
             (progn
               (setf (sheep-parents child) 
                     (delete new-parent
                             (sheep-parents child)))
               (finalize-sheep child)
               (error 'sheep-hierarchy-error
                         :format-control "A circular precedence graph was generated for ~A"
                         :format-args (list child)))))
         child)))

(defun add-parents (parents sheep)
  (loop for parent in (reverse parents) do (add-parent parent sheep))
  sheep)

(defgeneric remove-parent (parent sheep))
(defmethod remove-parent (unsheepish-parent (child standard-sheep))
  (remove-parent (sheepify unsheepish-parent) child))
(defmethod remove-parent ((parent standard-sheep) (child standard-sheep))
  (if (member parent (sheep-parents child))
      (progn
        (setf (sheep-parents child)
              (delete parent (sheep-parents child)))
        (remhash child (%direct-children parent))
        (finalize-sheep child)
        child)
      (error "~A is not a parent of ~A" parent child)))

;;; Inheritance predicates
(defun parent-p (maybe-parent child)
  (when (member maybe-parent (sheep-parents child))
    t))

(defun ancestor-p (maybe-ancestor descendant)
  (when (and (not (eql maybe-ancestor descendant))
             (member maybe-ancestor (collect-parents descendant)))
    t))

(defun child-p (maybe-child parent)
  (parent-p parent maybe-child))

(defun descendant-p (maybe-descendant ancestor)
  (ancestor-p ancestor maybe-descendant))

;;;
;;; Hierarchy Resolution
;;;
;;; Most of this is taken almost verbatim from closette
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
                       (union (sheep-parents sheep-to-process)
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
                  (error 'sheep-hierarchy-error
                         :format-control "A circular precedence graph was generated for ~A"
                         :format-args (list sheep)))))

(defun local-precedence-ordering (sheep)
  (mapcar #'list
          (cons sheep
                (butlast (sheep-parents sheep)))
          (sheep-parents sheep)))

(defun std-tie-breaker-rule (minimal-elements hl-so-far)
  (dolist (hl-constituent (reverse hl-so-far))
    (let* ((supers (sheep-parents hl-constituent))
           (common (intersection minimal-elements supers)))
      (when (not (null common))
        (return-from std-tie-breaker-rule (car common))))))

;;;
;;; Memoization
;;;
(defun memoize-sheep-hierarchy-list (sheep)
  (let ((list (compute-sheep-hierarchy-list sheep)))
    (setf (sheep-hierarchy-list sheep)
          list)
    (maphash (lambda (descendant iggy) 
               (declare (ignore iggy))
               (memoize-sheep-hierarchy-list descendant))
             (%direct-children sheep))))

;;;
;;; DEFCLONE macro
;;;
(defun canonize-sheeple (sheeple)
  `(list ,@sheeple))

(defun canonize-properties (properties &optional (accessors-by-default nil))
  `(list ,@(mapcar (lambda (p)
                     (canonize-property p accessors-by-default))
                   properties)))

(defun canonize-property (property &optional (accessors-by-default nil))
  (let ((name (car property)) (value (cadr property)) (readers nil)
        (writers nil) (other-options nil) (no-reader-p nil) (no-writer-p nil))
    (do ((olist (cddr property) (cddr olist)))
        ((null olist))
      (case (car olist)
        (:reader
         (cond (no-reader-p
                (error "You said you didn't want a reader, but now you want one? Make up your mind."))
               ((null (cadr olist))
                (if (null readers) (setf no-reader-p t)
                    (error "You already defined a reader, but now you say you don't want one? Make up your mind.")))
               (t (pushnew (cadr olist) readers))))
        (:writer
         (cond (no-writer-p
                (error "You said you didn't want a writer, but now you want one? Make up your mind."))
               ((null (cadr olist))
                (if (null writers) (setf no-writer-p t)
                    (error "You already defined a writer, but now you say you don't want one? Make up your mind.")))
               (t (pushnew (cadr olist) writers))))
        ((:manipulator :accessor)
         (cond ((or no-reader-p no-writer-p)
                (error "You said you didn't want a reader or a writer, but now you want one? Make up your mind."))
               ((null (cadr olist))
                (if (and (null writers) (null readers))
                    (progn
                      (setf no-reader-p t)
                      (setf no-writer-p t))
                    (error "You already defined a reader or writer, but now you say you don't want any of them? Make up your mind.")))
               (t
                (pushnew (cadr olist) readers)
                (pushnew `(setf ,(cadr olist)) writers))))
        (otherwise 
         (pushnew (cadr olist) other-options)
         (pushnew (car olist) other-options))))
    (when accessors-by-default
      (unless (or readers no-reader-p)
        (pushnew name readers))
      (unless (or writers no-writer-p)
        (pushnew `(setf ,name) writers)))
    `(list ',name ,value
           ,@(when readers `(:readers ',readers))
           ,@(when writers `(:writers ',writers))
           ,@other-options)))

(defun canonize-clone-options (options)
  (mapappend #'canonize-clone-option options))

(defun canonize-clone-option (option)
  (list `,(car option) (cadr option)))

(defmacro defclone (sheeple properties &rest options)
  "Standard sheep-generation macro. This variant auto-generates accessors."
  `(let ((sheep (spawn-sheep
                 ,(canonize-sheeple sheeple)
		 :properties ,(canonize-properties properties)
                 ,@(canonize-clone-options options))))
     sheep))

(defmacro defproto (name sheeple properties &rest options)
  `(progn
     (declaim (special ,name))
     (let ((sheep (spawn-or-reinit-sheep
                   (if (boundp ',name)
                       ,name nil)
                   ,(canonize-sheeple sheeple)
                   :properties ,(canonize-properties properties t)
                   ,@(canonize-clone-options options))))
       (unless (sheep-nickname sheep)
         (setf (sheep-nickname sheep) ',name))
       (setf (symbol-value ',name) sheep))))

(defun spawn-or-reinit-sheep (maybe-sheep parents &rest options)
  (if maybe-sheep
      (apply #'reinit-sheep maybe-sheep :new-parents parents options)
      (apply #'spawn-sheep parents options)))