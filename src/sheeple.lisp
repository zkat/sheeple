;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; indent-tabs-mode: nil -*-

;; This file is part of Sheeple

;; sheep-creation.lisp
;;
;; Sheep creation, cloning, inspection
;;
;; TODO - write validate-hierarchy-list, which could be called whenever the parents list changes.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :sheeple)

;; We declare these base vars here so that bootstrapping won't complain.
(defvar =standard-sheep= nil)
(defvar =t= nil)
(defvar =dolly= nil)

;;;
;;; Sheeple object
;;;

;; Sketch of a freshly-consed sheep:
;; (cons =standard-metasheep=)
;;       (vector parents properties property-metaobjects roles hierarchy-list children))
;;
;; By default, everything in the cdr of the sheep is initialized to nil.
(defun allocate-std-sheep ()
  "Creates a standard sheep object."
  (cons =standard-sheep=
        (make-array 6 :initial-element nil)))

(defun std-sheep-p (obj)
  "A standard sheep object is a simple cons that points to the vector where the actual
'metaproperties' live. For a standard sheep object, this vector is 6 elements long, and
everything is initialized to nil."
  (when (and (consp obj)
             (eql =standard-sheep=
                  (car obj))
             (vectorp (cdr obj))
             (= 6 (length (cdr obj)))
             (every #'null (cdr obj)))
    t))

(defun allocate-sheep (metasheep)
  (if (eql =standard-sheep= metasheep)
      (allocate-std-sheep)
      (allocate-sheep-using-metasheep metasheep)))

(defun sheepp (sheep)
  (if (std-sheep-p sheep)
      t
      (and (consp sheep)
           (std-sheep-p (car sheep)))))

;; (defclass standard-sheep ()
;;   (;;; Core slots
;;    (nickname :accessor sheep-nickname :initform nil
;;              :documentation "Displayed nickname")
;;    (documentation :accessor sheep-documentation :initform ""
;;                   :documentation "Docstring for this sheep.")
;;    (parents :accessor sheep-parents :initform nil :initarg :direct-parents
;;             :documentation "Parents of this sheep object.")
;;    (property-value-table :accessor sheep-property-value-table
;;                          :initform (make-hash-table :test #'eq))
;;    (property-metaobject-table :accessor sheep-property-metaobject-table
;;                         :initform (make-hash-table :test #'eq))
;;    (direct-roles :accessor sheep-direct-roles :initform nil)
;;    ;;; Extra slots for backstage tricks.
;;    ;; The reason we keep these references is to alert children recursively whenever the
;;    ;; hierarchy-list changes for a sheep, which means we can memoize the thing.
;;    (%children :accessor %children :initform  (make-weak-hash-table :weakness :key :test #'eq)
;;               :documentation "This is a special slot kept around mostly to make memoization
;;                               easy (possible?) by keeping references to children.")
;;    ;; I think the following would be unnecessary if there was a generator that spit out the
;;    ;; sheep's parents one by one that the implementation could use, instead of either
;;    ;; recalculating the whole thing each time, or saving the whole thing.
;;    (hierarchy-list :accessor sheep-hierarchy-list :initform nil
;;                    :documentation "A pre-calculated hierarchy list, so we don't have to run all
;;                                    of sheep-direct-parents every time.")))

;; (defun allocate-sheep (&optional (class 'standard-sheep))
;;   (make-instance class))

;; (defgeneric sheep-p (obj)
;;   (:method (obj) (declare (ignore obj)) nil)
;;   (:method ((obj standard-sheep)) (declare (ignore obj)) t))

;; ;;;
;; ;;; Cloning
;; ;;;
;; (defgeneric copy-sheep (model)
;;   ;; todo
;;   (:documentation "Makes a direct copy of MODEL."))

;; (defgeneric finalize-sheep (sheep)
;;   (:documentation "Performs any needed finalization on SHEEP."))

;; (defun spawn-sheep (sheep-or-sheeple &rest all-keys
;;                     &key (metaclass 'standard-sheep)
;;                     &allow-other-keys)
;;   "Creates a new sheep with SHEEPLE as its parents. METACLASS is used as the class when instantiating
;; the new sheep object. ALL-KEYS is passed on to INIT-SHEEP."
;;   (let ((sheep (allocate-sheep metaclass)))
;;     (if sheep-or-sheeple
;;         (add-parents (if (listp sheep-or-sheeple)
;;                          sheep-or-sheeple
;;                          (list sheep-or-sheeple))
;;                      sheep)
;;         (add-parent =dolly= sheep))
;;     (apply #'init-sheep sheep all-keys)))

;; (defun clone (&rest sheeple)
;;   "Creates a new standard-sheep object with SHEEPLE as its parents."
;;   (spawn-sheep sheeple))

;; (defmethod finalize-sheep ((sheep standard-sheep))
;;   "we memoize the hierarchy list here."
;;   (loop for parent in (sheep-parents sheep)
;;      do (setf (gethash sheep (%children parent)) t))
;;   (memoize-sheep-hierarchy-list sheep)
;;   sheep)

;; ;;; Inheritance setup
;; (defgeneric add-parent (new-parent sheep)
;;   (:documentation "Adds NEW-PARENT as a parent to SHEEP."))

;; (defmethod add-parent (unsheepish-parent (child standard-sheep))
;;   "If the given parent isn't already a sheep object, we box it before handing it down to
;; the real add-parent."
;;   (add-parent (sheepify unsheepish-parent) child))

;; (defmethod add-parent ((new-parent standard-sheep) (child standard-sheep))
;;   "Some basic checking here, and then the parent is actually added to the sheep's list."
;;   (cond ((equal new-parent child)
;;          (error "Sheeple cannot be parents of themselves."))
;;         ((member new-parent (sheep-parents child))
;;          (error "~A is already a parent of ~A." new-parent child))
;;         (t
;;          (handler-case
;;              (progn
;;                (push new-parent (sheep-parents child))
;;                (setf (gethash child (%children new-parent)) t)
;;                (finalize-sheep child)
;;                child)
;;            ;; This error is signaled by compute-sheep-hierarchy-list, which right now
;;            ;; is called from inside finalize-sheep (this is probably a bad idea, move
;;            ;; c-s-h-l in here just to do the check?)
;;            (sheep-hierarchy-error ()
;;              (progn
;;                (setf (sheep-parents child)
;;                      (delete new-parent
;;                              (sheep-parents child)))
;;                (finalize-sheep child)
;;                (error 'sheep-hierarchy-error
;;                       :format-control "A circular precedence graph was generated for ~A"
;;                       :format-args (list child)))))
;;          child)))

;; (defun add-parents (parents sheep)
;;   "Mostly a utility function for easily adding multiple parents. They will be added to
;; the front of the sheep's parent list in reverse order (so they will basically be appended
;; to the front of the list)"
;;   (mapc (lambda (parent) 
;;           (add-parent parent sheep))
;;         (reverse parents))
;;   sheep)

;; (defgeneric remove-parent (parent sheep)
;;   (:documentation "Remove PARENT as a parent of SHEEP."))

;; (defmethod remove-parent (unsheepish-parent (child standard-sheep))
;;   "As with add-parent, we make sure to box the parent first if it's not already a standard-sheep."
;;   (remove-parent (sheepify unsheepish-parent) child))

;; (defmethod remove-parent ((parent standard-sheep) (child standard-sheep))
;;   "Removing PARENT to SHEEP's parent list is a matter of deleting it from the parent list."
;;   (if (member parent (sheep-parents child))
;;       ;; TODO - this could check to make sure that the hierarchy list is still valid.
;;       (progn
;;         (setf (sheep-parents child)
;;               (delete parent (sheep-parents child)))
;;         (remhash child (%children parent))
;;         (finalize-sheep child)
;;         child)
;;       (error "~A is not a parent of ~A" parent child)))

;; ;;; Inheritance predicates
;; (defun parent-p (maybe-parent child)
;;   "A parent is a sheep directly in CHILD's parent list."
;;   (when (member maybe-parent (sheep-parents child))
;;     t))

;; (defun ancestor-p (maybe-ancestor descendant)
;;   "A parent is a sheep somewhere in CHILD's hierarchy list."
;;   (when (member maybe-ancestor (cdr (sheep-hierarchy-list descendant)))
;;     t))

;; (defun child-p (maybe-child parent)
;;   "A child is a sheep that has PARENT in its parent list."
;;   (parent-p parent maybe-child))

;; (defun descendant-p (maybe-descendant ancestor)
;;   "A descendant is a sheep that has ANCESTOR in its hierarchy-list."
;;   (ancestor-p ancestor maybe-descendant))

;; ;;;
;; ;;; Hierarchy Resolution
;; ;;;
;; ;;; Most of this is taken almost verbatim from closette
;; ;;;
;; (defun collect-parents (sheep)
;;   (labels ((all-parents-loop (seen parents)
;;               (let ((to-be-processed
;;                      (set-difference parents seen)))
;;                 (if (null to-be-processed)
;;                     parents
;;                     (let ((sheep-to-process
;;                            (car to-be-processed)))
;;                       (all-parents-loop
;;                        (cons sheep-to-process seen)
;;                        (union (sheep-parents sheep-to-process)
;;                               parents)))))))
;;     (all-parents-loop () (list sheep))))

;; (defun compute-sheep-hierarchy-list (sheep)
;;   (handler-case
;;     (let ((sheeple-to-order (collect-parents sheep)))
;;       (topological-sort sheeple-to-order
;;                         (remove-duplicates
;;                          (mapappend #'local-precedence-ordering
;;                                     sheeple-to-order))
;;                         #'std-tie-breaker-rule))
;;     (simple-error ()
;;                   (error 'sheep-hierarchy-error
;;                          :format-control "A circular precedence graph was generated for ~A"
;;                          :format-args (list sheep)))))

;; (defun local-precedence-ordering (sheep)
;;   (mapcar #'list
;;           (cons sheep
;;                 (butlast (sheep-parents sheep)))
;;           (sheep-parents sheep)))

;; (defun std-tie-breaker-rule (minimal-elements hl-so-far)
;;   (dolist (hl-constituent (reverse hl-so-far))
;;     (let* ((supers (sheep-parents hl-constituent))
;;            (common (intersection minimal-elements supers)))
;;       (when (not (null common))
;;         (return-from std-tie-breaker-rule (car common))))))

;; ;;;
;; ;;; Memoization
;; ;;;
;; (defun memoize-sheep-hierarchy-list (sheep)
;;   (let ((list (compute-sheep-hierarchy-list sheep)))
;;     (setf (sheep-hierarchy-list sheep)
;;           list)
;;     (maphash (lambda (descendant iggy)
;;                (declare (ignore iggy))
;;                (memoize-sheep-hierarchy-list descendant))
;;              (%children sheep))))

;; ;;;
;; ;;; DEFCLONE macro
;; ;;;
;; (defun canonize-sheeple (sheeple)
;;   `(list ,@sheeple))

;; (defun canonize-properties (properties &optional (accessors-by-default nil))
;;   `(list ,@(mapcar (lambda (p)
;;                      (canonize-property p accessors-by-default))
;;                    properties)))

;; (defun canonize-property (property &optional (accessors-by-default nil))
;;   (let ((name (car property)) (value (cadr property)) (readers nil)
;;         (writers nil) (other-options nil) (no-reader-p nil) (no-writer-p nil))
;;     (do ((olist (cddr property) (cddr olist)))
;;         ((null olist))
;;       (case (car olist)
;;         (:reader
;;          (cond (no-reader-p
;;                 (error "You said you didn't want a reader, but now you want one? Make up your mind."))
;;                ((null (cadr olist))
;;                 (if (null readers) (setf no-reader-p t)
;;                     (error "You already defined a reader, but now you say you don't want one? Make up your mind.")))
;;                (t (pushnew (cadr olist) readers))))
;;         (:writer
;;          (cond (no-writer-p
;;                 (error "You said you didn't want a writer, but now you want one? Make up your mind."))
;;                ((null (cadr olist))
;;                 (if (null writers) (setf no-writer-p t)
;;                     (error "You already defined a writer, but now you say you don't want one? Make up your mind.")))
;;                (t (pushnew (cadr olist) writers))))
;;         ((:manipulator :accessor)
;;          (cond ((or no-reader-p no-writer-p)
;;                 (error "You said you didn't want a reader or a writer, but now you want one? Make up your mind."))
;;                ((null (cadr olist))
;;                 (if (and (null writers) (null readers))
;;                     (progn
;;                       (setf no-reader-p t)
;;                       (setf no-writer-p t))
;;                     (error "You already defined a reader or writer, but now you say you don't want any of them? Make up your mind.")))
;;                (t
;;                 (pushnew (cadr olist) readers)
;;                 (pushnew `(setf ,(cadr olist)) writers))))
;;         (otherwise
;;          (pushnew (cadr olist) other-options)
;;          (pushnew (car olist) other-options))))
;;     (when accessors-by-default
;;       (unless (or readers no-reader-p)
;;         (pushnew name readers))
;;       (unless (or writers no-writer-p)
;;         (pushnew `(setf ,name) writers)))
;;     `(list ',name ,value
;;            ,@(when readers `(:readers ',readers))
;;            ,@(when writers `(:writers ',writers))
;;            ,@other-options)))

;; (defun canonize-clone-options (options)
;;   (mapappend #'canonize-clone-option options))

;; (defun canonize-clone-option (option)
;;   (list `,(car option) (cadr option)))

;; (defmacro defclone (sheeple properties &rest options)
;;   "Standard sheep-generation macro. This variant auto-generates accessors."
;;   `(let ((sheep (spawn-sheep
;;                  ,(canonize-sheeple sheeple)
;;                  :properties ,(canonize-properties properties)
;;                  ,@(canonize-clone-options options))))
;;      sheep))

;; (defmacro defproto (name sheeple properties &rest options)
;;   `(progn
;;      (declaim (special ,name))
;;      (let ((sheep (spawn-or-reinit-sheep
;;                    (if (boundp ',name)
;;                        ,name nil)
;;                    ,(canonize-sheeple sheeple)
;;                    :properties ,(canonize-properties properties t)
;;                    ,@(canonize-clone-options options))))
;;        (unless (sheep-nickname sheep)
;;          (setf (sheep-nickname sheep) ',name))
;;        (setf (symbol-value ',name) sheep))))

;; (defun spawn-or-reinit-sheep (maybe-sheep parents &rest options)
;;   (if maybe-sheep
;;       (apply #'reinit-sheep maybe-sheep :new-parents parents options)
;;       (apply #'spawn-sheep parents options)))
