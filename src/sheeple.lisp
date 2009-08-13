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
;; They're also initialized to (gensym) so that pre-bootstrap tests have
;; something to check against that isn't all NIL.
(defvar =standard-sheep= (gensym))
(defvar =t= (gensym))
(defvar =dolly= (gensym))

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
             (= 6 (length (cdr obj))))
    t))

(defun allocate-sheep (metasheep)
  "Allocates the basic skeleton for a sheep object in memory and returns it."
  (if (eql =standard-sheep= metasheep)
      (allocate-std-sheep)
      (allocate-sheep-using-metasheep metasheep)))

(defun sheepp (sheep)
  "Predicate for sheepdom."
  (if (std-sheep-p sheep)
      t
      (and (consp sheep)
           (std-sheep-p (car sheep)))))

;;; Some useful accessors...
(defun sheep-metasheep (sheep)
  "Returns SHEEP's metasheep. For now, there is no support for changing a sheep's metasheep."
  (car sheep))
;; no (setf sheep-metasheep) for now.

(defun sheep-parents (sheep)
  (svref (cdr sheep) 0))
(defun (setf sheep-parents) (new-value sheep)
  (setf (svref (cdr sheep) 0) new-value))

;; Sheep properties are split into two separate data structures:
;; The first is a vector of values, where each entry is (cons 'property-name value)
;; The second is a vector of property metaobjects.
;; Having a local property metaobject means the sheep MUST have a value for that property,
;; but the inverse is not necessarily true.
(defun sheep-pvalue-vector (sheep)
  (svref (cdr sheep) 1))
(defun (setf sheep-pvalue-vector) (new-value sheep)
  (setf (svref (cdr sheep) 1) new-value))

(defun sheep-property-metaobjects (sheep)
  (svref (cdr sheep) 2))
(defun (setf sheep-property-metaobjects) (new-value sheep)
  (setf (svref (cdr sheep) 2) new-value))

(defun sheep-roles (sheep)
  (svref (cdr sheep) 3))
(defun (setf sheep-roles) (new-value sheep)
  (setf (svref (cdr sheep) 3) new-value))

;; Although it makes the sheep objects much heavier, we cache the sheep's hierarchy list,
;; so we don't have to run through the fairly heavy sorting algorithm each time.
(defun %sheep-hierarchy-cache (sheep)
  (svref (cdr sheep) 4))
(defun (setf %sheep-hierarchy-cache) (new-value sheep)
  (setf (svref (cdr sheep) 4) new-value))

;; The current caching scheme also requires us to keep track of any children a sheep has,
;; so changes in the hierarchy can be propagated. This could be probably done in a 
;; much better way...
(defun %sheep-children (sheep)
  (svref (cdr sheep) 5))
(defun (setf %sheep-children) (new-value sheep)
  (setf (svref (cdr sheep) 5) new-value))

;;; children cache
(defun %child-cache-full-p (sheep)
  (when (and (%sheep-children sheep)
             (every (lambda (x) x)
                    (%sheep-children sheep)))
    t))

(defun %adjust-child-cache (sheep)
  (cond ((and (= 5 (length (%sheep-children sheep)))
              (not (adjustable-array-p (%sheep-children sheep))))
         (let ((old-vector (%sheep-children sheep)))
           (setf (%sheep-children sheep)
                 (make-array 100 :adjustable t :initial-element nil))
           (loop 
              for old-entry across old-vector
              for i below (length (%sheep-children sheep))
              do (setf (aref (%sheep-children sheep) i)  old-entry))))
        ((and (<= 100 (length (%sheep-children sheep)))
              (adjustable-array-p (%sheep-children sheep)))
         (adjust-array (%sheep-children sheep)
                       (+ 100 (length (%sheep-children sheep)))
                       :initial-element nil))
        (t (error "Something went wrong with adjusting the array. Weird.")))
  sheep)

(defun %create-child-cache (sheep)
  (setf (%sheep-children sheep)
        (make-array 5 :initial-element nil)))

(defun %add-child (child sheep)
  (unless (%sheep-children sheep)
    (%create-child-cache sheep))
  (when (%child-cache-full-p sheep)
    (%adjust-child-cache sheep))
  (unless (find child (%sheep-children sheep) :key #'weak-pointer-value)
    (let ((entry (make-weak-pointer child)))
      (loop for i below (length (%sheep-children sheep))
         when (null (aref (%sheep-children sheep) i))
         do (setf (aref (%sheep-children sheep) i) entry)
         (return))))
  sheep)

(defun %remove-child (child sheep)
  (when (find child (%sheep-children sheep) :key #'weak-pointer-value)
    (setf (%sheep-children sheep)
          (delete child (%sheep-children sheep) :key #'weak-pointer-value)))
  sheep)

(defun %map-children (function sheep)
  (map 'vector (lambda (pointer)
                 (when (weak-pointer-p pointer)
                  (funcall function (weak-pointer-value pointer))))
       (%sheep-children sheep)))

;;;
;;; Inheritance
;;;
(defun collect-ancestors (sheep)
  "Recursively collects all of SHEEP's ancestors."
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
    (all-parents-loop () (sheep-parents sheep))))

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

(defun compute-sheep-hierarchy-list (sheep)
  (handler-case
      ;; since collect-ancestors only collects the _ancestors_, we cons the sheep in front..
      (let ((sheeple-to-order (cons sheep (collect-ancestors sheep))))
        (topological-sort sheeple-to-order
                          (remove-duplicates
                           (mapappend #'local-precedence-ordering
                                      sheeple-to-order))
                          #'std-tie-breaker-rule))
    (simple-error ()
      (error 'sheeple-hierarchy-error :sheep sheep))))

(defun initialize-children-cache (sheep)
  (setf (%sheep-children sheep)
        (make-weak-hash-table :weakness :key :test #'eq))
  sheep)

(defun memoize-sheep-hierarchy-list (sheep)
  (let ((list (compute-sheep-hierarchy-list sheep)))
    (setf (%sheep-hierarchy-cache sheep)
          list)
    (when (%sheep-children sheep)
      (maphash (lambda (descendant iggy)
                 (declare (ignore iggy))
                 (memoize-sheep-hierarchy-list descendant))
               (%sheep-children sheep)))))

(defun finalize-sheep-inheritance (sheep)
  (if (std-sheep-p sheep)
      (std-finalize-sheep-inheritance sheep)
      (finalize-sheep-inheritance-using-metasheep
       (sheep-metasheep sheep) sheep)))

(defun std-finalize-sheep-inheritance (sheep)
  "we memoize the hierarchy list here."
  (loop for parent in (sheep-parents sheep)
     do (unless (%sheep-children parent)
          (initialize-children-cache parent))
       (setf (gethash sheep (%sheep-children parent)) t))
  (memoize-sheep-hierarchy-list sheep)
  sheep)

;;; Add/remove parents
(defun remove-parent (parent sheep)
  "Remove PARENT as a parent of SHEEP."
  (if (and (std-sheep-p parent)
           (std-sheep-p sheep))
      (std-remove-parent parent sheep)
      (remove-parent-using-metasheeple (sheep-metasheep parent)
                                       (sheep-metasheep sheep)
                                       parent sheep)))

(defun std-remove-parent (parent child)
  "Removing PARENT to SHEEP's parent list is a matter of deleting it from the parent list."
  (if (member parent (sheep-parents child))
      ;; TODO - this could check to make sure that the hierarchy list is still valid.
      (progn
        (setf (sheep-parents child)
              (delete parent (sheep-parents child)))
        (remhash child (%sheep-children parent))
        (finalize-sheep-inheritance child)
        child)
      (error "~A is not a parent of ~A" parent child)))

(defun add-parent (new-parent sheep)
  "Adds NEW-PARENT as a parent to SHEEP."
  (if (and (std-sheep-p new-parent)
           (std-sheep-p sheep))
      (std-add-parent new-parent sheep)
      (add-parent-using-metasheeple (sheep-metasheep new-parent)
                                    (sheep-metasheep sheep)
                                    new-parent sheep)))

(defun std-add-parent (new-parent child)
  "Some basic checking here, and then the parent is actually added to the sheep's list."
  (cond ((equal new-parent child)
         (error "Sheeple cannot be parents of themselves."))
        ((member new-parent (sheep-parents child))
         (error "~A is already a parent of ~A." new-parent child))
        (t
         (handler-case
             (progn
               (push new-parent (sheep-parents child))
               (finalize-sheep-inheritance child)
               child)
           ;; This error is signaled by compute-sheep-hierarchy-list, which right now
           ;; is called from inside finalize-sheep-inheritance (this is probably a bad idea, move
           ;; c-s-h-l in here just to do the check?)
           (sheeple-hierarchy-error ()
             (progn
               (remove-parent new-parent child)
               (error 'sheeple-hierarchy-error
                      :sheep child))))
         child)))

(defun add-parents (parents sheep)
  "Mostly a utility function for easily adding multiple parents. They will be added to
the front of the sheep's parent list in reverse order (so they will basically be appended
to the front of the list)"
  (mapc (lambda (parent) 
          (add-parent parent sheep))
        (reverse parents))
  sheep)

(defun sheep-hierarchy-list (sheep)
  "Returns the full hierarchy-list for SHEEP"
  (%sheep-hierarchy-cache sheep))

;;; Inheritance predicates
(defun parentp (maybe-parent child)
  "A parent is a sheep directly in CHILD's parent list."
  (when (member maybe-parent (sheep-parents child))
    t))

(defun ancestorp (maybe-ancestor descendant)
  "A parent is a sheep somewhere in CHILD's hierarchy list."
  (when (member maybe-ancestor (cdr (sheep-hierarchy-list descendant)))
    t))

(defun childp (maybe-child parent)
  "A child is a sheep that has PARENT in its parent list."
  (parentp parent maybe-child))

(defun descendantp (maybe-descendant ancestor)
  "A descendant is a sheep that has ANCESTOR in its hierarchy-list."
  (ancestorp ancestor maybe-descendant))

;; ;;;
;; ;;; Cloning
;; ;;;
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
