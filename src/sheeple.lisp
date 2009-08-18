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
(defvar =standard-metasheep= (gensym "=STANDARD-METASHEEP="))
(defvar =t= (gensym "=T="))
(defvar =standard-sheep= (gensym "=STANDARD-SHEEP="))
(defvar *bootstrappedp* nil)

;;;
;;; Sheeple object
;;;

(defun standard-metasheep-p (obj)
  "Tests whether OBJ is EQ to =STANDARD-METASHEEP= at run-time."
  (eq obj =standard-metasheep=))

(deftype std-sheep ()
  "A standard sheep is a cons, the CAR of which points to =standard-metasheep=,
and the CDR of which points to a simple-vector with 6 elements, containing the
metaproperties for the sheep.

Put another way, this is the structure of a standard-sheep:

  (=standard-metasheep=
    . #(parents properties property-metaobjects roles hierarchy-list children))"
  '(cons (satisfies standard-metasheep-p) (simple-vector 6)))

(deftype sheep ()
  "A sheep is a cons, the CAR of which points to a metasheep. The contents of the
CDR are dependant upon the metasheep."
  '(or std-sheep (cons std-sheep)))

(defun std-sheep-p (sheep)
  "Internal predicate for sheepdom."
  (typep sheep 'std-sheep))

(defun sheepp (sheep)
  "Predicate for sheepdom."
  (typep sheep 'sheep))

(defun allocate-std-sheep ()
  "Creates a standard sheep object. By default, all the metaproperties are NIL."
  (cons =standard-metasheep= (make-array 6 :initial-element nil)))

(defun print-young-sheep (sheep stream)
  (print-unreadable-object (sheep stream :identity t)
    (format stream "Young Sheep")))

(defmethod print-object ((obj cons) stream)
  (typecase obj
    (std-sheep (print-young-sheep obj stream))
    (otherwise (call-next-method))))

(defun allocate-sheep (metasheep)
  "Allocates the basic skeleton for a sheep object in memory and returns it."
  (if (eql =standard-metasheep= metasheep)
      (allocate-std-sheep)
      (allocate-sheep-using-metasheep metasheep)))

;;; STD-SHEEP accessor definitions
(macrolet ((define-std-sheep-accessor (name place &optional docstring)
             `(progn (defun ,name (sheep)
                       ,@(when (stringp docstring) (list docstring))
                       ,place)
                     (defun (setf ,name) (new-value sheep)
                       (setf ,place new-value)))))

  (define-std-sheep-accessor sheep-metasheep
      (car sheep)
    "SHEEP's metasheep. Do not set this place.")

  (define-std-sheep-accessor sheep-parents
      (svref (cdr sheep) 0)
    "SHEEP's parents.")

  ;; Sheep properties are split into two separate data structures:
  ;; The first is a vector of values, where each entry is (cons 'property-name value)
  ;; The second is a vector of property metaobjects.
  ;; Having a local property metaobject means the sheep MUST have a value for that property,
  ;; but the inverse is not necessarily true.
  (define-std-sheep-accessor sheep-pvalue-vector
      (svref (cdr sheep) 1)
    "SHEEP's property values. A property's existence here doesn't guarantee a corresponging
 metaobject in (sheep-property-metaobjects SHEEP).")

  (define-std-sheep-accessor sheep-property-metaobjects
      (svref (cdr sheep) 2)
    "SHEEP's property metaobjects. A property metaobject is only present when SHEEP's
 metasheep is not =STANDARD-SHEEP=.")

  (define-std-sheep-accessor sheep-roles
      (svref (cdr sheep) 3)
    "SHEEP's roles. These are relevant to reply dispatch.")

  ;; These last two are used internally, for hierarchy caching.

  (define-std-sheep-accessor %sheep-hierarchy-cache
      (svref (cdr sheep) 4)
    "SHEEP's hierarchy cache. Lets us avoid running the heavy sorting algorithm each time.")

  (define-std-sheep-accessor %sheep-children
      (svref (cdr sheep) 5)
    "SHEEP's children cache. Enables propagation of changes in the hierarchy list.")
) ; End STD-SHEEP accessor definitions

;;; children cache
(defun %child-cache-full-p (sheep)
  "A child cache is full if all its items are live weak pointers to other sheep."
  (and (%sheep-children sheep)
       (every #'maybe-weak-pointer-value
              (%sheep-children sheep))))

(defun %adjust-child-cache (sheep)
  "When the child cache gets full, we have to make it bigger. In general, we assume
a 5-slot array will be enough for sheeple that only have a couple of children. Once that
threshold is crossed, though, we assume the worst and replace that relatively small vector
with a massive 100-slot adjustable array. When -that- is full, we'll resize the vector
by 100 each time."
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
        ;; may as well.
        (t (error "Something went wrong with adjusting the array. Weird.")))
  sheep)

(defun %create-child-cache (sheep)
  "This creates only the basic child cache: A simple 5-item vector of NILs.
It sets the vector as SHEEP's child cache."
  (setf (%sheep-children sheep)
        (make-array 5 :initial-element nil)))

(defun %add-child (child sheep)
  "Registers CHILD as a weak pointer in SHEEP's child cache."
  (unless (%sheep-children sheep)
    (%create-child-cache sheep))
  (when (%child-cache-full-p sheep)
    (%adjust-child-cache sheep))
  (unless (find child (%sheep-children sheep) :key #'maybe-weak-pointer-value)
    (let ((children (%sheep-children sheep)))
      (dotimes (i (length children))
        (unless (maybe-weak-pointer-value (aref children i))
          (return (setf (aref children i) (make-weak-pointer child)))))))
  sheep)

(defun %remove-child (child sheep)
  "Takes CHILD out of SHEEP's child cache."
  (when (find child (%sheep-children sheep) :key #'maybe-weak-pointer-value)
    (setf (%sheep-children sheep)
          (delete child (%sheep-children sheep) :key #'maybe-weak-pointer-value)))
  sheep)

(defun %map-children (function sheep)
  "Iteratively applies FUNCTION to SHEEP's children (it takes care of taking each child out
of the weak pointer)."
  (when (%sheep-children sheep)
    (map 'vector (lambda (pointer)
                   (when (weak-pointer-p pointer)
                     (funcall function (weak-pointer-value pointer))))
         (%sheep-children sheep))))

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

;;; <<<<<<< BEGIN OUTDATED CODE BLOCK >>>>>>>
(defun compute-sheep-hierarchy-list-old (sheep)
  (handler-case
      ;; since collect-ancestors only collects the _ancestors_, we cons the sheep in front.
      (let ((sheeple-to-order (cons sheep (collect-ancestors sheep))))
        (topological-sort sheeple-to-order
                          (remove-duplicates
                           (mapappend #'local-precedence-ordering
                                      sheeple-to-order))
                          #'std-tie-breaker-rule))
    (simple-error ()
      (error 'sheeple-hierarchy-error :sheep sheep))))

(defun local-precedence-ordering-old (sheep)
  (mapcar #'list
          (cons sheep
                (butlast (sheep-parents sheep)))
          (sheep-parents sheep)))
;;; <<<<<<< END OUTDATED CODE BLOCK >>>>>>>

(defun local-precedence-ordering (sheep)
  "Calculates the local precedence ordering. Relies on the fact that mapcar will
return when any list is NIL to avoid traversing the entire parent list."
  (let ((parents (sheep-parents sheep)))
    (mapcar #'list (cons sheep parents) parents)))

(defun std-tie-breaker-rule (minimal-elements hl-so-far)
  (dolist (hl-constituent (reverse hl-so-far))
    (let* ((supers (sheep-parents hl-constituent))
           (common (intersection minimal-elements supers)))
      (when (not (null common))
        (return-from std-tie-breaker-rule (car common))))))

(defun compute-sheep-hierarchy-list (sheep)
  (typecase sheep
    (std-sheep (std-compute-sheep-hierarchy-list sheep))
    (otherwise (compute-sheep-hierarchy-list-using-metasheep
                (sheep-metasheep sheep) sheep))))

(defun std-compute-sheep-hierarchy-list (sheep)
  "Because #'local-precedence-ordering returns a fresh list each time, we can
afford to use the destructive #'mapcan and cons less."
  (handler-case
      ;; since collect-ancestors only collects the _ancestors_, we cons the sheep in front.
      (let ((sheeple-to-order (cons sheep (collect-ancestors sheep))))
        (topological-sort sheeple-to-order
                          (remove-duplicates
                           (mapcan #'local-precedence-ordering
                                      sheeple-to-order))
                          #'std-tie-breaker-rule))
    (simple-error ()
      (error 'sheeple-hierarchy-error :sheep sheep))))

(defmacro with-sheep-hierarchy (sheep-and-parents &body body)
  "This macro sets up a sheep hierarchy. The parent-child are expressend in a
 list passed as the first parameter. Each item in the list is either a variable,
 which will be bound to a fresh sheep, or a list of form (var &rest parents), in
 which case VAR will be bound to a fresh sheep with PARENTS as its parents.

As an example, the following call:

  (with-some-sheep-hierarchy (a (b a) (c a) (d b c))
    ...)

Would produce this familiar \"diamond\" hierarchy:

   A
  / \\
 B   C
  \\ /
   D"
  `(let* ,(mapcar #'(lambda (hierarchy-spec)
                      (destructuring-bind (sheep &rest parents)
                          (ensure-list hierarchy-spec)
                        `(,sheep (add-parents (list ,@parents)
                                              (allocate-std-sheep)))))
                  sheep-and-parents)
     ,@body))

(defun memoize-sheep-hierarchy-list (sheep)
  (let ((list (compute-sheep-hierarchy-list sheep)))
    (setf (%sheep-hierarchy-cache sheep)
          list)
    (%map-children (lambda (child)
                     (memoize-sheep-hierarchy-list child))
                   sheep)))

(defun finalize-sheep-inheritance (sheep)
  (typecase sheep
    (std-sheep (std-finalize-sheep-inheritance sheep))
    (otherwise (finalize-sheep-inheritance-using-metasheep
                (sheep-metasheep sheep) sheep))))

(defun std-finalize-sheep-inheritance (sheep)
  "we memoize the hierarchy list here."
  (loop for parent in (sheep-parents sheep)
     do (%add-child sheep parent))
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
        (%remove-child child parent)
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
               (error 'sheeple-hierarchy-error :sheep child))))
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

;;;
;;; Cloning
;;;
(defun ensure-sheep (sheep-or-sheeple &rest all-keys
                    &key (metasheep =standard-metasheep=)
                    &allow-other-keys)
  "Creates a new sheep with SHEEPLE as its parents. METASHEEP is used as the metasheep when
allocating the new sheep object. ALL-KEYS is passed on to INIT-SHEEP."
  (let ((sheep (allocate-sheep metasheep)))
    (if sheep-or-sheeple
        (add-parents (if (listp sheep-or-sheeple)
                         sheep-or-sheeple
                         (list sheep-or-sheeple))
                     sheep)
        (add-parent =standard-sheep= sheep))
     (apply #'init-sheep sheep all-keys)))

(defun clone (&rest sheeple)
  "Creates a new standard-sheep object with SHEEPLE as its parents."
  (ensure-sheep sheeple))

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
  `(let ((sheep (ensure-sheep
                 ,(canonize-sheeple sheeple)
                 :properties ,(canonize-properties properties)
                 ,@(canonize-clone-options options))))
     sheep))

(defmacro defproto (name sheeple properties &rest options)
  `(progn
     (declaim (special ,name))
     (let ((sheep (create-or-reinit-sheep
                   (if (boundp ',name)
                       ,name nil)
                   ,(canonize-sheeple sheeple)
                   :properties ,(canonize-properties properties t)
                   ,@(canonize-clone-options options))))
       (unless (sheep-nickname sheep)
         (setf (sheep-nickname sheep) ',name))
       (setf (symbol-value ',name) sheep))))

(defun create-or-reinit-sheep (maybe-sheep parents &rest options)
  (if maybe-sheep
      (apply #'reinit-sheep maybe-sheep :new-parents parents options)
      (apply #'ensure-sheep parents options)))
