;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; indent-tabs-mode: nil -*-

;;;; This file is part of Sheeple

;;;; sheeple.lisp
;;;;
;;;; Sheep creation, cloning, inspection
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :sheeple)

;; We declare these base vars here so that bootstrapping won't complain.
(define-bound-variables =t= =standard-sheep= =standard-metasheep=)

(defvar *bootstrappedp* nil)

;;;
;;; Sheeple object
;;;
(defun %std-sheep-p (obj)
  "Tests whether OBJ is EQ to =STANDARD-METASHEEP= at run-time."
  (eq (svref obj 0) =standard-metasheep=))

(deftype std-sheep ()
  "A standard sheep object is a 6-slot simple vector where the 0th element
is =standard-metasheep=."
  '(and (simple-vector 6) (satisfies %std-sheep-p)))

(deftype sheep ()
  "A sheep is a 6-slot simple vector where the 0th element is =standard-metasheep= or one
of its descendants."
  '(satisfies sheepp))

(defun std-sheep-p (sheep)
  "Internal predicate for sheepdom."
  (typep sheep 'std-sheep))

(defun sheepp (sheep)
  "Predicate for sheepdom."
  (or (typep sheep 'std-sheep)
      (and (typep sheep '(simple-vector 6))
           (typep (elt sheep 0) 'std-sheep))))

;;; The basics of printing sheep
(defun verify-print-settings ()
  (assert (or *print-pretty* *print-circle*)
          (*print-pretty* *print-circle*)
          (format nil "It is impossible to print sheep when both *PRINT-PRETTY* ~
                       and *PRINT-CIRCLE* are~%disabled. Please enable at least ~
                       one of them, and try again.~%Unless you are hacking ~
                       Sheeple internals, it is highly recommended that you~@
                       enable pretty-printing."))
  (unless *print-pretty*
    (warn "Pretty-printing is disabled. Sheep objects will be printed raw.")))

;;; This form currently overrides the previous print settings. We should decide
;;; whether we want to take this approach, or just get people to stick a form
;;; in their Lisp's init file.
(handler-case (verify-print-settings)
  (condition ()
    (setf *print-pretty* t
          *print-circle* t)
    (verify-print-settings)))

(defun print-young-sheep (stream sheep)
  (print-unreadable-object (sheep stream :identity t)
    (format stream "Young Sheep")))

(set-pprint-dispatch 'sheep 'print-young-sheep 0.1)

;;; The basics of allocating sheep objects
(defun std-allocate-sheep (metasheep)
  "Creates a standard sheep object. By default, all the metaproperties are NIL."
  (let ((array (make-vector 6)))
    (setf (svref array 0) metasheep)
    array))

(defun maybe-std-allocate-sheep (metasheep)
  (if (eq =standard-metasheep= metasheep)
      (std-allocate-sheep metasheep)
      (allocate-sheep metasheep)))

;;; STD-SHEEP accessor definitions
(defmacro define-internal-accessors (&body names-and-indexes)
  `(progn ,@(loop for (name index) on names-and-indexes by #'cddr
               collect `(progn (defun ,name (sheep) (svref sheep ,index))
                               (defun (setf ,name) (new-value sheep)
                                 (setf (svref sheep ,index) new-value))))))

(define-internal-accessors %sheep-metasheep 0
                           %sheep-parents 1 ; actual parents
                           %sheep-direct-properties 2 ; direct property vector
                           %sheep-roles 3 ; direct roles
                           ;; These last two are used internally, for hierarchy caching.
                           %sheep-hierarchy-cache 4
                           %sheep-children 5)

;; If we didn't define these functions, Lisp's package system would
;; export the SETF version as well as the reader.
(defun sheep-metasheep (sheep)
  (%sheep-metasheep sheep))

(defun sheep-parents (sheep)
  (declare (inline %sheep-parents))
  (%sheep-parents sheep))

;;; children cache
(defvar *child-cache-initial-size* 5
  "The initial size for a sheep's child cache.")

(defvar *child-cache-grow-ratio* 5
  "The ratio by which the child-cache is expanded when full.")

(symbol-macrolet ((%children (%sheep-children sheep)))

  (defun %create-child-cache (sheep)
    "Sets SHEEP's child cache to a blank (simple-vector `*child-cache-initial-size*')"
    (setf %children (make-vector *child-cache-initial-size*)))

  (defun %child-cache-full-p (sheep)
    "A child cache is full if all its items are live weak pointers to other sheep."
    (aand %children (every 'maybe-weak-pointer-value it)))

  (defun %enlarge-child-cache (sheep)
    "Enlarges SHEEP's child cache by the value of `*child-cache-grow-ratio*'."
    (let* ((old-vector (%sheep-children sheep))
           (new-vector (make-vector (* *child-cache-grow-ratio* (length old-vector)))))
      (setf (%sheep-children sheep) (replace new-vector old-vector))
      sheep))

  (defun %add-child (child sheep)
    "Registers CHILD in SHEEP's child cache."
    (let ((children %children))
      (if children
          (when (%child-cache-full-p sheep)
            (%enlarge-child-cache sheep)
            (setf children %children))
          (progn (%create-child-cache sheep)
                 (setf children %children)))
      (unless (find child children :key 'maybe-weak-pointer-value)
        (dotimes (i (length children))
          (unless (maybe-weak-pointer-value (aref children i))
            (return (setf (aref children i) (make-weak-pointer child)))))))
    sheep)

  (defun %remove-child (child sheep)
    "Takes CHILD out of SHEEP's child cache."
    (awhen (position child %children :key 'maybe-weak-pointer-value)
      (setf (svref %children it) nil))
    sheep)

  (defun %map-children (function sheep)
    "Applies FUNCTION to each of SHEEP's children."
    (awhen %children
      (map nil (fun (awhen (maybe-weak-pointer-value _) (funcall function it))) it))))

;;; This utility is useful for concisely setting up sheep hierarchies
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
  `(let* ,(mapcar (fun (destructuring-bind (sheep &rest parents)
                           (ensure-list _)
                         `(,sheep (add-parents (list ,@parents)
                                               (std-allocate-sheep =standard-metasheep=)))))
                  sheep-and-parents)
     ,@body))

;;;
;;; Inheritance
;;;

(defun topological-sort (elements constraints tie-breaker)
  "Sorts ELEMENTS such that they satisfy the CONSTRAINTS, falling back
on the TIE-BREAKER in the case of ambiguous constraints. On the assumption
that they are freshly generated, this implementation is destructive with
regards to the CONSTRAINTS. A future version will undo this change."
  (loop
     :for minimal-elements := (remove-if (fun (member _ constraints :key 'cadr)) elements)
     :while minimal-elements
     :for choice := (if (null (cdr minimal-elements))
                        (car minimal-elements)
                        (funcall tie-breaker minimal-elements result))
     :collect choice :into result
     :do (deletef constraints choice :test 'member)
         (setf elements (remove choice elements))
     :finally (if (null elements)
                  (return-from topological-sort result)
                  (error "Inconsistent precedence graph."))))

(defun collect-ancestors (sheep)
  "Recursively collects all of SHEEP's ancestors."
  (labels ((all-parents-loop (seen parents)
             (let ((to-be-processed (set-difference parents seen)))
               (if (null to-be-processed)
                   parents
                   (let ((sheep-to-process (car to-be-processed)))
                     (all-parents-loop (cons sheep-to-process seen)
                                       (union (sheep-parents sheep-to-process)
                                              parents)))))))
    (all-parents-loop () (sheep-parents sheep))))

(defun local-precedence-ordering (sheep)
  "Calculates the local precedence ordering. Relies on the fact that mapcar will
return when any list is NIL to avoid traversing the entire parent list."
  (let ((parents (sheep-parents sheep)))
    (mapcar 'list (cons sheep parents) parents)))

(defun std-tie-breaker-rule (minimal-elements hl-so-far)
  (mapc (fun (awhen (intersection minimal-elements (sheep-parents _))
               (return-from std-tie-breaker-rule (car it))))
        (reverse hl-so-far)))

(defun std-compute-sheep-hierarchy-list (sheep)
  "Lists SHEEP's ancestors, in precedence order."
  (handler-case
      ;; since collect-ancestors only collects the _ancestors_, we cons the sheep in front.
      (let ((sheeple-to-order (cons sheep (collect-ancestors sheep))))
        (topological-sort sheeple-to-order
                          (remove-duplicates
                           ;; LOCAL-PRECEDENCE-ORDERING conses up fresh structure,
                           ;; so we can be destructive here
                           (mapcan 'local-precedence-ordering sheeple-to-order))
                          'std-tie-breaker-rule))
    (simple-error () (error 'sheeple-hierarchy-error :sheep sheep))))

(defun compute-sheep-hierarchy-list (sheep)
  (typecase sheep
    (std-sheep (std-compute-sheep-hierarchy-list sheep))
    (otherwise (compute-sheep-hierarchy-list-using-metasheep
                (sheep-metasheep sheep) sheep))))

(defun memoize-sheep-hierarchy-list (sheep)
  (setf (%sheep-hierarchy-cache sheep) (compute-sheep-hierarchy-list sheep))
  (%map-children 'memoize-sheep-hierarchy-list sheep))

(defun std-finalize-sheep-inheritance (sheep)
  "Memoizes SHEEP's hierarchy list."
  (mapc (curry '%add-child sheep) (sheep-parents sheep))
  (memoize-sheep-hierarchy-list sheep)
  sheep)

(defun finalize-sheep-inheritance (sheep)
  "Memoizes SHEEP's hierarchy list, running a MOP hook along the way.
See `finalize-sheep-inheritance-using-metasheep'."
  (typecase sheep
    (std-sheep (std-finalize-sheep-inheritance sheep))
    (otherwise (finalize-sheep-inheritance-using-metasheep
                (sheep-metasheep sheep) sheep))))

;;; Add/remove parents
(defun remove-parent (parent sheep)
  "Removes PARENT from SHEEP, running a MOP hook along the way.
See `remove-parent-using-metasheeple'."
  (if (and (std-sheep-p parent) (std-sheep-p sheep))
      (std-remove-parent parent sheep)
      (remove-parent-using-metasheeple (sheep-metasheep parent) (sheep-metasheep sheep)
                                       parent sheep)))

(defun std-remove-parent (parent child)
  "Removes PARENT from SHEEP."
  (if (member parent (sheep-parents child))
      (prog1 child
        (deletef (%sheep-parents child) parent)
        (%remove-child child parent)
        (finalize-sheep-inheritance child))
      (error "~A is not a parent of ~A" parent child)))

(defun add-parent (new-parent child)
  "Adds NEW-PARENT as a parent to CHILD, running a MOP hook along the way.
See `add-parent-using-metasheeple'."
  (if (and (std-sheep-p new-parent) (std-sheep-p child))
      (std-add-parent new-parent child)
      (add-parent-using-metasheeple (sheep-metasheep new-parent) (sheep-metasheep child)
                                    new-parent child)))

(defun std-add-parent (new-parent child)
  "Adds NEW-PARENT as a parent to CHILD."
  (when (eq new-parent child) (error "Sheeple cannot be parents of themselves."))
  (when (member new-parent (sheep-parents child) :test 'eq)
    (error "~A is already a parent of ~A." new-parent child))
  (handler-case
      (progn
        (push new-parent (%sheep-parents child))
        (finalize-sheep-inheritance child)
        child)
    ;; This error is signaled by compute-sheep-hierarchy-list, which right now
    ;; is called from inside finalize-sheep-inheritance (this is probably a bad idea, move
    ;; c-s-h-l in here just to do the check?)
    ;; one problem with this is that it'll call c-s-h-l twice
    (sheeple-hierarchy-error () (progn (remove-parent new-parent child)
                                       (error 'sheeple-hierarchy-error :sheep child)))))

(defun add-parents (parents sheep)
  "Mostly a utility function for easily adding multiple parents. They will be added to
the front of the sheep's parent list in reverse order (so they will basically be appended
to the front of the list)"
  (map nil (rcurry 'add-parent sheep) (reverse parents))
  sheep)

(defun add-parent* (parent* sheep)
  "A utility/interface/laziness function, for adding parent(s) to a sheep."
  (ctypecase parent*
    (sheep (add-parent parent* sheep))
    (cons (add-parents parent* sheep))))

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
;;; Spawning
;;;
(defun make-sheep (parent* &rest all-keys
                     &key (metasheep =standard-metasheep=) &allow-other-keys)
  "Creates a new sheep with SHEEPLE as its parents. METASHEEP is used as the metasheep when
allocating the new sheep object. ALL-KEYS is passed on to INIT-SHEEP."
  (apply 'init-sheep
         (add-parent* (or (ensure-list parent*) =standard-sheep=)
                      (maybe-std-allocate-sheep metasheep))
         all-keys))

(defun spawn (&rest sheeple)
  "Creates a new standard-sheep object with SHEEPLE as its parents."
  (make-sheep sheeple))

;;; Feel free to change the exact interface if you don't like it. -- Adlai
(defun clone (sheep &optional (metasheep (sheep-metasheep sheep)))
  "Creates a sheep with the same parents and metasheep as SHEEP. If supplied, METASHEEP
will be used instead of SHEEP's metasheep, but SHEEP itself remains unchanged."
  (make-sheep (sheep-parents sheep) :metasheep metasheep))

;;;
;;; DEFSHEEP macro
;;;
(defun canonize-sheeple (sheeple)
  `(list ,@sheeple))

(defun canonize-properties (properties &optional (accessors-by-default nil))
  `(list ,@(mapcar (rcurry 'canonize-property accessors-by-default) properties)))

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
        (:accessor
         (cond ((or no-reader-p no-writer-p)
                (error "You said you didn't want a reader or a writer, but now you want one? Make up your mind."))
               ((null (cadr olist))
                (if (and (null writers) (null readers))
                    (setf no-reader-p t
                          no-writer-p t)
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

(defun canonize-options (options)
  (mapcan 'canonize-option options))

(defun canonize-option (option)
  (list (car option) (cadr option)))

(defmacro defsheep (sheeple properties &rest options)
  "Standard sheep-generation macro. This variant auto-generates accessors."
  `(make-sheep
    ,(canonize-sheeple sheeple)
    :properties ,(canonize-properties properties)
    ,@(canonize-options options)))

(defmacro defproto (name sheeple properties &rest options)
  "Words cannot express how useful this is."
  `(progn
     (declaim (special ,name))
     (let ((sheep (ensure-sheep
                   (when (boundp ',name) ,name)
                   ,(canonize-sheeple sheeple)
                   :properties ,(canonize-properties properties t)
                   ,@(canonize-options options))))
       (unless (sheep-nickname sheep)
         (setf (sheep-nickname sheep) ',name))
       (setf (symbol-value ',name) sheep))))

(defun ensure-sheep (maybe-sheep parents &rest options)
  (if maybe-sheep
      (apply 'reinit-sheep maybe-sheep :new-parents parents options)
      (apply 'make-sheep parents options)))
