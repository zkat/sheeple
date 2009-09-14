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
  "Creates a standard sheep object."
  (vector metasheep nil nil nil nil nil))

(defun maybe-std-allocate-sheep (metasheep)
  (if (eq =standard-metasheep= metasheep)
      (std-allocate-sheep metasheep)
      (allocate-sheep metasheep)))

;;; STD-SHEEP accessor definitions
(defmacro define-internal-accessors (&body names-and-indexes)
  `(progn ,@(loop for (name index) on names-and-indexes by #'cddr
               collect `(progn (declaim (inline ,name (setf ,name)))
                               (defun ,name (sheep) (svref sheep ,index))
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
      (map nil (fun (awhen (maybe-weak-pointer-value _) (funcall function it))) it)))

) ;end symbol-macrolet

;;; This utility is useful for concisely setting up sheep hierarchies
(defmacro with-sheep-hierarchy (sheep-and-parents &body body)
  "SHEEP-AND-PARENTS is a list, where each element is either a symbol or a list of
the form (SHEEP &REST PARENTS), where SHEEP is a symbol and each of PARENTS is a form
evaluating to produce a sheep object. Each SHEEP symbol is bound to a sheep with the
corresponding PARENTS, and the nickname is set to the symbol to facilitate debugging."
  `(let* ,(mapcar (fun (destructuring-bind (sheep &rest parents) (ensure-list _)
                         `(,sheep (make-sheep ,(when parents ``(,,@parents))
                                              :nickname ',sheep))))
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
  (multiple-value-bind (befores afters) (nunzip-alist constraints)
    (loop for minimal-elements = (remove-if (fun (memq _ afters)) elements)
       while minimal-elements
       for choice = (if (null (cdr minimal-elements))
                        (car minimal-elements)
                        (funcall tie-breaker minimal-elements result))
       with result do (push choice result)
         (setf elements (delete choice elements :test 'eq)
               (values befores afters) (parallel-delete choice befores afters))
       finally (if (null elements)
                   (return-from topological-sort (nreverse result))
                   (error "Inconsistent precedence graph.")))))

(defun collect-ancestors (sheep)
  "Collects all of SHEEP's ancestors."
  (do* ((checked nil (cons chosen-sheep checked))
        (ancestors (copy-list (%sheep-parents sheep))
                   (dolist (parent (%sheep-parents chosen-sheep) ancestors)
                     (unless (member parent ancestors)
                       (push parent ancestors))))
        (chosen-sheep (car ancestors)
                      (dolist (ancestor ancestors)
                        (unless (find ancestor checked :test 'eq)
                          (return ancestor)))))
       ((not chosen-sheep) ancestors)))

(defun local-precedence-ordering (sheep)
  "Calculates the local precedence ordering."
  (let ((parents (sheep-parents sheep)))
    ;; Since MAPCAR returns once any list is NIL, we only traverse the parent list once.
    (mapcar 'cons (cons sheep parents) parents)))

(defun std-tie-breaker-rule (minimal-elements chosen-elements)
  (dolist (candidate chosen-elements)
    (awhen (dolist (parent (sheep-parents candidate))
             (awhen (find parent (the list minimal-elements) :test 'eq) (return it)))
      (return-from std-tie-breaker-rule it))))

(defun std-compute-sheep-hierarchy-list (sheep)
  "Lists SHEEP's ancestors, in precedence order."
  (cond
    ((cdr (%sheep-parents sheep))
     (handler-case
         ;; since collect-ancestors only collects the _ancestors_, we cons the sheep in front.
         ;; LOCAL-PRECEDENCE-ORDERING returns fresh conses, so we can be destructive.
         (let ((unordered (cons sheep (collect-ancestors sheep))))
           (topological-sort unordered
                             (delete-duplicates (mapcan 'local-precedence-ordering unordered))
                             'std-tie-breaker-rule))
       (simple-error () (error 'sheeple-hierarchy-error :sheep sheep))))
    ((car (%sheep-parents sheep))
     (let ((cache (%sheep-hierarchy-cache (car (%sheep-parents sheep)))))
       (if (find sheep cache)
           (error 'sheeple-hierarchy-error :sheep sheep)
           (cons sheep cache))))
    (t (list sheep))))

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
  (dolist (parent (sheep-parents sheep)) (%add-child sheep parent))
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
  (handler-bind
      ((sheeple-hierarchy-error (fun (remove-parent new-parent child))))
    (push new-parent (%sheep-parents child))
    (finalize-sheep-inheritance child)
    child))

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
  (member maybe-parent (sheep-parents child)))

(defun ancestorp (maybe-ancestor descendant)
  "A parent is a sheep somewhere in CHILD's hierarchy list."
  (member maybe-ancestor (cdr (sheep-hierarchy-list descendant))))

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
  ;; Here's what's causing the current failure with MAKE-SHEEP:
  ;; FINALIZE-SHEEP-INHERITANCE isn't dispatching correctly because the new sheep
  ;; object created by maybe-std-allocate-sheep has no parents. Thus, the reply for
  ;; F-S-I specialized on (=standard-metasheep= =T=) doesn't run. The metasheep itself is
  ;; fine, but =T= isn't in the new object's hierarchy-list yet (and it won't be until
  ;; ADD-PARENT, and then F-S-I both work. This is a serious issue with the MOP that we
  ;; might possibly need an ugly hack to fix. For now, it's good to know that this is
  ;; the reason that failure is happening, so we can rest assured that standard sheeple
  ;; behavior is working fine and dandy. -- sykopomp
  (apply 'init-sheep
         (add-parent* (or parent* =standard-sheep=)
                      (finalize-sheep-inheritance (maybe-std-allocate-sheep metasheep)))
         all-keys))

(defun spawn (&rest sheeple)
  "Creates a new standard-sheep object with SHEEPLE as its parents."
  (make-sheep sheeple))

;; Feel free to change the exact interface if you don't like it. -- Adlai
;; TODO: this should actually copy SHEEP's roles and properties locally. -- sykopomp
(defun clone (sheep &optional (metasheep (sheep-metasheep sheep)))
  "Creates a sheep with the same parents and metasheep as SHEEP. If supplied, METASHEEP
will be used instead of SHEEP's metasheep, but SHEEP itself remains unchanged."
  (make-sheep (sheep-parents sheep) :metasheep metasheep))

;;;
;;; fancy macros
;;;
(defun canonize-sheeple (sheeple)
  `(list ,@sheeple))

(defun canonize-properties (properties &optional (accessors-by-default nil))
  `(list ,@(mapcar (rcurry 'canonize-property accessors-by-default) properties)))

(defun canonize-property (property &optional (accessors-by-default nil))
  `(list ',(car property) ,@(cdr property)
         ,@(when (and (not (find :accessor (cddr property)))
                      accessors-by-default)
                 `(:accessor ',(car property)))))

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
                   (when (boundp ',name) (symbol-value ',name))
                   ,(canonize-sheeple sheeple)
                   :properties ,(canonize-properties properties t)
                   ,@(canonize-options options))))
       (unless (or (not *bootstrappedp*) (has-direct-property-p sheep :nickname))
         (setf (sheep-nickname sheep) ',name))
       (setf (symbol-value ',name) sheep))))

(defun ensure-sheep (maybe-sheep parents &rest options)
  (if maybe-sheep
      (apply 'reinit-sheep maybe-sheep :new-parents parents options)
      (apply 'make-sheep parents options)))
