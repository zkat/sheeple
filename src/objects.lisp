;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; indent-tabs-mode: nil -*-

;;;; This file is part of Sheeple

;;;; objects.lisp
;;;;
;;;; Object creation, cloning, inspection
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :sheeple)

;; We declare these base vars here so that bootstrapping won't complain.
(define-bound-variables =t= =standard-object= =standard-metaobject=)

(defvar *bootstrappedp* nil)

;;;
;;; Molds
;;;
;;; - Molds act as a sort of "backend class" for objects. A mold is a separate concept from a
;;;   metaobject. Their purpose is to offload the stuff that a lot of objects would share into
;;;   a single object, and have many similar objects use the data stored in the mold.
;;;   Right now, molds are used to hold the list of direct properties and list of parents.
;;;   One big win already possible with molds is that they allow us to cache the entire
;;;   hierarchy list for an object without having to worry about recalculating it every time
;;;   a new object is created.
;;;   A properties-related win is that since we hold information about *which* properties are
;;;   available in the mold, our actual object instances can simply carry a lightweight vector
;;;   whose slots are indexed into based on information held in the mold. This is identical to
;;;   what CLOS implementations often do. Meaning? Direct property access can be as fast as
;;;   CLOS' (once similar optimization strategies are implemented).
(defstruct (mold (:predicate   moldp)
                 (:constructor make-mold (parents properties)))
  (parents        nil :read-only t) ;list of parents
  (properties     nil :read-only t) ;list of properties (later, property metaobjects)
  (hierarchy-list nil) ; cached hierarchy-list, topologically-sorted based on PARENTS
  (sub-molds      nil) ; molds that this mold must report to when there are HL changes.
  (transitions    nil)) ; transitions are new molds that are similar to this one, but add one property.

(deftype property-name ()
  "A valid name for an object's property"
  'symbol)

(define-print-object ((mold mold)))

(defun find-transition (mold property-name)
  "Returns the mold which adds a property named PROPERTY-NAME to MOLD.
If no such mold exists, returns NIL."
  (cdr (assoc property-name (mold-transitions mold) :test 'eq)))

;;; TODO: ensure-transition-by-property  - Adlai

(defun add-transition-by-property (from-mold property-name to-mold)
  "Adds a link from FROM-MOLD to TO-MOLD, indexed by PROPERTY-NAME.
If a new link was created, FROM-MOLD is returned; otherwise, an error of type
`mold-collision' is signaled."
  (check-type from-mold mold)
  (check-type property-name property-name)
  (check-type to-mold mold)
  (assert (null (set-difference (mold-properties from-mold)
                                (mold-properties to-mold))) ()
          "~A does not contain all the properties of ~A, and is thus not a ~
           valid transition to it." to-mold from-mold)
  (assert (equal (list property-name)
                 (set-difference (mold-properties to-mold)
                                 (mold-properties from-mold)))
          () "~A is not a unique property transition from ~A to ~A."
          property-name from-mold to-mold)
  (awhen (assoc property-name (mold-transitions from-mold))
    (error 'mold-collision :new-mold to-mold :collision-mold (cdr it)))
  (aconsf (mold-transitions from-mold) property-name to-mold)
  from-mold)

(defun find-mold-by-transition (start-mold goal-properties)
  "Searches the transition tree from START-MOLD to find the mold containing
GOAL-PROPERTIES, returning that mold if found, or NIL on failure."
  (check-type start-mold mold)
  (check-list-type goal-properties property-name)
  ;; This algorithm is very concise, but it's not optimal AND it's unclear.
  ;; Probably the first target for cleaning up. - Adlai
  (let ((path (set-difference goal-properties (mold-properties start-mold))))
    (if (null path) start-mold
        (awhen (some (fun (find-transition start-mold _)) path)
          (find-mold-by-transition it path)))))

(defvar *molds* (make-hash-table :test 'equal))

(defun find-mold-tree (parents)
  (check-list-type parents object)
  (values (gethash parents *molds*)))

(defun (setf find-mold-tree) (mold parents)
  (check-list-type parents object)
  (check-type mold mold)
  (setf (gethash parents *molds*) mold))

(defun find-mold (parents properties)
  "Searches the mold cache for one with parents PARENTS and properties PROPERTIES,
returning that mold if found, or NIL on failure."
  (check-list-type parents object)
  (check-list-type properties property-name)
  (awhen (find-mold-tree parents)
    (find-mold-by-transition it properties)))

;;; Creating molds

(defun ensure-mold-tree (parents)
  "Returns the mold tree for PARENTS, creating and caching a new one if necessary."
  (or (find-mold-tree parents)
      (setf (find-mold-tree parents)
            (make-mold parents nil))))

;;; Deprecated
(defun ensure-mold (parents properties)
  (or (find-mold parents properties)
      (link-mold (make-mold parents properties))))

(defun build-mold-transition-between (mold bounds)
  "Returns a linear mold transition tree leading to MOLD. BOUNDS is a strict subset
of MOLD's properties, representing the inclusive upper bound for the new tree."
  (check-type mold mold)
  (check-list-type bounds property-name)
  (assert (and (subsetp bounds (mold-properties mold))
               (not (subsetp (mold-properties mold) bounds)))
          () "~A is not a strict subset of the properties of ~A"
          bounds mold)
  (labels ((build-up-links (mold path)
             (if (null path) mold
                 (let ((new-mold (make-mold (mold-parents mold)
                                            (remove (car path)
                                                    (mold-properties mold)))))
                   (build-up-links (add-transition-by-property new-mold (car path) mold)
                                   (cdr path))))))
    (build-up-links mold (set-difference (mold-properties mold) bounds))))

(defun link-mold (mold)
  "Links MOLD into the mold cache, returning NIL if MOLD was already linked, or MOLD
if it successfully linked MOLD into the cache."
  (check-type mold mold)
  (let ((parents (mold-parents mold)) (props (mold-properties mold)))
    ;; Do we need to create a new tree in the mold cache?
    (aif (find-mold parents nil)
         ;; No; so we find the common base, and link the remaining tree to that.
         (do* ((base it next-base)
               (prop (car props) (car props-left))
               (props-left (cdr props) (cdr props-left))
               (next-base (find-transition base prop)
                          (find-transition next-base prop)))
              ((eq next-base mold))
           (when (null next-base)
             (add-transition-by-property
              base prop (build-mold-transition-between mold (mold-properties base)))))
         ;; Yes; so this builds an entire tree, from no props, to MOLD.
         (prog1 mold
           (setf (gethash parents *molds*)
                 (build-mold-transition-between mold nil))))))

;;;
;;; Objects
;;;
(defstruct (object (:conc-name %object-) (:predicate objectp)
                   (:constructor std-allocate-object (metaobject))
                   (:print-object print-sheeple-object-wrapper))
  mold metaobject property-values roles)

(declaim (inline %object-metaobject %object-parents %object-properties
                 %object-roles %object-hierarchy-cache %object-children))

(defun std-object-p (x)
  (ignore-errors (eq (%object-metaobject x) =standard-metaobject=)))

(defun maybe-std-allocate-object (metaobject)
  (if (eq =standard-metaobject= metaobject)
      (std-allocate-object metaobject)
      (allocate-object metaobject)))

(defun std-print-sheeple-object (object stream)
  (print-unreadable-object (object stream :identity t)
    (format stream "Object ~:[[~S]~;~S~]"
            (has-direct-property-p object 'nickname)
            (ignore-errors (object-nickname object)))))

(declaim (inline print-sheeple-object-wrapper))
(defun print-sheeple-object-wrapper (object stream)
  (handler-bind ((no-applicable-replies (fun (return-from print-sheeple-object-wrapper
                                               (std-print-sheeple-object object stream))))
                 (unbound-function (fun (when (eq (cell-error-name _) 'print-object)
                                          (return-from print-sheeple-object-wrapper
                                            (std-print-sheeple-object object stream))))))
    (print-sheeple-object object stream)))

;; If we didn't define these functions, Lisp's package system would
;; export the SETF version as well as the reader.
(defun object-metaobject (object)
  (%object-metaobject object))

(defun object-parents (object)
  (%object-parents object))

;;; children cache
(defvar *child-cache-initial-size* 5
  "The initial size for a object's child cache.")

(defvar *child-cache-grow-ratio* 5
  "The ratio by which the child-cache is expanded when full.")

(symbol-macrolet ((%children (%object-children object)))

  (defun %create-child-cache (object)
    "Sets OBJECT's child cache to a blank (simple-vector `*child-cache-initial-size*')"
    (setf %children (make-vector *child-cache-initial-size*)))

  (defun %child-cache-full-p (object)
    "A child cache is full if all its items are live weak pointers to other object."
    (aand %children (every 'maybe-weak-pointer-value it)))

  (defun %enlarge-child-cache (object)
    "Enlarges OBJECT's child cache by the value of `*child-cache-grow-ratio*'."
    (let* ((old-vector (%object-children object))
           (new-vector (make-vector (* *child-cache-grow-ratio* (length old-vector)))))
      (setf (%object-children object) (replace new-vector old-vector))
      object))

  (defun %add-child (child object)
    "Registers CHILD in OBJECT's child cache."
    (let ((children %children))
      (if children
          (when (%child-cache-full-p object)
            (%enlarge-child-cache object)
            (setf children %children))
          (progn (%create-child-cache object)
                 (setf children %children)))
      (unless (find child children :key 'maybe-weak-pointer-value)
        (dotimes (i (length children))
          (unless (maybe-weak-pointer-value (aref children i))
            (return (setf (aref children i) (make-weak-pointer child)))))))
    object)

  (defun %remove-child (child object)
    "Takes CHILD out of OBJECT's child cache."
    (awhen (position child %children :key 'maybe-weak-pointer-value)
      (setf (svref %children it) nil))
    object)

  (defun %map-children (function object)
    "Applies FUNCTION to each of OBJECT's children."
    (awhen %children
      (map nil (fun (awhen (maybe-weak-pointer-value _) (funcall function it))) it)))

) ;end symbol-macrolet

;;; This utility is useful for concisely setting up object hierarchies
(defmacro with-object-hierarchy (object-and-parents &body body)
  "OBJECT-AND-PARENTS is a list, where each element is either a symbol or a list of
the form (OBJECT &REST PARENTS), where OBJECT is a symbol and each of PARENTS is a form
evaluating to produce a object object. Each OBJECT symbol is bound to a object with the
corresponding PARENTS, and the nickname is set to the symbol to facilitate debugging."
  `(let* ,(mapcar (fun (destructuring-bind (object &rest parents) (ensure-list _)
                         `(,object (make-object ,(when parents ``(,,@parents))
                                              :nickname ',object))))
                  object-and-parents)
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

(defun collect-ancestors (object)
  "Collects all of OBJECT's ancestors."
  (do* ((checked nil (cons chosen-object checked))
        (ancestors (copy-list (%object-parents object))
                   (dolist (parent (%object-parents chosen-object) ancestors)
                     (unless (member parent ancestors)
                       (push parent ancestors))))
        (chosen-object (car ancestors)
                      (dolist (ancestor ancestors)
                        (unless (find ancestor checked :test 'eq)
                          (return ancestor)))))
       ((not chosen-object) ancestors)))

(defun local-precedence-ordering (object)
  "Calculates the local precedence ordering."
  (let ((parents (object-parents object)))
    ;; Since MAPCAR returns once any list is NIL, we only traverse the parent list once.
    (mapcar 'cons (cons object parents) parents)))

(defun std-tie-breaker-rule (minimal-elements chosen-elements)
  (dolist (candidate chosen-elements)
    (awhen (dolist (parent (object-parents candidate))
             (awhen (find parent (the list minimal-elements) :test 'eq) (return it)))
      (return-from std-tie-breaker-rule it))))

(defun std-compute-object-hierarchy-list (object)
  "Lists OBJECT's ancestors, in precedence order."
  (cond
    ((cdr (%object-parents object))
     (handler-case
         ;; since collect-ancestors only collects the _ancestors_, we cons the object in front.
         ;; LOCAL-PRECEDENCE-ORDERING returns fresh conses, so we can be destructive.
         (let ((unordered (cons object (collect-ancestors object))))
           (topological-sort unordered
                             (delete-duplicates (mapcan 'local-precedence-ordering unordered))
                             'std-tie-breaker-rule))
       (simple-error () (error 'object-hierarchy-error :object object))))
    ((car (%object-parents object))
     (let ((cache (%object-hierarchy-cache (car (%object-parents object)))))
       (error-when (find object cache) 'object-hierarchy-error :object object)
       (cons object cache)))
    (t (list object))))

(defun compute-object-hierarchy-list (object)
  (if (std-object-p object)
      (std-compute-object-hierarchy-list object)
      (compute-object-hierarchy-list-using-metaobject
       (object-metaobject object) object)))

(defun memoize-object-hierarchy-list (object)
  (setf (%object-hierarchy-cache object) (compute-object-hierarchy-list object))
  (%map-children 'memoize-object-hierarchy-list object))

(defun std-finalize-object-inheritance (object)
  "Memoizes OBJECT's hierarchy list."
  (dolist (parent (object-parents object)) (%add-child object parent))
  (memoize-object-hierarchy-list object)
  object)

(defun finalize-object-inheritance (object)
  "Memoizes OBJECT's hierarchy list, running a MOP hook along the way.
See `finalize-object-inheritance-using-metaobject'."
  (if (std-object-p object)
      (std-finalize-object-inheritance object)
      (finalize-object-inheritance-using-metaobject
       (object-metaobject object) object)))

;;; Add/remove parents
(defun remove-parent (parent object)
  "Removes PARENT from OBJECT, running a MOP hook along the way.
See `remove-parent-using-metaobjects'."
  (if (and (std-object-p parent) (std-object-p object))
      (std-remove-parent parent object)
      (remove-parent-using-metaobjects (object-metaobject parent) (object-metaobject object)
                                       parent object)))

(defun std-remove-parent (parent child)
  "Removes PARENT from CHILD"
  (error-when (not (parentp parent child)) "~A is not a parent of ~A" parent child)
  (deletef (%object-parents child) parent)
  (%remove-child child parent)
  (finalize-object-inheritance child))

(defun add-parent (new-parent child)
  "Adds NEW-PARENT as a parent to CHILD, running a MOP hook along the way.
See `add-parent-using-metaobjects'."
  (if (and (std-object-p new-parent) (std-object-p child))
      (std-add-parent new-parent child)
      (add-parent-using-metaobjects (object-metaobject new-parent) (object-metaobject child)
                                    new-parent child)))

(defun std-add-parent (new-parent child)
  "Adds NEW-PARENT as a parent to CHILD."
  (error-when (eq new-parent child) "Objects cannot be parents of themselves.")
  (error-when (member new-parent (object-parents child) :test 'eq)
              "~A is already a parent of ~A." new-parent child)
  (handler-bind
      ((object-hierarchy-error (fun (remove-parent new-parent child))))
    (pushend new-parent (%object-parents child))
    (finalize-object-inheritance child)
    child))

(defun add-parents (parents object)
  "Adds multiple parents to the hierarchy list. The net effect of this function
is that the parents are appended to the end of the list."
  (prog1 object (mapc (rcurry 'add-parent object) parents)))

(defun add-parent* (parent* object)
  "A utility/interface/laziness function, for adding parent(s) to a object."
  (ctypecase parent*
    (object (add-parent parent* object))
    (cons (add-parents parent* object))))

(defun object-hierarchy-list (object)
  "Returns the full hierarchy-list for OBJECT"
  (%object-hierarchy-cache object))

;;; Inheritance predicates
(defun parentp (maybe-parent child)
  "A parent is a object directly in CHILD's parent list."
  (member maybe-parent (object-parents child)))

(defun ancestorp (maybe-ancestor descendant)
  "A parent is a object somewhere in CHILD's hierarchy list."
  (member maybe-ancestor (cdr (object-hierarchy-list descendant))))

(defun childp (maybe-child parent)
  "A child is a object that has PARENT in its parent list."
  (parentp parent maybe-child))

(defun descendantp (maybe-descendant ancestor)
  "A descendant is a object that has ANCESTOR in its hierarchy-list."
  (ancestorp ancestor maybe-descendant))

;;;
;;; Spawning
;;;
(defun make-object (parent* &rest all-keys
                   &key (metaobject =standard-metaobject=) &allow-other-keys)
  "Creates a new object with OBJECTS as its parents. METAOBJECT is used as the metaobject when
allocating the new object object. ALL-KEYS is passed on to INIT-OBJECT."
  (declare (dynamic-extent all-keys))
  ;; Here's what's causing the current failure with MAKE-OBJECT:
  ;; FINALIZE-OBJECT-INHERITANCE isn't dispatching correctly because the new object
  ;; object created by maybe-std-allocate-object has no parents. Thus, the reply for
  ;; F-S-I specialized on (=standard-metaobject= =T=) doesn't run. The metaobject itself is
  ;; fine, but =T= isn't in the new object's hierarchy-list yet (and it won't be until
  ;; ADD-PARENT, and then F-S-I both work. This is a serious issue with the MOP that we
  ;; might possibly need an ugly hack to fix. For now, it's good to know that this is
  ;; the reason that failure is happening, so we can rest assured that standard objects
  ;; behavior is working fine and dandy. -- sykopomp
  (apply 'init-object
         (add-parent* (or parent* =standard-object=)
                      (finalize-object-inheritance (maybe-std-allocate-object metaobject)))
         all-keys))

(defun spawn (&rest objects)
  "Creates a new standard-object object with OBJECTS as its parents."
  (declare (dynamic-extent objects))
  (make-object objects))

;; Feel free to change the exact interface if you don't like it. -- Adlai
;; TODO: this should actually copy OBJECT's roles and properties locally. -- sykopomp
(defun clone (object &optional (metaobject (object-metaobject object)))
  "Creates a object with the same parents and metaobject as OBJECT. If supplied, METAOBJECT
will be used instead of OBJECT's metaobject, but OBJECT itself remains unchanged."
  (make-object (object-parents object) :metaobject metaobject))

;;;
;;; fancy macros
;;;
(defun canonize-objects (objects)
  `(list ,@objects))

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

(defmacro defobject (objects properties &rest options)
  "Standard object-generation macro. This variant auto-generates accessors."
  `(make-object
    ,(canonize-objects objects)
    :properties ,(canonize-properties properties)
    ,@(canonize-options options)))

(defmacro defproto (name objects properties &rest options)
  "Words cannot express how useful this is."
  `(progn
     (declaim (special ,name))
     (let ((object (ensure-object
                   (when (boundp ',name) (symbol-value ',name))
                   ,(canonize-objects objects)
                   :properties ,(canonize-properties properties t)
                   ,@(canonize-options options))))
       (unless (or (not *bootstrappedp*) (has-direct-property-p object 'nickname))
         (setf (object-nickname object) ',name))
       (setf (symbol-value ',name) object))))

(defun ensure-object (maybe-object parents &rest options)
  (if maybe-object
      (apply 'reinit-object maybe-object :parents parents options)
      (apply 'make-object parents options)))
