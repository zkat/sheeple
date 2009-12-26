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

;;;
;;; Mold Overview
;;;
;;; Molds act as a sort of "backend class" for objects. A mold is a separate concept from a
;;; metaobject. Their purpose is to offload the stuff that a lot of objects would share into
;;; a single object, and have many similar objects use the data stored in the mold.
;;; Right now, molds are used to keep track of direct properties and store the parents list.
;;; One big win already possible with molds is that they allow us to cache the entire
;;; precedence list for an object without having to worry about recalculating it every time
;;; a new object is created.
;;;
;;; In fact, there are two levels of caching going on; molds have their own shared data
;;; storage for 'lineages'. Lineages cache shared parent and precedence lists, and are also
;;; cached by objects, so that changes in precedence lists get propagated to children.
;;;
;;; A properties-related win is that since we hold information about *which* properties are
;;; available in the mold, our actual object instances can simply carry a lightweight vector
;;; whose slots are indexed into based on information held in the mold. This is identical to
;;; what CLOS implementations often do. Meaning? Direct property access can be as fast as
;;; CLOS' (once similar optimization strategies are implemented).
;;;
;;; There are 4 situations where molds must be handled:
;;; 1. A new object is created: in this case, find or create a new toplevel mold
;;; 2. A property is added: find or create one new transition mold
;;; 3. A property is -removed-: start from top of mold tree, find an acceptable mold
;;; 4. (setf object-parents) is called: begin from the beginning. Object may have properties.
;;;
;;; Every time a mold is switched up, care must be taken that relevant properties are copied
;;; over appropriately, and caches are reset. Additionally, when (setf object-parents) is called,
;;; all sub-molds must be alerted (and they must alert -their- sub-molds), and each sub-mold must
;;; recalculate its precedence list.
;;;
;;; One significant problem with the current transition model is that it does not try to reuse
;;; potentially similar transitions. For example, if there are two molds, A and B, an object
;;; that adds property b to mold A will transition to mold AB, whereas adding property a to
;;; mold B will transition to mold BA. Later on, this should (and -will-) be resolved.

;;;
;;; Data definitions
;;;
(defstruct (mold
             (:predicate moldp)
             (:constructor make-mold (lineage properties &optional back)))
  "Also known as 'backend classes', molds are hidden caches which enable
Sheeple to use class-based optimizations yet keep its dynamic power."
  (back        nil :read-only t :type (or null mold)) ; Back pointer
  (lineage     nil :read-only t :type lineage) ; A common cache of parent stuff
  (properties  nil :read-only t :type simple-vector) ; Direct properties
  (transitions (make-weak-hash-table :weakness :value :test #'eq)
               :read-only t :type hash-table)) ; V8-like links to other molds

(define-print-object ((object mold) :identity nil)
  (format t "on ~A" (mold-lineage object)))

(defstruct (lineage
             (:predicate lineagep)
             (:constructor
              make-lineage (metaobject parents
                                       &aux (precedence-list (compute-precedence parents)))))
  "Information about an object's ancestors and descendants."
  metaobject ; Not readonly for ease of bootstrapping
  (members         (make-weak-hash-table :weakness :key :test #'eq)
                   :read-only t :type hash-table) ; The lineage's members
  (parents         nil :read-only t)              ; A set of objects
  (precedence-list nil)) ; A precedence list of all the lineage's ancestors

(define-print-object ((object lineage) :identity nil)
  (format t "from ~{~{~:[[~A]~;~A~]~}~#[~; and ~:;, ~]~}"
          (mapcar (fun (list (direct-property-p _ 'nickname)
                             (object-nickname _)))
                  (lineage-parents object))))

(macrolet ((define-mold-reader (name lineage-reader)
             `(defun ,name (mold)
                (,lineage-reader (mold-lineage mold)))))
  (define-mold-reader mold-parents   lineage-parents)
  (define-mold-reader mold-precedence-list lineage-precedence-list))

(defstruct (object (:conc-name %object-) (:predicate objectp)
                   (:constructor std-allocate-object
                                 (metaobject &aux (mold (ensure-mold metaobject ()))))
                   (:print-object print-sheeple-object-wrapper))
  (mold (assert NIL) :type mold)
  (precedence-list nil :type list)
  (property-values nil)
  (roles nil :type list)) ; Roles are used in dispatch -- see reply-foo.lisp

(declaim (inline %object-metaobject %object-parents))
(defun %object-metaobject (object)
  (declare (optimize speed (safety 0)))
  (lineage-metaobject (mold-lineage (%object-mold object))))
(defun %object-parents (object)
  (declare (optimize speed (safety 0)))
  (lineage-parents (mold-lineage (%object-mold object))))

(defun %object-children (object)
  (gethash object (lineage-members (mold-lineage (%object-mold object)))))

(defun (setf %object-children) (new-kids object)
  (setf (gethash object (lineage-members (mold-lineage (%object-mold object))))
        new-kids))

(defun trigger-precedence-recalculation (lineage)
  "Updates LINEAGE's precedence list, and propagates down the members."
  (with-accessors ((precedence lineage-precedence-list)
                   (parents   lineage-parents)
                   (members   lineage-members)) lineage
    (setf precedence (compute-precedence parents))
    (maphash (lambda (member children)
               (setf (%object-precedence-list member) (compute-object-precedence-list member))
               (mapcar 'trigger-precedence-recalculation children))
             members)))

;;;
;;; Molds
;;;
(defvar *molds* (make-weak-hash-table :test 'equal :weakness :value)
  "Maps parent lists to their corresponding molds. This is the global entry
point to Sheeple's backend class system.")

(defun find-mold (metaobject parents)
;  (check-type metaobject object)
  (check-list-type parents object)
  (let ((cons (cons metaobject parents)))
    (declare (dynamic-extent cons))
    (values (gethash cons *molds*))))

(defun (setf find-mold) (mold metaobject parents)
;  (check-type metaobject object)
  (check-list-type parents object)
  (check-type mold mold)
  (setf (gethash (cons metaobject parents) *molds*) mold))

;;;
;;; Transitions
;;;
(defun find-transition (mold propd)
  "Returns the mold which adds a property named PROPERTY-NAME to MOLD.
If no such mold exists, returns NIL."
  (check-type mold mold)
  (values (gethash propd (mold-transitions mold))))

;;;
;;; Mold API -- Retrieval and Automatic Creation of Molds
;;;
(defun ensure-toplevel-mold (metaobject parents)
  "Returns the mold for PARENTS, creating and caching a new one if necessary."
;  (check-type metaobject object)
  (check-list-type parents object)
  (or (find-mold metaobject parents)
      (setf (find-mold metaobject parents)
            (make-mold (aprog1 (make-lineage metaobject parents)
                         (dolist (parent parents)
                           (push it (%object-children parent))))
                       (vector)))))

(defun ensure-transition (mold propd)
  "Returns the transition from MOLD indexed by PROPERTY-NAME, creating and
linking a new one if necessary."
  (check-type mold mold)
  (or (find-transition mold propd)
      (aprog1 (make-mold (mold-lineage mold)
                         (vector-cons (cons (property-name propd) propd) (mold-properties mold)) mold)
        (setf (gethash propd (mold-transitions mold)) it))))

(defun ensure-mold (metaobject parents &optional (properties #()))
  "Returns the mold with properties PROPERTIES of the mold for PARENTS,
creating and linking a new one if necessary."
;  (check-type metaobject object)
  (check-list-type parents object)
  (check-type properties vector)
  (let ((top (ensure-toplevel-mold metaobject parents)))
    (do* ((mold top (ensure-transition mold (cdar props-left)))
          (props-left (coerce properties 'list) (cdr props-left)))
         ((null props-left) mold))))

;;;
;;; Objects
;;;
(defun std-object-p (x)
  (when (objectp x)
    (eq (%object-metaobject x) =standard-metaobject=)))

(declaim (inline maybe-std-allocate-object))
(defun maybe-std-allocate-object (metaobject)
  (if (eq =standard-metaobject= metaobject)
      (std-allocate-object metaobject)
      (funcall 'smop:allocate-object metaobject)))

(defun std-print-sheeple-object (object stream)
  (print-unreadable-object (object stream :identity t)
    (format stream "Object ~:[[~S]~;~S~]"
            (ignore-errors (direct-property-p object 'nickname))
            (ignore-errors (object-nickname object)))))

(declaim (inline print-sheeple-object-wrapper))
(defun print-sheeple-object-wrapper (object stream)
  (handler-case
      (if (fboundp 'print-sheeple-object)
          (funcall 'print-sheeple-object object stream)
          (std-print-sheeple-object object stream))
    (no-applicable-reply () (std-print-sheeple-object object stream))))

(defun object-metaobject (object)
  (check-type object object)
  (%object-metaobject object))

(defun object-parents (object)
  (check-type object object)
  (%object-parents object))

;;; This utility is useful for concisely setting up object hierarchies
(defmacro with-object-hierarchy (object-and-parents &body body)
  "OBJECT-AND-PARENTS is a list, where each element is either a symbol or a list of
the form (OBJECT &REST PARENTS), where OBJECT is a symbol and each of PARENTS is a form
evaluating to produce a object object. Each OBJECT symbol is bound to a object with the
corresponding PARENTS, and the nickname is set to the symbol to facilitate debugging."
  `(let* ,(mapcar (fun (destructuring-bind (object &rest parents) (ensure-list _)
                         `(,object (object :parents ,(when parents ``(,,@parents))
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
    (prog (minimal-elements choice result result-tail)
       (declare (list minimal-elements result result-tail))
       top (setf minimal-elements (remove-if (fun (memq _ afters)) elements))
       (when (null minimal-elements) (go end))
       (setf choice (if (null (cdr minimal-elements)) (car minimal-elements)
                        (funcall tie-breaker minimal-elements result))
             elements (delete choice elements :test 'eq)
             (values befores afters) (parallel-delete choice befores afters))
       (unless result (go create-tail))
       (setf (cdr result-tail) (list choice))
       (pop result-tail) (go top) create-tail
       (setf result-tail (list choice) result result-tail) (go top)
       end (if (null elements) (return result)
               (error 'topological-sort-conflict
                      :conflicting-elements elements
                      :sorted-elements result
                      :constraints (mapcar 'cons befores afters))))))

(defun collect-ancestors (object)
  "Collects all of OBJECT's ancestors."
  (do* ((checked nil (cons chosen-object checked))
        (ancestors (copy-list (object-parents object))
                   (dolist (parent (object-parents chosen-object) ancestors)
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
  ;; Pick the one with a direct leftmost in the precedence list computed so far
  (dolist (candidate chosen-elements)
    (awhen (dolist (parent (object-parents candidate))
             (awhen (find parent (the list minimal-elements) :test 'eq) (return it)))
      (return-from std-tie-breaker-rule it))))

(defun compute-precedence (parents)
  "Generates an abstract precedence list out of PARENTS; this would be suitable as the
CDR of the precedence list of a standard object with PARENTS, in order, as its parents."
  (if (null (cdr parents))
      (unless (null (car parents)) ; FIXME: This happens ONCE during bootstrap
        (object-precedence-list (car parents)))
      (let ((unordered
             (delete-duplicates (append parents (mapcan 'collect-ancestors parents)))))
        (topological-sort
         unordered
         (delete-duplicates (nconc (mapcar 'cons parents (cdr parents))
                                   (mapcan 'local-precedence-ordering unordered))
                            :test #'equal)
         #'std-tie-breaker-rule))))

(defun std-compute-object-precedence-list (object)
  (cons object (mold-precedence-list (%object-mold object))))

(defun compute-object-precedence-list (object)
  "Computes the full precedence list for OBJECT"
  (if (eq =standard-metaobject= (%object-metaobject object))
      (std-compute-object-precedence-list object)
      (funcall 'smop:compute-object-precedence-list
               (%object-metaobject object) object)))

(defun object-precedence-list (object)
  "Returns the full precedence list for OBJECT"
  (%object-precedence-list object))

;;;
;;; Modifying mold-level stuff
;;;
(defun (setf object-mold) (new-mold object)
  (with-accessors ((new-lineage mold-lineage)) new-mold
    (with-accessors ((old-lineage mold-lineage)) (%object-mold object)
      (unless (eq new-lineage old-lineage)
        (setf (gethash object (lineage-members new-lineage)) (%object-children object))
        (remhash object (lineage-members old-lineage)))))
  (setf (%object-mold object) new-mold
        (%object-precedence-list object) (compute-object-precedence-list object))
  new-mold)

(defun change-mold (object new-mold)
  "Creates a new property-value vector in OBJECT, according to NEW-MOLD's specification, and
automatically takes care of bringing the correct property-values over into the new vector, in the
right order. Keep in mind that NEW-MOLD might specify some properties in a different order."
  (check-type object object)
  (check-type new-mold mold)
  (let* ((new-properties (mold-properties new-mold))
         (new-values (make-array (length new-properties)))
         (old-values (%object-property-values object)))
    (unless (zerop (length old-values))
      ;; This simplification of DO-HASH-VECTOR is suboptimal
      (let ((properties (mold-properties (%object-mold object))))
        (dotimes (position (length properties))
          (let ((pname (svref properties position)))
            (awhen (position pname new-properties)
              (setf (svref new-values it) (svref old-values position)))))))
    (setf (object-mold object) new-mold
          (%object-property-values object) new-values))
  object)

(defun change-parents (object new-parents)
  "Wraps around `change-mold' to give OBJECT a mold with the requested NEW-PARENTS.
This function has no high-level error checks and SHOULD NOT BE CALLED FROM USER CODE."
  (check-type object object)
  (check-list-type new-parents object)
  (change-mold object (ensure-mold (%object-metaobject object) new-parents
                                   (mold-properties (%object-mold object))))
  (map 'nil 'trigger-precedence-recalculation (%object-children object)))

(defun (setf object-parents) (new-parents object)
  (check-type object object)
  (check-list-type new-parents object)
  (let ((metaobject (object-metaobject object)))
    (dolist (parent new-parents)
      (unless (funcall 'smop:validate-parent metaobject object (object-metaobject parent) parent)
        (error "~A cannot be a parent of ~A" parent object))))
  (flet ((lose (reason) (error 'object-precedence-error :object object :conflict reason)))
    (let ((precedence (handler-case (compute-precedence new-parents)
                        (topological-sort-conflict (conflict) (lose conflict)))))
      (cond ((null precedence) (lose "Precedence list is empty"))
            ((find object precedence)
             (lose "Object appears multiple times in the precedence list"))
            (t (change-parents object new-parents)))))
  new-parents)

(defun (setf object-metaobject) (new-metaobject object)
  (setf (smop:object-metaobject (%object-metaobject object) object) new-metaobject))

;;; Inheritance predicates
(defun parentp (maybe-parent child)
  "A parent is a object directly in CHILD's parent list."
  (member maybe-parent (object-parents child)))

(defun ancestorp (maybe-ancestor descendant)
  "A parent is a object somewhere in CHILD's precedence list."
  (member maybe-ancestor (cdr (object-precedence-list descendant))))

(defun childp (maybe-child parent)
  "A child is a object that has PARENT in its parent list."
  (parentp parent maybe-child))

(defun descendantp (maybe-descendant ancestor)
  "A descendant is a object that has ANCESTOR in its precedence list."
  (ancestorp ancestor maybe-descendant))

;;;
;;; Spawning
;;;
(defun object (&rest all-keys &key parents (metaobject =standard-metaobject=)
               &allow-other-keys &aux (object (maybe-std-allocate-object metaobject)))
  "Returns a new object delegating to PARENTS, with metaobject METAOBJECT.
ALL-KEYS is passed on to INIT-OBJECT."
  (declare (dynamic-extent all-keys))
  (unless (listp parents)
    (warn 'deprecated-feature :version "3.0.2"
          :feature "Passing a non-list :parents option to #'OBJECT")
    (setf parents (list parents)))
  (handler-case
      (setf (object-mold object) (ensure-mold metaobject (or parents (list =standard-object=))))
    (topological-sort-conflict (conflict)
      (error 'object-precedence-error :object object :conflict conflict)))
  (apply 'init-object object all-keys))

(defun clone (object &optional (metaobject (%object-metaobject object)))
  "Creates a object with the same parents and metaobject as OBJECT. If supplied, METAOBJECT
will be used instead of OBJECT's metaobject, but OBJECT itself remains unchanged."
  (when (eq =t= object)
    (error 'fuck-off :format-control "You ain't allowed to clone =T=. Shoo."))
  (aprog1 (maybe-std-allocate-object metaobject)
    (setf (object-mold it) (%object-mold object))
    (with-accessors ((props %object-property-values)
                     (roles %object-roles)) object
      (with-accessors ((new-props %object-property-values)
                       (new-roles %object-roles)) it
        (setf new-roles (copy-list roles))
        (when props
          (setf new-props (copy-simple-vector props)))))))

;;;
;;; Fancy Macros
;;;
(defun canonize-parents (parents)
  `(list ,@(ensure-list parents)))

(defun canonize-properties (properties &optional (accessors-by-default nil))
  `(list ,@(mapcar (rcurry 'canonize-property accessors-by-default) properties)))

(defun canonize-property (property &optional (accessors-by-default nil))
  (let* ((property-name (if (consp property) (car property) property))
         (property-value (when (consp property) (cadr property)))
         (rest-of-property (when (consp property) (cddr property)))
         (add-accessor-p (and (if (consp property) (not (find :accessor (cddr property))) t)
                              accessors-by-default)))
    `(list ',property-name ,property-value ,@rest-of-property
           ,@(when add-accessor-p `(:accessor ',property-name)))))

(defun canonize-options (options)
  (flet ((defclass-option-syntax-p (form)
           (typep form '(cons symbol list))))
    (declare (dynamic-extent #'defclass-option-syntax-p))
    (when (and options (every #'defclass-option-syntax-p options))
      (warn 'deprecated-feature :version "3.0.2"
            :feature "DEFCLASS-style options to DEFPROTO and DEFOBJECT")
      (flet ((canonize-defclasslike-option (option)
               (symbol-macrolet
                   ((simple-expansion `(,(car option) ,(cadr option))))
                 (case (car option)
                   ((:nickname :metaobject) simple-expansion)
                   (:documentation
                    (let ((docstring (cadr option)))
                      (check-type docstring string))
                    simple-expansion)
                   (otherwise `(',(car option) ',(cdr option)))))))
        (return-from canonize-options
          (mapcan #'canonize-defclasslike-option options)))))
  ;; In the new DEFPROTO option style, this function is a no-op
  options)

(defmacro defobject (objects &optional ((&rest properties)) &rest options)
  "Standard object-generation macro."
  `(object :parents ,(canonize-parents objects)
           :properties ,(canonize-properties properties)
           ,.(canonize-options options)))

(defmacro defproto (name &optional objects ((&rest properties)) &rest options)
  "Words cannot express how useful this is."
  (let ((canonized-properties (canonize-properties properties t)))
   `(progn
      (declaim (special ,name))
      (eval-when (:compile-toplevel)
        ,.(generate-defproto-accessors canonized-properties))
      (setf (symbol-value ',name)
            (ensure-object (when (boundp ',name)
                             (symbol-value ',name))
                           ,(canonize-parents objects)
                           :properties ,canonized-properties
                           ,.(canonize-options options)
                           :nickname ',name)))))

(defun generate-defproto-accessors (canonized-properties &aux messages)
  (dolist (property-spec (mapcar 'cdr (cdr canonized-properties)) messages)
    (loop with type and name do
         (setf (values type name property-spec)
               (get-properties property-spec '(:accessor :reader :writer)))
         (setf property-spec (cddr property-spec))
         while type when name do
         (flet ((add-reader (name)
                  (push `(ensure-message ,name :lambda-list '(object)) messages))
                (add-writer (name)
                  (push `(ensure-message ,name :lambda-list '(new-value object)) messages)))
           (case type
             (:accessor (add-reader name)
                        (add-writer ``(setf ,,name)))
             (:reader (add-reader name))
             (:writer (add-writer name)))))))

(defun ensure-object (maybe-object parents &rest options)
  (if maybe-object
      (apply 'reinit-object maybe-object :parents parents options)
      (apply 'object :parents parents options)))
