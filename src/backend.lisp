;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; indent-tabs-mode: nil -*-
;;;;
;;;; This file is part of Sheeple
;;;;
;;;; object-backend.lisp
;;;;
;;;; Backend Datastructures Of The Object Hierarchy
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :sheeple)

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
                                       &aux (precedence-list (when parents
                                                               (compute-precedence parents))))))
  "Information about an object's ancestors and descendants."
  metaobject ; Not readonly for ease of bootstrapping
  (members         (make-weak-hash-table :weakness :key :test #'eq)
                   :read-only t :type hash-table) ; The lineage's members
  (parents         nil :type list :read-only t)              ; A set of objects
  (precedence-list nil :type list)) ; A precedence list of all the lineage's ancestors

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

(defun find-transition (mold property-name)
  "Returns the mold which adds a property named PROPERTY-NAME to MOLD.
If no such mold exists, returns NIL."
  (check-type mold mold)
  (values (gethash property-name (mold-transitions mold))))

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

(defun ensure-transition (mold property-name)
  "Returns the transition from MOLD indexed by PROPERTY-NAME, creating and
linking a new one if necessary."
  (check-type mold mold)
  (or (find-transition mold property-name)
      (aprog1 (make-mold (mold-lineage mold)
                         (vector-cons property-name (mold-properties mold)) mold)
        (setf (gethash property-name (mold-transitions mold)) it))))

(defun ensure-mold (metaobject parents &optional (properties #()))
  "Returns the mold with properties PROPERTIES of the mold for PARENTS,
creating and linking a new one if necessary."
;  (check-type metaobject object)
  (check-list-type parents object)
  (check-type properties vector)
  (let ((top (ensure-toplevel-mold metaobject parents)))
    (do* ((mold top (ensure-transition mold (car props-left)))
          (props-left (coerce properties 'list) (cdr props-left)))
         ((null props-left) mold))))

;;;
;;; Backend Bootstrap
;;;

(defvar =standard-metaobject= (std-allocate-object nil))

(setf (lineage-metaobject (mold-lineage (%object-mold =standard-metaobject=)))
      =standard-metaobject=)
