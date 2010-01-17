;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; indent-tabs-mode: nil -*-

;;;; This file is part of Sheeple

;;;; builtins.lisp
;;;;
;;;; Boxing of built-in lisp types
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :sheeple)

(macrolet ((defvars (&rest syms) `(progn ,@(mapcar (fun `(defvar ,_)) syms))))
  (defvars =boxed-object= =symbol= =sequence= =array= =number= =character= =function=
           =hash-table= =package= =pathname= =readtable= =stream= =list= =null= =cons=
           =vector= =bit-vector= =string= =complex= =integer= =float= =boolean=))

(defun box-type-of (x)
  "Maps the type of X to a built-in object."
  (typecase x
    (object (warn "This is already an object!")    x)
    (null                                          =null=)
    (boolean                                       =boolean=)
    ((and symbol (not null))                       =symbol=)
    ((complex *)                                   =complex=)
    ((integer * *)                                 =integer=)
    ((float * *)                                   =float=)
    (cons                                          =cons=)
    (character                                     =character=)
    (hash-table                                    =hash-table=)
    (package                                       =package=)
    (pathname                                      =pathname=)
    (readtable                                     =readtable=)
    (stream                                        =stream=)
    ((and number (not (or integer complex float))) =number=)
    ((string *)                                    =string=)
    ((bit-vector *)                                =bit-vector=)
    ((and vector (not string))                     =vector=)
    ((and array (not vector))                      =array=)
    (function                                      =function=)
    (t                                             =boxed-object=)))

;; This thing is a bit problematic. We don't necessarily want to keep references around to
;; objects that have been autoboxed, right? I'm tempted to say that the ideal here would
;; be a weak hash table with both key *and* value weakness. For now, though, we hold on
;; to references until a solution is thought out.
(defvar *boxed-object-table* (make-hash-table)
  "Lisp objects boxed by Objects are stored in here.")

(defun wrapped-object (box)
  (property-value box 'wrapped-object))

(defun box-object (object)
  "Wraps OBJECT with a object."
  (when (objectp object)
    (restart-case (error "~S is already a object." object)
      (continue ()
        :report (lambda (s) (format s "Box ~S anyways." object)))
      (return-object ()
        :report (lambda (s) (format s "Do not box ~S." object))
        (return-from box-object object))))
  (setf (gethash object *boxed-object-table*)
        (defobject ((box-type-of object))
            ((wrapped-object object))
          :nickname object)))

(defun remove-boxed-object (object)
  "Kills object dead"
  (remhash object *boxed-object-table*))

(defun find-boxed-object (object)
  "Finds a previously-boxed object in the boxed object table.
Returns the boxed object, or NIL if OBJECT has not been boxed."
  (values (gethash object *boxed-object-table*)))

(declaim (inline ensure-dispatch-object))
(defun ensure-dispatch-object (object)
  "Ensures that OBJECT is a valid object for reply dispatch."
  (if (objectp object) object
      (or (find-boxed-object object)
          (box-type-of object))))

(defun ensure-boxed-object (object)
  "Returns two values: OBJECT or a boxed object representing it, and a boolean
specifying whether boxing took place."
  (if (objectp object)
      (values object nil)
      (aif (find-boxed-object object)
           (values it nil)
           (values (box-object object) t))))

(defun ensure-boxed-objects (list)
  "Converts OBJ-LIST to a list where each item is either a object or a boxed object."
  ;; Maybe we should share structure for an all-object tail?
  (mapcar 'ensure-boxed-object list))
