;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; indent-tabs-mode: nil -*-

;;;; This file is part of Sheeple

;;;; builtins.lisp
;;;;
;;;; Boxing of built-in lisp types
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :sheeple)

(define-unbound-variables
    =boxed-object= =symbol= =sequence= =array= =number= =character= =function=
    =hash-table= =package= =pathname= =readtable= =stream= =list= =null= =cons=
    =vector= =bit-vector= =string= =complex= =integer= =float= =boolean=)

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
        :report (fun (format _ "Box ~S anyways." object)))
      (return-object ()
        :report (fun (format _ "Do not box ~S." object))
        (return-from box-object object))))
  (setf (gethash object *boxed-object-table*)
        (defobject ((box-type-of object))
            ((wrapped-object object))
          (:nickname object))))

(defun remove-boxed-object (object)
  "Kills object dead"
  (remhash object *boxed-object-table*))

(defun find-boxed-object (object &optional (errorp nil))
  "Finds a previously-boxed object in the boxed object table.
If ERRORP is T, this signals an error if OBJECT is a object, or if OBJECT
has not already been boxed."
  (if (objectp object)
      (when errorp (error "~S seems to already be a object." object))
      (multiple-value-bind (object hasp)
          (gethash object *boxed-object-table*)
        (if hasp object
            (when errorp (error "~S has not been boxed." object))))))

(declaim (inline ensure-dispatch-object))
(defun ensure-dispatch-object (object)
  "Ensures that OBJECT is a valid object for reply dispatch."
  (if (objectp object) object
      (or (find-boxed-object object)
          (box-type-of object))))

(defun objectify (object)
  "Returns two values: OBJECT or a boxed object representing it, and a boolean
specifying whether boxing took place."
  ;; I'm not sure that this is the right way to treat T... imo, it should box
  ;; to =boolean=; =T= isn't a boxed object.   - Adlai
  (cond ((eq object t)
         (values =t= nil))
        ((not (objectp object))
         (or (find-boxed-object object)
             (values (box-object object) t)))
        (t (values object nil))))

(defun objectify-list (list)
  "Converts OBJ-LIST to a list where each item is either a object or a boxed object."
  ;; Worst case scenario -- traverses a long list twice and conses up a complete copy
  ;; of the CDR when only the CAR needed to be boxed.
  ;; We could maybe make it better by sharing structure for an all-object tail. - Adlai
  (if (every 'objectp list) list (mapcar 'objectify list)))
