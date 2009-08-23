;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; indent-tabs-mode: nil -*-

;;;; This file is part of Sheeple

;;;; builtins.lisp
;;;;
;;;; Boxing of built-in lisp types
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :sheeple)
(declaim (optimize (speed 3) (safety 1)))

(defmacro define-variables (&body variables)
  `(progn ,@(mapcar (fun `(defvar ,_ (gensym (symbol-name ',_)))) variables)))

(eval-when (:compile-toplevel :load-toplevel :execute)
 (define-variables =boxed-object= =symbol= =sequence= =array= =number= =character= =function=
                   =hash-table= =package= =pathname= =readtable= =stream= =list= =null= =cons=
                   =vector= =bit-vector= =string= =complex= =integer= =float=))

(defun box-type-of (x)
  (if (sheepp x)
      (progn
        (warn "This is already a sheep!")
        x)
      (typecase x
        (null                                          =null=)
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
        (t                                             =boxed-object=))))

(defvar *boxed-object-table* (make-hash-table :test #'equal))

(defun find-boxed-object (object &optional (errorp nil))
  "Finds a previously-boxed object in the boxed object table.
If ERRORP is T, this signals an error if OBJECT is a sheep, or if OBJECT
has not already been boxed."
  (if (sheepp object)
      (if errorp (error "~S seems to already be a sheep." object) nil)
      (multiple-value-bind (sheep hasp)
          (gethash object *boxed-object-table*)
        (if hasp
            sheep
            (if errorp (error "~S has not been boxed." object) nil)))))

(defun box-object (object)
  "Wraps OBJECT with a sheep."
  (if (sheepp object)
      (error "~S seems to already be a sheep." object)
      (setf (gethash object *boxed-object-table*)
            (defsheep ((box-type-of object))
                ((wrapped-object object)) (:nickname object)))))

(defun remove-boxed-object (object)
  "Kills object dead"
  (remhash object *boxed-object-table*))

(defun sheepify (object)
  "Returns OBJECT or boxes it."
  (cond ((eq object t)
         =t=)
        ((not (sheepp object))
         (or (find-boxed-object object)
             (values (box-object object) t)))
        (t
         (values object nil))))

(defun sheepify-list (obj-list)
  "Converts OBJ-LIST to a list where each item is either a sheep or a boxed object."
  (mapcar #'sheepify obj-list))
