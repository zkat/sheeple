;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; indent-tabs-mode: nil -*-

;;;; This file is part of Sheeple

;;;; boot/boxed.lisp
;;;;
;;;; Building the initial boxed object hierarchy
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :sheeple)

(defproto =boxed-object= =t=)

(macrolet ((define-boxed-objects (&body names)
             `(progn ,@(loop for (name . parents) in (mapcar 'ensure-list names)
                          collect `(defproto ,name ,(or parents '=boxed-object=))))))
  (define-boxed-objects
    =character= =function= =hash-table= =package= =pathname= =readtable=
    =stream= =sequence= =symbol= (=boolean= =symbol=) (=list= =sequence=)
    (=null= =symbol= =list=) (=cons= =list=) =array= (=vector= =array= =sequence=)
    (=bit-vector= =vector=) (=string= =vector=) =number= (=complex= =number=)
    (=integer= =number=) (=float= =number=)))
