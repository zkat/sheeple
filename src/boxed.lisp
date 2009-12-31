;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; indent-tabs-mode: nil -*-

;;;; This file is part of Sheeple

;;;; boot/boxed.lisp
;;;;
;;;; Building the initial boxed object hierarchy
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :sheeple)

(defproto =boxed-object= =t=)

(macrolet ((defboxed (name &rest parents)
             `(defproto ,name ,(or parents '=boxed-object=))))
  (defboxed =character=)
  (defboxed =function=)
  (defboxed =hash-table=)
  (defboxed =package=)
  (defboxed =pathname=)
  (defboxed =readtable=)
  (defboxed =stream=)
  (defboxed =sequence=)
  (defboxed =symbol=)
  (defboxed =boolean=    =symbol=)
  (defboxed =list=       =sequence=)
  (defboxed =null=       =symbol= =list=)
  (defboxed =cons=       =list=)
  (defboxed =array=)
  (defboxed =vector=     =array= =sequence=)
  (defboxed =bit-vector= =vector=)
  (defboxed =string=     =vector=)
  (defboxed =number=)
  (defboxed =complex=    =number=)
  (defboxed =integer=    =number=)
  (defboxed =float=      =number=))
