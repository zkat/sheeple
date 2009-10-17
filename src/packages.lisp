;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; indent-tabs-mode: nil -*-

(defpackage #:%sheeple-garbage
  (:use :cl)
  (:shadow :make-hash-table)
  (:export :gc
           :make-weak-pointer
           :weak-pointer-value
           :weak-pointer-p
           :make-weak-hash-table
           :hash-table-weakness
           :finalize
           :cancel-finalization))

(defpackage #:sheeple
  (:use :cl :%sheeple-garbage)
  (:export

   ;; Cloning and management
   :clone
   :defclone
   :defproto
   :objectp
   :object-parents
   :add-parent
   :remove-parent
   :ancestorp
   :parentp
   :descendantp
   :childp
   :sheeple-hierarchy-error ;something went wrong with the hierarchy list!
   :sheeple-error                       ;general error
   :sheeple-warning                     ;general warning
   :object-metaobject

   ;; ;; Property access
   ;; :unbound-property                    ;error
   ;; :add-property
   ;; :property-value
   ;; :direct-property-value
   ;; :available-properties
   ;; :remove-property
   ;; :remove-all-direct-properties
   ;; :property-owner
   ;; :has-property-p
   ;; :has-direct-property-p
   ;; :with-properties

   ;; ;; Messages
   ;; :clobbering-function-definition
   ;; :defmessage
   ;; :defreply
   ;; :available-replies
   ;; :undefmessage
   ;; :undefreply
   ;; :participant-p
   ;; :call-next-reply
   ;; :next-reply-p
   ;; :clobbering-function-definition      ;warning
   ;; :message-lambda-list-error           ;error when something is wrong with a lambda list
   ;; :no-applicable-replies               ;error
   ;; :no-most-specific-reply              ;error
   ;; :no-primary-replies    ;error (signaled when the only applicable replies are combination replies)
   ;; :specialized-lambda-list-error       ;error

   ;; Protos
   :=standard-object=
   :=t=
   :=null=
   :=symbol=
   :=complex=
   :=integer=
   :=float=
   :=cons=
   :=character=
   :=hash-table=
   :=package=
   :=pathname=
   :=readtable=
   :=stream=
   :=number=
   :=string=
   :=bit-vector=
   :=vector=
   :=array=
   :=function=
   :=boxed-object=

   ;; ;; built-ins
   ;; :box-type-of ;returns the appropriate sheep for a wolf. Good for getting an idea of what is what.
   ;; :find-boxed-object                   ;returns a fleeced version of a wolf
   ;; :sheepify

   ;; ;; MOP
   ;; :standard-sheep
   ;; :init-sheep
   ;; :reinit-sheep
   ;; :spawn-sheep
   ;; :finalize-sheep
   ;; :print-sheep
   ;; :sheep-nickname
   ;; :sheep-hierarchy-list
   ;; :sheep-parents
   ;; :sheep-direct-roles
   ;; :sheep-direct-properties
   ;; :sheep-documentation
   ;; ;; properties
   ;; :standard-property
   ;; :add-property-using-property-metaobject
   ;; :direct-property-metaobject
   ;; :property-name
   ;; :property-value
   ;; :property-readers
   ;; :property-writers
   ;; :property-summary
   ;; :direct-property-summary
   ))

(defpackage #:sheeple-user
  (:use :cl :sheeple))
