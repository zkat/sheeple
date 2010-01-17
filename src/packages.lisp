;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; indent-tabs-mode: nil -*-

(defpackage #:%sheeple-garbage
  (:use :cl)
  (:shadow :make-hash-table)
  (:export
   #:cancel-finalization
   #:finalize
   #:gc
   #:hash-table-weakness
   #:make-weak-hash-table
   #:make-weak-pointer
   #:maybe-make-weak-pointer
   #:maybe-weak-pointer-value
   #:weak-pointer
   #:weak-pointer-p
   #:weak-pointer-value
   ))

(defpackage #:sheeple
  (:use :cl :%sheeple-garbage)
  (:export

   ;; Objects
   #:ancestorp
   #:childp
   #:clone
   #:create
   #:make
   #:defobject
   #:defproto
   #:descendantp
   #:init-object
   #:object
   #:object-precedence-list
   #:object-metaobject
   #:object-nickname
   #:object-parents
   #:objectp
   #:parentp
   #:print-sheeple-object
   #:reinit-object
   #:shared-init

   ;; Property access
   #:available-properties
   #:direct-properties
   #:direct-property-p
   #:direct-property-value
   #:property-makunbound
   #:property-value
   #:remove-all-direct-properties
   #:remove-property
   #:with-properties

   ;; Messages
   #:call-next-reply
   #:available-replies
   #:defmessage
   #:defreply
   #:next-reply-p
   #:participantp
   #:undefmessage
   #:undefreply

   ;; Protos
   #:=array=
   #:=bit-vector=
   #:=boxed-object=
   #:=character=
   #:=complex=
   #:=cons=
   #:=float=
   #:=function=
   #:=hash-table=
   #:=integer=
   #:=list=
   #:=null=
   #:=number=
   #:=package=
   #:=pathname=
   #:=readtable=
   #:=sequence=
   #:=standard-metaobject=
   #:=standard-object=
   #:=stream=
   #:=string=
   #:=symbol=
   #:=t=
   #:=vector=

   ;; built-ins
   #:box-type-of
   #:find-boxed-object
   #:objectify

   ;; Conditions
   #:automatic-message-creation
   #:clobbering-function-definition
   #:insufficient-message-args
   #:message-lambda-list-error
   #:no-applicable-reply
   #:no-next-reply
   #:no-primary-reply
   #:no-such-message
   #:object-precedence-error
   #:object-property-error
   #:reply-argument-conflict
   #:reply-lambda-list-conflict
   #:sheeple-error
   #:sheeple-message-error
   #:sheeple-reply-error
   #:sheeple-warning
   #:specialized-lambda-list-error
   #:topological-sort-conflict
   #:unbound-property
   #:unbound-direct-property
   ))

(defpackage #:sheeple-metaobject-protocol
  (:use :cl)
  (:nicknames :smop :sheeple-mop)
  (:export
   ;; Objects
   #:allocate-object
   #:compute-object-precedence-list
   #:validate-parent
   #:object-metaobject

   ;; Properties
   :available-properties
   :direct-properties
   :direct-property-p
   :direct-property-value
   :property-makunbound
   :property-owner
   :property-value
   :add-direct-property
   :remove-all-direct-properties
   :remove-all-direct-properties
   :remove-property
   ))

(defpackage #:sheeple-user
  (:use :cl :sheeple))
