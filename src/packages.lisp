(defpackage #:sheeple
  (:use :cl)
  (:export

   ;; Cloning and management
   :=dolly=
   :clone
   :sheep-p
   :add-parent
   :remove-parent
   :direct-parent-p
   :ancestop-p
   :direct-child-p
   :descendant-p
   
   ;; Introspection
   :sid
   :sheep-direct-parents

   ;; Property access
   :get-property
   :available-properties
   :remove-property
   :who-sets
   :has-property-p
   :has-direct-property-p

   ;; Buzzwords
   :defbuzzword
   :defmessage
   :find-buzzword
   :sheepify
   :sheepify-list

   ;; Fleeces
   :fleece-of
   :=symbol=
   :=sequence=
   :=array=
   :=number=
   :=character=
   :=function=
   :=hash-table=
   :=package=
   :=pathname=
   :=readtable=
   :=stream=
   :=list=
   :=null=
   :=cons=
   :=vector=
   :=bit-vector=
   :=string=
   :=complex=
   :=integer=
   :=float=

   
   ))

(defpackage #:sheeple-user
  (:use :cl :sheeple))
