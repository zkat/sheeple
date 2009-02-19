(defpackage #:sheeple
  (:use :cl :trivial-garbage)
  (:export

   ;; Cloning and management
   :=t=
   :=dolly=
   :print-sheep
   :print-object
   :clone
   :sheep-hierarchy-error
   :sheep-p
   :add-parent
   :remove-parent
   :direct-parent-p
   :ancestor-p
   :direct-child-p
   :descendant-p

   ;; Property access
   :unbound-property ;error
   :property-value
   :available-properties
   :remove-property
   :who-sets
   :has-property-p
   :has-direct-property-p
   
   ;; Buzzwords
   :clobbering-function-definition
   :defbuzzword
   :defmessage
   :undefbuzzword
   :undefmessage
   :participant-p
   :available-messages
   :sheepify

   ;; Fleeces
   :fleece-of ;returns the appropriate sheep for a wolf. Good for getting an idea of what is what.
   :find-fleeced-wolf ;returns a fleeced version of a wolf
   ;; built-in fleeces
   :=white-fang=
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

   ;; MOP-related
   ;; It's a little small right now...
   :sheep-direct-parents
   :sheep-direct-roles
   :sheep-nickname
   :sheep-hierarchy-list
   ))

(defpackage #:sheeple-user
  (:use :cl :sheeple))