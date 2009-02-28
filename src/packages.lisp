(defpackage #:sheeple
  (:use :cl :trivial-garbage)
  (:export

   ;; Cloning and management
   :=t=
   :=dolly=
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
   :with-properties
   :with-manipulators

   ;; Cloneforms
   :available-cloneforms
   :cloneform-owner
   :add-cloneform
   :remove-cloneform
   :get-cloneform ; don't setf this, please

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
   :=white-fang= ; parent of all built-in fleeces
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
   :print-sheep

   ;; Sheep creation
   :initialize-sheep ; shitsux
   :finalize-sheep-using-metasheep
   :add-parent-using-metasheeple
   :remove-parent-using-metasheeple

   ;; Properties
   :property-value-using-metasheep
   :setf-property-value-using-metasheep
   :get-cloneform-using-metasheep
   :setf-get-cloneform-using-metasheep
   :get-clonefunction-using-metasheep
   :setf-get-clonefunction-using-metasheep
   :remove-property-using-metasheep
   :has-direct-property-p-using-metasheep
   :who-sets-using-metasheep
   :available-properties-using-metasheep
   :available-cloneforms-using-metasheep
   ))

(defpackage #:sheeple-user
  (:use :cl :sheeple))