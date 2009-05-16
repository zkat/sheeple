(defpackage #:sheeple
  (:use :cl :trivial-garbage)
  (:export

   ;; Cloning and management
   :=t=
   :=dolly=
   :clone
   :clone*
   :defsheep
   :sheep-hierarchy-error
   :sheep-p
   :add-parent
   :remove-parent
   :direct-parent-p
   :ancestor-p
   :direct-child-p
   :descendant-p
   :sheeple-error ;general error
   :sheeple-warning ;general warning
   
   ;; Property access
   :unbound-property ;error
   :property-value
   :available-properties
   :remove-property
   :property-owner
   :has-property-p
   :has-direct-property-p
   :with-properties
   :with-manipulators

   ;; Cloneforms
   :available-cloneforms
   :cloneform-owner
   :add-cloneform
   :remove-cloneform
   :inspect-cloneform
   
   ;; Buzzwords
   :clobbering-function-definition
   :defbuzzword
   :defmessage
   :available-messages
   :undefbuzzword
   :undefmessage
   :participant-p
   :call-next-message
   :next-message-p
   :clobbering-function-definition ;warning
   :buzzword-lambda-list-error ;error when something is wrong with a lambda list
   :no-applicable-messages ;error
   :no-most-specific-message ;error
   :no-primary-messages ;error (signaled when the only applicable messages are combination messages)
   :specialized-lambda-list-error ;error

   ;; Fleeces
   :fleece-of ;returns the appropriate sheep for a wolf. Good for getting an idea of what is what.
   :find-fleeced-wolf ;returns a fleeced version of a wolf
   ;; :sheepify
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
   :initialize-sheep
   :reinitialize-sheep
   :print-sheep
   :sheep-nickname
   :sheep-hierarchy-list ;I leave this because it might be useful during development.
   ;; :sheep-direct-parents
   ;; :sheep-direct-roles
   ))

(defpackage #:sheeple-user
  (:use :cl :sheeple))