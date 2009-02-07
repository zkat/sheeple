(defpackage #:sheeple
  (:use :cl :trivial-garbage)
  (:export

   ;; Cloning and management
   :=dolly= ;Ancestor of everything
   :clone
   :sheep-hierarchy-error ;error
   :mitosis-error ;error
   :probably-meant-to-be-option ;error
   :invalid-option-error ;error
   :sheep-p
   :add-parent
   :remove-parent
   :direct-parent-p
   :ancestor-p
   :direct-child-p
   :descendant-p
   
   ;; Introspection
   :sid
   :sheep-direct-parents
   :sheep-direct-roles
   :sheep-nickname

   ;; Property access
   :unbound-property ;error
   :get-property
   :available-properties
   :remove-property
   :who-sets
   :has-property-p
   :has-direct-property-p

   ;; Locking
   :locked-property ;error
   :property-locking-error ;error
   :property-locked-p
   :lock-property
   :unlock-property
   :toggle-lock
   :lock-sheep
   :unlock-sheep

   ;; Buzzwords
   :defbuzzword
   :defmessage
   :undefbuzzword
   :undefmesage
   :find-buzzword
   :participant-p
   :available-messages
   :sheepify
   :sheepify-list
   :message-pointer ;used on Roles to get actual message object
   :message-function ;used on message objects to access executable function
   :message-body ;Contains list-form of message

   ;; Fleeces -- wrappers around built-in types
   :fleece-of ; returns the appropriate fleece for a wolf. Good for getting an idea of what is what.
   :find-fleeced-wolf ; returns a fleeced version of object
   ;; built-in fleeces, these correspond directly to lisp types
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

   ;; MOP
   :standard-sheep
   :standard-sheep-property
   :sid
   :sheep-nickname
   :sheep-direct-parents
   :sheep-direct-children
   :sheep-direct-roles
   :sheep-hierarchy-list
   ))

(defpackage #:sheeple-user
  (:use :cl :sheeple))
