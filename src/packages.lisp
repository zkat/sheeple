(defpackage #:sheeple
  (:use :cl :trivial-garbage)
  (:export

   ;; Cloning and management
   :clone
   :defclone
   :defproto
   :spawn-sheep
   :sheep-hierarchy-error
   :sheep-p
   :add-parent
   :remove-parent
   :ancestor-p
   :parent-p
   :descendant-p
   :child-p
   :sheeple-error ;general error
   :sheeple-warning ;general warning
   
   ;; Property access
   :unbound-property ;error
   :add-property
   :property-value
   :direct-property-value
   :available-properties
   :remove-property
   :remove-all-direct-properties
   :property-owner
   :has-property-p
   :has-direct-property-p
   :with-properties

   ;; Messages
   :clobbering-function-definition
   :defmessage
   :defreply
   :available-replies
   :undefmessage
   :undefreply
   :participant-p
   :call-next-reply
   :next-reply-p
   :clobbering-function-definition ;warning
   :message-lambda-list-error ;error when something is wrong with a lambda list
   :no-applicable-replies ;error
   :no-most-specific-reply ;error
   :no-primary-replies ;error (signaled when the only applicable replies are combination replies)
   :specialized-lambda-list-error ;error

   ;; built-ins
   :box-type-of ;returns the appropriate sheep for a wolf. Good for getting an idea of what is what.
   :find-boxed-object ;returns a fleeced version of a wolf
   :sheepify

   ;; MOP
   :standard-sheep
   :init-sheep
   :reinit-sheep
   :spawn-sheep
   :finalize-sheep
   :print-sheep
   :sheep-nickname
   :sheep-hierarchy-list
   :sheep-parents
   :sheep-direct-roles
   :sheep-direct-properties
   :sheep-documentation
   :direct-property-spec
   :property-spec-name
   :property-spec-value
   :property-spec-readers
   :property-spec-writers
   :property-summary
   :direct-property-summary
   ))

(defpackage #:sheeple-user
  (:use :cl :sheeple))