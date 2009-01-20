(defpackage #:sheeple
  (:use :cl)
  (:export

   ;; Cloning and management
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
   :set-property
   :get-property
   :available-properties
   :remove-property
   :who-sets
   :has-property-p
   :has-direct-property-p
   ))

(defpackage #:sheeple-user
  (:use :cl :sheeple))
