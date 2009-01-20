(defpackage #:sheeple
  (:use :cl)
  (:export

   ;; Cloning and management
   :clone
   :add-parent
   :remove-parent
   
   ;; Introspection
   :sheep-id
   :sheep-direct-parents

   ;; Property access
   :set-property
   :get-property
   :available-properties
   :remove-property
   :who-sets
   :has-direct-property-p
   ))

(defpackage #:sheeple-user
  (:use :cl :sheeple))
