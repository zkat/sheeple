(defpackage #:sheeple
  (:use :cl)
  (:export

   ;; Cloning and management
   :clone
   :add-parent
   :remove-parent

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
