;; This file is part of Sheeple

;; post-boot.lisp
;;
;; These are some things to run only after the whole thing has finished booting up.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :sheeple)

;; This seems to work. Keep an eye out.
(defbuzzword print-sheep "Defines the expression print-object uses.")
(defmessage print-sheep (sheep stream)
  (print-unreadable-object (sheep stream :identity t)
    (format stream "Standard Sheep SID: ~a~@[ AKA: ~a~]" (sid sheep) (sheep-nickname sheep))))
(defmethod print-object ((sheep standard-sheep) stream)
  (print-sheep sheep stream))

;; (defmethod print-object ((sheep standard-sheep) stream)
;;   (print-unreadable-object (sheep stream :identity t)
;;     (format stream "Standard Sheep SID: ~a~@[ AKA: ~a~]" (sid sheep) (sheep-nickname sheep))))
 
;; FIXME: This forces an autoboxing loop. Find where this can be fixed.
;; (defbuzzword run-cloneform "Runs a cloneform on a property.")
;; (defmessage run-cloneform (foo bar)
;;    (declare (ignorable foo bar))
;;    foo bar
;;    (values))
;; (defmethod set-up-property :before (property-list (sheep standard-sheep))
;;   "Run-cloneform runs the form defined for this property."
;;   (let ((prop-name (getf property-list :name)))
;;     (run-cloneform sheep prop-name)))
