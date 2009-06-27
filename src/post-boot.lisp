;; This file is part of Sheeple

;; post-boot.lisp
;;
;; Once sheeple is booted up, we can define messages/replies normally
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :sheeple)

;;; with-*
(defmacro with-properties (properties sheep &body body)
  (let ((sh (gensym)))
    `(let ((,sh ,sheep))
       (symbol-macrolet ,(mapcar (lambda (property-entry)
				   (let ((var-name
					  (if (symbolp property-entry)
					      property-entry
					      (car property-entry)))
					 (property-name
					  (if (symbolp property-entry)
					      property-entry
					      (cadr property-entry))))
				     `(,var-name
				       (property-value ,sh ',property-name))))
				 properties)
	 ,@body))))

(defmacro with-manipulators (properties sheep &body body)
  (let ((sh (gensym)))
    `(let ((,sh ,sheep))
       (symbol-macrolet ,(mapcar (lambda (property-entry)
				   (let ((var-name (car property-entry))
					 (manipulator-name (cadr property-entry)))
				     `(,var-name (,manipulator-name ,sh))))
				 properties)
	 ,@body))))


;;;
;;; Printing sheep!
;;;
(defmessage print-sheep (sheep stream)
  (:documentation "Defines the expression print-object uses."))
(defreply print-sheep (sheep (stream #@stream))
  (print-unreadable-object (sheep stream :identity t)
    (format stream "Sheep~@[ AKA: ~a~]" (sheep-nickname sheep))))
(defreply print-sheep ((sheep #@boxed-object) (stream #@stream))
  (print-unreadable-object (sheep stream :identity t)
    (format stream "Boxed object~@[ AKA: ~a~]" (sheep-nickname sheep))))

