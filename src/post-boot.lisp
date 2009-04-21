;; This file is part of Sheeple

;; post-boot.lisp
;;
;; Once sheeple is booted up, we can define buzzwords/messages normally
;;
;; TODO:
;; * Figure out why the hell this isn't always loading.
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

;;; Sheep creation

;; (defbuzzword initialize-sheep (metasheep-prototype 
;; 			       &rest all-keys))
;; (defmessage initialize-sheep ((metaproto =standard-sheep-metasheep=)
;; 			      &rest all-keys)
;;   (apply #'std-initialize-sheep metaproto all-keys))

;; (defbuzzword finalize-sheep-using-metasheep (metasheep sheep))
;; (defmessage finalize-sheep-using-metasheep ((metasheep =standard-sheep-metasheep=)
;; 					    sheep)
;;   (std-finalize-sheep sheep))

;;;
;;; Printing sheep!
;;;
(defbuzzword print-sheep (sheep stream)
  (:documentation "Defines the expression print-object uses."))
(defmessage print-sheep (sheep stream)
  (print-unreadable-object (sheep stream :identity t)
    (format stream "Standard Sheep~@[ AKA: ~a~]" (sheep-nickname sheep))))
(defmessage print-sheep ((sheep =white-fang=) stream)
  (print-unreadable-object (sheep stream :identity t)
    (format stream "Fleeced Wolf~@[ AKA: ~a~]" (sheep-nickname sheep))))

;;; buzzwords/messages
(defun participant-p (sheep message-name)
  (when (member-if (lambda (role) (equal message-name (role-name role)))
		   (sheep-direct-roles sheep))
    t))
