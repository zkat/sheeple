;; This file is part of Sheeple

;; post-boot.lisp
;;
;; Once sheeple is booted up, we can define buzzwords/messages normally
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :sheeple)

;;;
;;; Printing sheep!
;;;
(defbuzzword print-sheep
    (:documentation "Defines the expression print-object uses."))
(defmessage print-sheep (sheep stream)
  (print-unreadable-object (sheep stream :identity t)
    (format stream "Standard Sheep~@[ AKA: ~a~]" (sheep-nickname sheep))))
(defmessage print-sheep ((sheep =white-fang=) stream)
  (print-unreadable-object (sheep stream :identity t)
    (format stream "Fleeced Wolf~@[ AKA: ~a~]" (sheep-nickname sheep))))
(defmessage print-sheep ((sheep =standard-sheep-metasheep=) stream)
  (print-unreadable-object (sheep stream :identity t)
    (format stream "Standard Metasheep~@[ AKA: ~a~]" (sheep-nickname sheep))))
(defmessage print-sheep ((sheep =standard-buzzword-metasheep=) stream)
  (print-unreadable-object (sheep stream :identity t)
    (format stream "Standard Buzzword: ~a" (buzzword-name sheep))))
(defmessage print-sheep ((sheep =standard-message-metasheep=) stream)
  (print-unreadable-object (sheep stream :identity t)
    (format stream "Standard Message: ~a" (message-name sheep))))
(defmessage print-sheep ((sheep =standard-role-metasheep=) stream)
  (print-unreadable-object (sheep stream :identity t)
    (format stream "Standard Role: ~a" (role-name sheep))))

;;; Property access
(defbuzzword property-value-using-metasheep)
(defmessage property-value-using-metasheep ((metasheep =standard-sheep-metasheep=) sheep property-name)
  (declare (ignore metasheep))
  (std-property-value sheep property-name))

(defbuzzword setf-property-value-using-metasheep
    (:documentation "Sets the property, dispatching on the metasheep."))
(defmessage setf-property-value-using-metasheep (new-value
						 (metasheep =standard-sheep-metasheep=) 
						 sheep property-name)
  (declare (ignore metasheep))
  (setf (std-property-value sheep property-name) new-value))


(defbuzzword get-cloneform-using-metasheep)
(defmessage get-cloneform-using-metasheep ((metasheep =standard-sheep-metasheep=) sheep property-name)
  (declare (ignore metasheep))
  (std-get-cloneform sheep property-name))

(defbuzzword setf-get-cloneform-using-metasheep
    (:documentation "Sets the cloneform, dispatching on the metasheep."))
(defmessage setf-get-cloneform-using-metasheep (new-value
						(metasheep =standard-sheep-metasheep=) 
						sheep property-name)
  (declare (ignore metasheep))
  (setf (std-get-cloneform sheep property-name) new-value))

(defbuzzword get-clonefunction-using-metasheep)
(defmessage get-clonefunction-using-metasheep ((metasheep =standard-sheep-metasheep=) sheep property-name)
  (declare (ignore metasheep))
  (std-get-clonefunction sheep property-name))

(defbuzzword setf-get-clonefunction-using-metasheep
    (:documentation "Sets the clonefunction, dispatching on the metasheep."))
(defmessage setf-get-clonefunction-using-metasheep (new-value
						    (metasheep =standard-sheep-metasheep=) 
						    sheep property-name)
  (declare (ignore metasheep))
  (setf (std-get-clonefunction sheep property-name) new-value))

(defbuzzword remove-property-using-metasheep
    (:documentation "Locally removes the specified property"))
(defmessage remove-property-using-metasheep ((metasheep =standard-sheep-metasheep=)
					     sheep property-name)
  (declare (ignore metasheep))
  (std-remove-property sheep property-name))

(defbuzzword has-direct-property-p-using-metasheep
    (:documentation "Returns T if the specified property is present locally."))
(defmessage has-direct-property-p-using-metasheep ((metasheep =standard-sheep-metasheep=)
						   sheep property-name)
  (declare (ignore metasheep))
  (std-has-direct-property-p sheep property-name))

(defbuzzword who-sets-using-metasheep
    (:documentation "Returns the sheep object that SHEEP inherits the property-value from."))
(defmessage who-sets-using-metasheep ((metasheep =standard-sheep-metasheep=)
				      sheep property-name)
  (declare (ignore metasheep))
  (std-who-sets sheep property-name))

(defbuzzword available-properties-using-metasheep
    (:documentation "Returns a list of symbols of available properties."))
(defmessage available-properties-using-metasheep ((metasheep =standard-sheep-metasheep=)
						  sheep)
  (declare (ignore metasheep))
  (std-available-properties sheep))

(defbuzzword available-cloneforms-using-metasheep
    (:documentation "Returns a list of symbols of available cloneforms."))
(defmessage available-cloneforms-using-metasheep ((metasheep =standard-sheep-metasheep=)
						  sheep)
  (declare (ignore metasheep))
  (std-available-cloneforms sheep))

;;; buzzwords/messages
(defun participant-p (sheep message-name)
  (when (member-if (lambda (role) (equal message-name (role-name role)))
		   (sheep-direct-roles sheep))
    t))

(defbuzzword buzzword-p)
(defmessage buzzword-p (anything-else)
  (declare (ignore anything-else))
  nil)
(defmessage buzzword-p ((buzzword =standard-buzzword-metasheep=))
  (declare (ignore buzzword))
  t)

(defbuzzword message-p)
(defmessage message-p (anything-else)
  (declare (ignore anything-else))
  nil)
(defmessage message-p ((message =standard-message-metasheep=))
  (declare (ignore message))
  t)

(defbuzzword available-messages-using-metasheep)
(defmessage available-messages-using-metasheep ((metasheep =standard-sheep-metasheep=) sheep)
  (declare (ignore metasheep))
  (std-available-messages sheep))
