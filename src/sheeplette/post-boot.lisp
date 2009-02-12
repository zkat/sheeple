;;;
;;; Safe to define buzzwords and messages now, as well as use CLONE normally
;;;
(defbuzzword sheep-p-using-sheep)
(defmessage sheep-p-using-sheep (sheep)
  (std-sheep-object-p sheep))

(defbuzzword print-sheep
    (:documentation "Defines the expression print-object uses."))
(defmessage print-sheep (sheep stream)
  (print-unreadable-object (sheep stream :identity t)
    (format stream "Standard Sheep SID: ~a~@[ AKA: ~a~]" (sid sheep) (sheep-nickname sheep))))
(defmethod print-object :around (sheep stream)
  (if (sheep-p sheep)
      (print-sheep sheep stream)
      (call-next-method)))

;;; Property access again
(defbuzzword get-property-using-metasheep)
(defmessage get-property-using-metasheep ((metasheep =standard-sheep-metasheep=) sheep property-name)
  (declare (ignore metasheep))
  (std-get-property sheep property-name))

(defbuzzword setf-get-property-using-metasheep
    (:documentation "Sets the property, dispatching on the metasheep."))
(defmessage setf-get-property-using-metasheep (new-value
					       (metasheep =standard-sheep-metasheep=) 
					       sheep property-name)
  (declare (ignore metasheep))
  (setf (std-get-property sheep property-name) new-value))


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

;;; lol i dunno
(defbuzzword generate-sheep
    (:documentation "Creates a new sheep object."))
