(in-package :sheeple-user)

;; Let's set up a basic 'class' hierarchy first.
(defsheep =entity= ()
  ((name "NoName")
   (eyes "Ordinary")
   (height "Ordinary")
   (ears "Ordinary")))

(defsheep =human= (=entity=)
  ((eyes "Blue")))

(defsheep =elf= (=entity=)
  ((height "Tall")
   (ears "Pointy")))

;; Now let's say we want to make these entities mate, and vary the results.
(defbuzzword mate (a b)
  (:documentation "Mates A and B, and returns their baby :)"))
(defmessage mate ((a =entity=) (b =entity=))
  (when (eql a b)
    (error "Can't mate two of the exact same entity!"))
  (clone (a b) () ; CLONE and DEFSHEEP are basically the same thing. CLONE just happens to return an anonymous object.
	 (:nickname (format nil "Child of ~a and ~a"
			    (name a)
			    (name b)))))

;; The objects created by DEFSHEEP and CLONE are the same. DEFSHEEP only really adds the convenience
;; of being able to redefine objects by reevaluating the form, but otherwise it's the same as this:
(defvar *edmond* (clone (=human=)
			((name "Edmond"))
			(:nickname "Eddy")))

(defvar *princess-rena* (clone (=elf=)
			       ((name "Rena")
				(title "Princess"))
			       (:nickname "Reni")))

;; Let's give the thild a name so it doesn't just end up taking its parent's name.
(defmessage mate :around ((a *edmond*) (b *princess-rena*))
	    (declare (ignore a b))
	    (let ((the-child (call-next-message)))
    (setf (name the-child)
	  "Eddie")
    (setf (title the-child)
	  "Little")
    the-child))

;; Princess Rena actually has a title. Maybe we want to be able to access the full name of an entity.
(defbuzzword full-name (entity))
(defmessage full-name ((entity =entity=))
  (name =entity=))
(defmessage full-name ((royalty *princess-rena*))
  (format nil "~a ~a" (title royalty) (name royalty)))

;; But it's a love that simply cannot be...
(defmessage mate ((a *edmond*) (b *princess-rena*))
  (declare (ignore a b))
  (error "NO! YOUR LOVE IS FORBIDDEN!!!11one"))
