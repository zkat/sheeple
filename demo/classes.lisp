;; This is a demonstration of Sheeple usage.

;; Sheeple can be used, when necessary, in a style identical to class-based OOP.
;; The main purpose of this demo is not to demonstrate the advantages of using Sheeple
;; over CLOS, but simply its ability to perform tasks in a similar way to CLOS.
(in-package :sheeple-user)

(defsheep =person= ()
  ((name  "NoName")
   (phone "NoPhone")))

(defvar *charles*
  (clone (=person=)
	 ((name "Charles"))))

(defvar *jenny*
  (clone (=person=)
	 ((name "Jenny")
	  (phone "543-867-5309"))))

(defbuzzword greet (person)
  (:documentation "Greets a person"))
(defmessage greet ((person =person=))
  (format t "Hello, ~a" (name person)))
(defmessage greet ((person *jenny*))
  (format t "Hullo, ~a!!!" (name person)))

;; SHEEPLE-USER> (name =person=)
;; "NoName"
;; SHEEPLE-USER> (name *charles*)
;; "Charles"
;; SHEEPLE-USER> (greet *charles*)
;; Hello, Charles
;; NIL
;; SHEEPLE-USER> (greet *jenny*)
;; Hullo, Jenny!!!
;; NIL
;; SHEEPLE-USER> (phone *charles*)
;; "NoPhone"
;; SHEEPLE-USER> (phone *jenny*)
;; "543-543-5533"
;; SHEEPLE-USER> (setf (phone *charles*) "555-555-5555")
;; "555-555-5555"
;; SHEEPLE-USER> (phone =person=)
;; "NoPhone"
