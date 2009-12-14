;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; indent-tabs-mode: nil -*-

;; This is a demonstration of Sheeple usage.

;; Sheeple can be used, when necessary, in a style identical to class-based OOP.
;; The main purpose of this demo is not to demonstrate the advantages of using Sheeple
;; over CLOS, but simply its ability to perform tasks in a similar way to CLOS.
(in-package :sheeple-user)

(defproto =person= ()
  ((name  "NoName")
   (phone "NoPhone#")))

(defvar *charles* (create =person= 'name "Charles"))

(defvar *jenny* (create =person=))

(setf (name *jenny*) "Jenny")
(setf (phone *jenny*) "543-867-5309")

(defmessage greet (person)
  (:documentation "Greets a person")
  (:reply ((person =person=))
    (format t "Hello, ~a" (name person)))
  (:reply ((person *jenny*))
    (format t "Hullo, ~a!!!" (name person))))

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
;; "543-867-5309"
;; SHEEPLE-USER> (setf (phone *charles*) "555-555-5555")
;; "555-555-5555"
;; SHEEPLE-USER> (phone =person=)
;; "NoPhone#"
