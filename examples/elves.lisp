;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; indent-tabs-mode: nil -*-

(in-package :sheeple-user)

;; Let's set up a basic 'class' hierarchy first.
(defproto =entity= ()
  ((name "NoName")
   (eyes "Ordinary")
   (height "Ordinary")
   (ears "Ordinary")))

;; It's good to note that accessors are being automatically defined for each property.
(defproto =human= (=entity=)
  ((eyes "Blue")))

(defproto =elf= (=entity=)
  ((height "Tall")
   (ears "Pointy")))

;; Now let's say we want to make these entities mate, and vary the results.
(defmessage mate (a b)
  (:documentation "Mates A and B, and returns their baby :)")
  (:reply ((a =entity=) (b =entity=))
    ;; this reply will only dispatch if the item in position 'a' is an =entity=, AND 'b'
    ;; is also an =entity=. Otherwise, it will signal an error.
    (when (eql a b)
      (error "Can't mate two of the exact same entity!"))
    ;; OBJECT and DEFPROTO return the same kind of object.
    ;; The difference is that defproto forms can be re-evaluated to 'redefine' a prototype
    ;; while maintaining its identity.
    (object :parents (list a b))))

;; When OBJECT isn't enough, there's DEFOBJECT, which works just like DEFPROTO except the object
;; it creates is 'anonymous'. It also does not automatically create new accessors. You must
;; explicitly pass it the :accessor option in order to do that.
(defvar *edmond*
  (defobject =human=
      ((name "Edmond"))
    :nickname "Eddy")) ; Nicknames are displayed when the object is printed.

(defvar *princess-renee*
  (defobject =elf=
      ((name "Renee")
       (title "Princess" :accessor 'title))
    :nickname "Reni"))

;; Let's give the child a name so it doesn't just end up taking its parent's name.
(defreply mate :around ((a *edmond*) (b *princess-renee*))
  ;; note that the second item in the specialized LL entry
  ;; can be any object. It doesn't have to be something defined with defproto.
  (let ((the-child (call-next-reply)))
    (setf (title the-child) "Little")
    the-child))

;; Princess Renee actually has a title. Maybe we want to be able to access the full name of an entity.
(defmessage full-name (entity)
  (:reply ((entity =entity=))
    (name entity)))
;; We might be tempted to do something like this, but maybe that's not the right thing to express...
;; (defreply full-name ((royalty *princess-renee*))
;;   (format nil "~a ~a" (title royalty) (name royalty)))
;; So let's do this instead...
(defproto =royalty= (=entity=) ())
(defreply full-name ((entity =royalty=))
  (format nil "~A ~A" (title entity) (name entity)))
;; A mixin?! BRILLIANT!
;; And we know how to use mixins, don't we kids? Except these guys work on instances :)
(push =royalty= (object-parents *princess-renee*))

;; But it's a love that simply cannot be...
(defreply mate ((a *edmond*) (b *princess-renee*))
  (error "NO! YOUR LOVE IS FORBIDDEN!!!11one"))
