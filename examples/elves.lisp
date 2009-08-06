;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; indent-tabs-mode: nil -*-

(in-package :sheeple-user)

;; Let's set up a basic 'class' hierarchy first.
(defproto entity ()
  ((name "NoName")
   (eyes "Ordinary")
   (height "Ordinary")
   (ears "Ordinary")))

;; It's good to note that accessors are being automatically defined for each property.
(defproto human ((proto 'entity))
  ((eyes "Blue")))

(defproto elf ((proto 'entity))
  ((height "Tall")
   (ears "Pointy")))

;; Now let's say we want to make these entities mate, and vary the results.
(defmessage mate (a b)
  (:documentation "Mates A and B, and returns their baby :)"))
(defreply mate ((a (proto 'entity)) (b (proto 'entity)))
  ;; this reply will only dispatch if the item in position 'a' is an #@entity, AND 'b'
  ;; is also an #@entity. Otherwise, it will signal a condition.
  (when (eql a b)
    (error "Can't mate two of the exact same entity!"))
  ;; CLONE and DEFPROTO return the same kind of object (a sheep)
  ;; The difference is that defproto forms can be re-evaluated to 'redefine' a prototype
  ;; while maintaining its identity. Prototypes defined with defproto reside in their own
  ;; namespace, and must be fetched with either (find-proto 'name), or the readmacro #@name
  (clone a b)
  ;; You might notice that clone only really accepts sheep objects, so you can't define properties
  ;; and options in one go....
  )

;; When CLONE isn't enough, there's DEFCLONE, which works just like DEFPROTO except the sheep
;; it creates is 'anonymous'. It also does not automatically create new accessors. You must
;; explicitly pass it the :accessor option in order to do that.
(defvar *edmond* (defclone ((proto 'human))
                     ((name "Edmond"))
                   (:nickname "Eddy"))) ; Nicknames are displayed when the sheep object is printed.

(defvar *princess-rena* (defclone ((proto 'elf))
                            ((name "Rena")
                             (title "Princess"))
                          (:nickname "Reni")))

;; Let's give the child a name so it doesn't just end up taking its parent's name.
(defreply mate :around ((a *edmond*) (b *princess-rena*))
  ;; note that the second item in the LL
  ;; can be any object. It doesn't have to be a proto.
  (declare (ignore a b))
  (let ((the-child (call-next-message)))
    (setf (name the-child)
          "Eddie")
    (setf (title the-child)
          "Little")
    the-child))

;; Princess Rena actually has a title. Maybe we want to be able to access the full name of an entity.
(defmessage full-name (entity))
(defreply full-name ((entity (proto 'entity)))
  (name entity))
(defreply full-name ((royalty *princess-rena*))
  (format nil "~a ~a" (title royalty) (name royalty)))

;; But it's a love that simply cannot be...
(defreply mate ((a *edmond*) (b *princess-rena*))
  (declare (ignore a b))
  (error "NO! YOUR LOVE IS FORBIDDEN!!!11one"))
