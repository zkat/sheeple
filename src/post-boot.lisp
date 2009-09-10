;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; indent-tabs-mode: nil -*-

;;;; This file is part of Sheeple

;;;; post-boot.lisp
;;;;
;;;; Once sheeple is booted up, we can define messages/replies normally
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :sheeple)

;;;
;;; Sheep creation protocol
;;;
(defmessage allocate-sheep (metasheep))
(defreply allocate-sheep ((metasheep =standard-metasheep=))
  (std-allocate-sheep metasheep))

(defmessage compute-sheep-hierarchy-list-using-metasheep (metasheep sheep))
(defreply compute-sheep-hierarchy-list-using-metasheep
    ((metasheep =standard-metasheep=) sheep)
  (std-compute-sheep-hierarchy-list sheep))

(defmessage finalize-sheep-inheritance-using-metasheep (metasheep sheep))
(defreply finalize-sheep-inheritance-using-metasheep
    ((metasheep =standard-metasheep=) sheep)
  (std-finalize-sheep-inheritance sheep))

(defmessage remove-parent-using-metasheeple (parent-metasheep child-metasheep parent child))
(defreply remove-parent-using-metasheeple ((parent-metasheep =standard-metasheep=)
                                           (child-metasheep =standard-metasheep=)
                                           parent child)
  (std-remove-parent parent child))

(defmessage add-parent-using-metasheeple (parent-metasheep child-metasheep parent child))
(defreply add-parent-using-metasheeple ((parent-metasheep =standard-metasheep=)
                                        (child-metasheep =standard-metasheep=)
                                        parent child)
  (std-add-parent parent child))


;;;
;;; Printing sheep!
;;;
(defmessage print-sheep (sheep stream)
  (:documentation "Defines the expression print-object uses."))
(defreply print-sheep (sheep (stream =stream=))
  (print-unreadable-object (sheep stream :identity t)
    (format stream "Sheep~@[ AKA: ~a~]" (sheep-nickname sheep))))
(defreply print-sheep ((sheep =boxed-object=) (stream =stream=))
  (print-unreadable-object (sheep stream :identity t)
    (format stream "Boxed object~@[ AKA: ~a~]" (sheep-nickname sheep))))

;; todo: convert this properly
(defmethod print-object ((sheep standard-sheep) stream)
  (if (null (sheep-parents sheep))
      (print-unreadable-object (sheep stream :type t :identity t)
        (format stream "Orphaned Sheep!"))
      (print-sheep sheep stream)))
(defmethod print-object ((sheep message) stream)
  (print-unreadable-object (sheep stream :identity t)
    (format stream "Message: ~a" (message-name sheep))))
(defmethod print-object ((sheep reply) stream)
  (print-unreadable-object (sheep stream :identity t)
    (format stream "Reply: ~a" (reply-name sheep))))
(defmethod print-object ((sheep role) stream)
  (print-unreadable-object (sheep stream :identity t)
    (format stream "Role: ~a" (role-name sheep))))
