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
(set-pprint-dispatch 'sheep (lambda (stream sheep) (print-sheep sheep stream)))
(defun std-print-sheep (stream sheep)
  (print-unreadable-object (sheep stream :identity t)
    (format stream "Sheep~@[ AKA ~A~]"
            (ignore-errors (property-value sheep 'name)))))
;;; BORKED!
(defmessage print-sheep (sheep stream)
  (:documentation "Defines the expression print-object uses."))
;;; BORKED!
(defreply print-sheep (sheep (stream =stream=))
  (std-print-sheep stream sheep))
;;; IT'S ALL BORKED!
(defreply print-sheep ((sheep =boxed-object=) (stream =stream=))
  (print-unreadable-object (sheep stream :identity t)
    (format stream "Boxed object")))
;;; Some sanity
(set-pprint-dispatch 'sheep 'std-print-sheep)
