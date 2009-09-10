;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; indent-tabs-mode: nil -*-

;;;; This file is part of Sheeple

;;;; post-boot.lisp
;;;;
;;;; Once sheeple is booted up, we can define messages/replies normally
;;;;
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
