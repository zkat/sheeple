;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; indent-tabs-mode: nil -*-

;; This file is part of Sheeple

;; glue.lisp
;;
;; Extra glue things
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :sheeple)

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
