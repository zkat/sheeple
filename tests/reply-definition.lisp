;;;; -*- Mode: lisp; indent-tabs-mode: nil -*-
;;;;
;;;; This file is part of Sheeple

;;;; tests/reply-definition.lisp
;;;;
;;;; Unit tests for reply-definition and replies
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :sheeple)

(def-suite reply-definition :in messages)

(def-suite reply-objects :in reply-definition)
(in-suite reply-objects)

(test make-reply)
(test replyp)
(test reply-name)

(def-suite role-objects :in reply-definition)
(in-suite role-objects)

(test make-role)
(test rolep)
(test role-reply)
(test role-position)
(test role-message)
(test participantp)

(in-suite reply-definition)

(test ensure-reply)
(test create-msg-lambda-list)
(test add-reply-to-message)
(test add-reply-to-sheeple)
(test available-replies)
(test add-reader-to-sheep)
(test add-readers-to-sheep)
(test add-writer-to-sheep)
(test add-writers-to-sheep)

(def-suite reply-undefinition :in reply-definition)
(in-suite reply-undefinition)

(test undefine-reply)
(test remove-specific-reply)
(test remove-applicable-reply)
(test delete-reply)
(test delete-role)

(def-suite user-interface :in reply-definition)
(in-suite user-interface)

(test defreply)
(test %defreply-expander)
(test make-reply-lambda)
(test parse-defreply)
(test extract-var-name)
(test confirm-var-name)
(test undefreply)
(test parse-undefreply)
