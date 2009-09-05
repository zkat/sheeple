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

(test %make-reply)
(test replyp)
(test reply-name)

(def-suite role-objects :in reply-definition)
(in-suite role-objects)

(test %make-role)
(test rolep)
(test role-reply)
(test role-position)
(test role-message)

(test reply-p)
(test role-p)
(test participant-p)
(test ensure-reply)
(test generate-reply)
(test defreply)
(test create-msg-lambda-list)
(test add-reply-to-message)
(test remove-specific-reply)
(test delete-reply)
(test delete-role)
(test add-reply-to-sheeple)
(test undefine-reply)
(test remove-applicable-reply)
(test available-replies)
(test add-readers-to-sheep)
(test add-writers-to-sheep)
(test make-reply-lambda)
(test parse-defreply)
(test extract-var-name)
(test confirm-var-name)
(test undefreply)
(test parse-undefreply)
