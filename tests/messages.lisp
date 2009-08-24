;;;; -*- Mode: lisp; indent-tabs-mode: nil -*-
;;;;
;;;; This file is part of Sheeple

;;;; tests/messages.lisp
;;;;
;;;; Unit tests for messages and replies
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :sheeple)

(def-suite messages :in sheeple)

(def-suite reply-combination :in messages)
(in-suite reply-combination)

(defmacro with-flag-stack (&body body)
  (let ((stack (gensym)))
    `(let (,stack)
       (flet ((flag (tag) (push tag ,stack))) ,@body)
       (nreverse ,stack))))

(test reply-stack
  (is (null (with-flag-stack)))
  (is (equal '(1) (with-flag-stack (flag 1))))
  (is (equal '(1 2 3) (with-flag-stack (flag 1) (flag 2) (flag 3)))))

(postboot-test primary
  (defmessage tester (x y))
  (with-sheep-hierarchy ((a) (b a))
    (is (equal '(:a-)
               (with-flag-stack
                 (defreply tester ((x a) y)
                   (flag :a-))
                 (tester a nil))))
    (is (equal '(:b- :a-)
               (with-flag-stack
                 (defreply tester ((x a) y)
                   (flag :a-))
                 (defreply tester ((x b) y)
                   (flag :b-)
                   (call-next-reply))
                 (tester b nil))))))

(def-suite message-definition :in messages)
(in-suite message-definition)

(postboot-test message-object
  (let ((test-message (%make-message :name 'name
                                     :lambda-list 'lambda-list
                                     :replies 'replies
                                     :documentation "dox")))
    (is (message-p test-message))
    (with-accessors ((name          message-name)
                     (lambda-list   message-lambda-list)
                     (replies       message-replies)
                     (memo-vector   message-memo-vector)
                     (arg-info      message-arg-info)
                     (documentation message-documentation))
        test-message
      (is (eq 'name        (message-name test-message)))
      (is (eq 'lambda-list (message-lambda-list test-message)))
      (is (eq 'replies     (message-replies test-message)))
      (is (typep memo-vector '(vector t 40)))
      (is (typep arg-info 'arg-info))
      (is (string= documentation "dox")))))

(test message-table
  (let ((test-message (%make-message :name 'name
                                     :lambda-list 'lambda-list
                                     :replies 'replies
                                     :documentation "dox")))
    (setf (find-message 'name) test-message)
    (is (eq test-message (find-message 'name)))
    (forget-message 'name)
    (is (eq nil (find-message 'name nil)))
    (signals no-such-message (find-message 'name))))

(test finalize-message)
(test ensure-message)
(test defmessage)
(test canonize-message-option)
(test canonize-message-options)
(test check-msg-lambda-list)
(test analyze-lambda-list)

