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

(test make-reply
  (let ((test-reply (make-reply :message 0 :qualifiers 1 :lambda-list 2 :function 3)))
    (is (replyp test-reply))
    (is (eq 'reply (type-of test-reply)))
    (is (= 0 (reply-message     test-reply)))
    (is (= 1 (reply-qualifiers  test-reply)))
    (is (= 2 (reply-lambda-list test-reply)))
    (is (= 3 (reply-function    test-reply)))))

(test reply-name
  (let* ((message (%make-message :name (gensym)))
         (reply   (make-reply :message message)))
    (is (eq (message-name message) (reply-name reply)))))

(def-suite role-objects :in reply-definition)
(in-suite role-objects)

(test role-implementation
  (for-all ((dummy-reply (gen-integer)) (dummy-position (gen-integer)))
    (let ((role (make-role dummy-reply dummy-position)))
      (is (eq dummy-reply (role-reply role)))
      (is (eq dummy-position (role-position role))))))

(test role-type
  (for-all ((reply (fun (make-reply))) (position (gen-integer)))
    (is (typep (make-role reply position) 'role))
    (is (rolep (make-role reply position)))))

(test role-message
  (for-all ((message (fun (%make-message))))
    (is (eq message (role-message (make-role (make-reply :message message) 0))))))

(test role-name
  (for-all ((message (fun (%make-message :name (gensym)))))
    (is (eq (message-name message) (role-name (make-role (make-reply :message message) 0))))))

(test participantp
  (for-all ((sheep (fun (cons-std-sheep))) (reply (fun (make-reply))) (position (gen-integer)))
    (push (make-role reply position) (%sheep-roles sheep))
    (is (not (null (participantp sheep reply))))))

(in-suite reply-definition)

(test ensure-reply)

(test reply-redefinition
  (defmessage foo (bar))
  (defreply foo (bar) 1)
  (is (= 1 (foo 'x)))
  (defreply foo (bar) 2)
  (is (= 2 (foo 'x))))

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
