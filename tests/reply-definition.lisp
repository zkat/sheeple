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

(defun %%make-reply (&key (message (allocate-message))
                     qualifiers lambda-list (function #'eq))
  "Just for testing purposes!"
  (make-reply message qualifiers lambda-list function))

(test make-reply
  (let* ((message (allocate-message))
         (qualifiers (list nil))
         (lambda-list (list nil))
         (test-reply (make-reply message qualifiers lambda-list #'eq)))
    (is (replyp test-reply))
    (is (eq 'reply (type-of test-reply)))
    (is (eq message (reply-message test-reply)))
    (is (eq qualifiers (reply-qualifiers test-reply)))
    (is (eq lambda-list (reply-lambda-list test-reply)))
    (is (eq #'eq (reply-function test-reply)))))

(test reply-name
  (let* ((message (%make-message (gensym) nil))
         (reply   (%%make-reply :message message)))
    (is (eq (message-name message) (reply-name reply)))))

(def-suite role-objects :in reply-definition)
(in-suite role-objects)

(test role-implementation
  (let* ((dummy-reply (%%make-reply))
         (dummy-position (random lambda-parameters-limit))
         (role (make-role dummy-reply dummy-position)))
    (is (eq dummy-reply (role-reply role)))
    (is (= dummy-position (role-position role)))))

(test role-type
  (let ((reply (%%make-reply))
        (position (random lambda-parameters-limit)))
    (is (typep (make-role reply position) 'role))
    (is (rolep (make-role reply position)))))

(test role-message
  (let ((message (allocate-message)))
    (is (eq message (role-message (make-role (%%make-reply :message message) 0))))))

(test role-name
  (let ((message (%make-message (gensym) nil)))
    (is (eq (message-name message)
            (role-name (make-role (%%make-reply :message message) 0))))))

(test participantp
  (let ((object (smop:allocate-object =standard-metaobject=))
        (reply (%%make-reply))
        (position (random lambda-parameters-limit)))
    (push (make-role reply position) (%object-roles object))
    (is (not (null (participantp object reply))))))

(in-suite reply-definition)

(test ensure-reply)

(test reply-redefinition
  (with-test-message (foo (bar))
    (defmessage foo (bar)
      (:reply ((bar =t=)) 1))
    (is (= 1 (foo 'x)))
    (defreply foo ((bar =t=)) 2)
    (is (= 2 (foo 'x)))))

(test add-reply-to-message)
(test add-reply-to-objects)
(test available-replies)
(test add-reader-to-object)
(test add-readers-to-object)
(test add-writer-to-object)
(test add-writers-to-object)

(def-suite reply-undefinition :in reply-definition)
(in-suite reply-undefinition)

(test undefine-reply)
(test remove-specific-reply)
(test remove-applicable-reply)
(test delete-reply)
(test delete-role)

(def-suite user-interface :in reply-definition)
(in-suite user-interface)

(test defreply
  ;; Test autoboxing
  (with-test-message (test-message (x))
    (Eos:finishes (handler-bind
                      ((automatic-message-creation #'muffle-warning))
                    (defreply test-message ((n 3)))))))
(test defreply-bug
  "Expected failure"
  (with-test-message (test-message ())
    (handler-case
        (defreply test-message ((x =t=)))
      (simple-error () (pass))
      (:no-error (&rest values)
        (declare (ignore values))
        (fail "~@<DEFREPLY silently added a reply with an incompatible ~
               ~_lambda-list to a message with no replies~:>")))))

(test %defreply-expander)
(test make-reply-lambda)
(test parse-defreply)
(test extract-var-name)
(test confirm-var-name)

(test undefreply
  (with-test-message (test-message (x))
    (let ((object (object)))
      (defreply test-message ((x object)) x)
      (is (not (null (undefreply test-message (object)))))
      (signals no-applicable-reply (test-message object))
      (is (null (undefreply test-message (object))))
      (is (null (%object-roles object))))))

(test parse-undefreply)
