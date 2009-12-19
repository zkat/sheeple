;;;; -*- Mode: lisp; indent-tabs-mode: nil -*-
;;;;
;;;; This file is part of Sheeple

;;;; tests/messages.lisp
;;;;
;;;; Unit tests for messages objects, ll-congruence, and message interface
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :sheeple)

(def-suite messages :in sheeple)

(def-suite message-basics :in messages)
(in-suite message-basics)

(test message-struct
  (let ((test-message (allocate-message)))
    (is (messagep test-message))
    (with-accessors ((name           message-name)
                     (lambda-list    message-lambda-list)
                     (replies        message-replies)
                     )
        test-message
      (is (null name))
      (setf name 'foo)
      (is (eq 'foo name))
      (is (null lambda-list))
      (setf lambda-list '(lambda the ultimate list))
      (is (equal '(lambda the ultimate list) lambda-list))
      (is (null replies))
      )))

(def-suite arg-info :in messages)
(in-suite arg-info)

(test set-arg-info)
(test check-reply-arg-info)
(test check-msg-lambda-list)
(test create-msg-lambda-list
  (let ((ll '(a (b =b-proto=) &key hey)))
    (is (equal '(a b &key) (create-msg-lambda-list ll)))))

(def-suite message-definition :in messages)
(in-suite message-definition)

(test ensure-message)
(test make-message)
(test finalize-message)

(test defmessage)
(test canonize-message-option
  (let ((o1 '(:documentation "foo"))
        (o2 '(:metaobject "theobject")))
    (is (equal (list :documentation "foo") (canonize-message-option o1)))
    (is (equal (list :metaobject "theobject") (canonize-message-option o2)))))

(test canonize-message-options
    (let ((o1 '(:documentation "foo"))
          (o2 '(:metaobject "theobject")))
    (is (equal (list :documentation "foo" :metaobject "theobject")
               (canonize-message-options (list o1 o2))))))
