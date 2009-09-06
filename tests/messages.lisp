;;;; -*- Mode: lisp; indent-tabs-mode: nil -*-
;;;;
;;;; This file is part of Sheeple

;;;; tests/messages.lisp
;;;;
;;;; Unit tests for messages objects, ll-congruence, dispatch-caching, and message interface
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :sheeple)

(def-suite messages :in sheeple)

(def-suite message-basics :in messages)
(in-suite message-basics)

(test message-struct
  (let ((test-message (%make-message :name 'name
                                     :lambda-list 'lambda-list
                                     :replies 'replies
                                     :dispatch-cache 'cache
                                     :arg-info 'arg-info)))
    (is (messagep test-message))
    (with-accessors ((name           message-name)
                     (lambda-list    message-lambda-list)
                     (replies        message-replies)
                     (dispatch-cache message-dispatch-cache)
                     (arg-info       message-arg-info))
        test-message
      (is (eq 'name        name))
      (is (eq 'lambda-list lambda-list))
      (is (eq 'replies     replies))
      (is (eq 'cache       dispatch-cache))
      (is (eq 'arg-info    arg-info))))
  (let ((test-message (%make-message)))
    (is (messagep test-message))
    (with-accessors ((name           message-name)
                     (lambda-list    message-lambda-list)
                     (replies        message-replies)
                     (dispatch-cache message-dispatch-cache)
                     (arg-info       message-arg-info))
        test-message
      (is (null name))
      (is (null lambda-list))
      (is (null replies))
      (is (dispatch-cache-p dispatch-cache))
      (is (arg-info-p arg-info)))))

(test *message-table*
  (is (message-table-p *message-table*))
  (let ((test-message (%make-message :name 'name
                                     :lambda-list 'lambda-list
                                     :replies 'replies)))
    (setf (%find-message 'name) test-message)
    (is (eq test-message (%find-message 'name)))
    (forget-message 'name)
    (is (eq nil (%find-message 'name)))
    (signals no-such-message (find-message 'name))))

(test %find-message
  (let ((test-message (%make-message)))
    (is (eq test-message (setf (%find-message 'name) test-message)))
    (is (eq test-message (%find-message 'name)))
    (forget-message 'name)
    (is (eq nil (%find-message 'name)))
    (signals no-such-message (find-message 'name))))

(test forget-message
  (let ((test-message (%make-message)))
    (setf (%find-message 'test) test-message)
    (is (forget-message 'test))
    (is (null (%find-message 'name)))
    (is (null (forget-message 'test)))))

(test forget-all-messages
  (let ((a (%make-message))
        (b (%make-message))
        (c (%make-message)))
    (setf (%find-message 'a) a)
    (setf (%find-message 'b) b)
    (setf (%find-message 'c) c)
    (is (forget-all-messages))
    (is (message-table-p *message-table*))
    (is (null (%find-message 'a)))
    (is (null (%find-message 'b)))
    (is (null (%find-message 'c)))))

(def-suite dispatch-cache :in messages)
(in-suite dispatch-cache)

(test *dispatch-cache-size*
  (is (integerp *dispatch-cache-size*)))

(test make-dispatch-cache
  (let ((cache (make-dispatch-cache)))
    (is (dispatch-cache-p cache))
    (is (= *dispatch-cache-size* (length cache)))))

(test dispatch-cache-p
  (let ((cache (make-dispatch-cache)))
    (is (dispatch-cache-p cache))))

(test make-dispatch-cache-entry
  (let ((entry (make-dispatch-cache-entry '(a b) '(c d))))
    (is (dispatch-cache-entry-p entry))
    (is (equal '(a b) (cache-entry-args entry)))
    (is (equal '(c d) (cache-entry-replies entry)))))

(test cache-entry-args
  (let ((entry (make-dispatch-cache-entry '(a b) '(c d))))
    (is (equal '(a b) (cache-entry-args entry)))
    (is (not (fboundp '(setf cache-entry-args))))))

(test cache-entry-replies
  (let ((entry (make-dispatch-cache-entry '(a b) '(c d))))
    (is (equal '(c d) (cache-entry-replies entry)))
    (is (not (fboundp '(setf cache-entry-replies))))))

(test add-entry-to-message)
(test clear-dispatch-cache)
(test clear-all-message-caches)

(def-suite arg-info :in messages)
(in-suite arg-info)

(test arg-info)
(test arg-info-valid-p)
(test arg-info-applyp)
(test arg-info-number-required)
(test arg-info-nkeys)
(test set-arg-info)
(test analyze-lambda-list)
(test check-reply-arg-info)
(test check-msg-lambda-list)

(def-suite message-definition :in messages)
(in-suite message-definition)

(test finalize-message)
(test make-message)
(test ensure-message)
(test defmessage)
(test canonize-message-option
  (let ((o1 '(:documentation "foo"))
        (o2 '(:metasheep "thesheep")))
    (is (equal (list :documentation "foo") (canonize-message-option o1)))
    (is (equal (list :metasheep "thesheep") (canonize-message-option o2)))))

(test canonize-message-options
    (let ((o1 '(:documentation "foo"))
          (o2 '(:metasheep "thesheep")))
    (is (equal (list :documentation "foo" :metasheep "thesheep")
               (canonize-message-options (list o1 o2))))))
