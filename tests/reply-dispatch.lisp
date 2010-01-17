;;;; -*- Mode: lisp; indent-tabs-mode: nil -*-
;;;;
;;;; This file is part of Sheeple

;;;; tests/reply-dispatch.lisp
;;;;
;;;; Unit tests for reply-dispatch and replies
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :sheeple)

(def-suite reply-combination :in messages)
(in-suite reply-combination)

(defmacro with-flag-stack (&body body)
  (with-gensyms (stack)
    `(let (,stack)
       (declare (ignorable ,stack))
       (flet ((flag (tag)
                (nconcf ,stack (list tag)))
              (check (reason &rest stack)
                (is (equal stack ,stack) reason)
                (setf ,stack nil)))
         (declare (ignorable (function flag) (function check)))
         ,@body))))

(defmacro with-dummy-message (message-arglist &body body)
  (let ((message-name (gensym)))
    `(with-test-message ,message-name
       (macrolet ((define-dummy-reply (qualifiers specializers &body body)
                    `(defreply ,',message-name ,@qualifiers
                       ,(mapcar 'list ',message-arglist specializers)
                       ,@body)))
         (flet ((call-dummy-message (,@message-arglist)
                  (,message-name ,@message-arglist)))
           (defmessage ,message-name ,message-arglist)
           ,@body)))))

(test standard-combination-primary
  (with-object-hierarchy (a (b a))
    (with-dummy-message (x y)
      (with-flag-stack
        (define-dummy-reply nil (a =t=) (flag :a))
        (call-dummy-message a nil)
        (check "Single reply" :a)))
    (with-dummy-message (x y)
      (with-flag-stack
        (define-dummy-reply nil (a =t=) (flag :a))
        (define-dummy-reply nil (b =t=) (flag :b) (call-next-reply))
        (call-dummy-message b nil)
        (check "Two replies, linked with CALL-NEXT-REPLY" :b :a)))))

(test standard-combination-before
  (with-object-hierarchy (a (b a))
    (with-dummy-message (x)
      (with-flag-stack
        (define-dummy-reply (:before) (a)
          (flag :a-before))
        (define-dummy-reply nil (a)
          (flag :a))
        (call-dummy-message a)
        (check "One primary, one :before, specialized on the same object" :a-before :a)))
    (with-dummy-message (x)
      (with-flag-stack
        (define-dummy-reply (:before) (b)
          (flag :b-before))
        (define-dummy-reply nil (a)
          (flag :a))
        (call-dummy-message b)
        (check "One primary and one :before specialized on a child" :b-before :a)))
    (with-dummy-message (x)
      (with-flag-stack
        (define-dummy-reply (:before) (b)
          (flag :b-before))
        (define-dummy-reply (:before) (a)
          (flag :a-before))
        (define-dummy-reply nil (a)
          (flag :a))
        (call-dummy-message b)
        (check "One primary, one :before specialized on the same object, and ~@
                one :before specialized on its child" :b-before :a-before :a)))
    (with-dummy-message (x)
      (with-flag-stack
        (define-dummy-reply (:before) (b)
          (flag :b-before))
        (define-dummy-reply (:before) (a)
          (flag :a-before))
        (define-dummy-reply nil (a)
          (flag :a))
        (call-dummy-message a)
        (check "One primary, one :before specialized on the same object, and ~@
                one :before which shouldn't be running" :a-before :a)))))

(test standard-combination-after
  (with-object-hierarchy (a (b a))
    (with-dummy-message (x)
      (with-flag-stack
        (define-dummy-reply (:after) (a)
          (flag :a-after))
        (define-dummy-reply nil (a)
          (flag :a))
        (call-dummy-message a)
        (check "One primary, one :after, specialized on the same object" :a :a-after)))
    (with-dummy-message (x)
      (with-flag-stack
        (define-dummy-reply (:after) (b)
          (flag :b-after))
        (define-dummy-reply nil (a)
          (flag :a))
        (call-dummy-message b)
        (check "One primary and one :after specialized on a child" :a :b-after)))
    (with-dummy-message (x)
      (with-flag-stack
        (define-dummy-reply (:after) (b)
          (flag :b-after))
        (define-dummy-reply (:after) (a)
          (flag :a-after))
        (define-dummy-reply nil (a)
          (flag :a))
        (call-dummy-message b)
        (check "One primary, one :after specialized on the same object, and ~@
                one :after specialized on its child" :a :a-after :b-after)))
    (with-dummy-message (x)
      (with-flag-stack
        (define-dummy-reply (:after) (b)
          (flag :b-after))
        (define-dummy-reply (:after) (a)
          (flag :a-after))
        (define-dummy-reply nil (a)
          (flag :a))
        (call-dummy-message a)
        (check "One primary, one :after specialized on the same object, and ~@
                one :after which shouldn't be running" :a :a-after)))))

(test standard-combination-around
  (with-object-hierarchy (a (b a))
    (with-dummy-message (x)
      (with-flag-stack
        (define-dummy-reply (:around) (a)
          (flag :around-entry)
          (call-next-reply)
          (flag :around-exit))
        (define-dummy-reply nil (a)
          (flag :a))
        (call-dummy-message a)
        (check "One primary, and one :around wrapping it" :around-entry :a :around-exit)))
    (with-dummy-message (x)
      (with-flag-stack
        (define-dummy-reply (:around) (b)
          (flag :poop))
        (define-dummy-reply nil (a)
          (flag :a))
        (call-dummy-message a)
        (check "One primary, and an :around which shouldn't be running" :a)))))

(def-suite reply-dispatch :in messages)
(in-suite reply-dispatch)

(test apply-replies)
(test next-reply-p)
(test call-next-reply)
(test compute-effective-reply-function)
(test compute-primary-erfun)
(test find-applicable-replies)

;; other dispatch stuff
(test sort-applicable-replies)
(test contain-reply)
(test unbox-replies)
(test fully-specified-p)
(test calculate-rank-score)
(test reply-specialized-portion)
