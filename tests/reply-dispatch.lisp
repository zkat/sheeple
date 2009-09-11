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
                 (defreply tester ((x a) (y =t=))
                   (flag :a-))
                 (funcall 'tester a nil)
                 (undefreply tester ((x a) y)))))
    (is (equal '(:b- :a-)
               (with-flag-stack
                 (defreply tester ((x a) (y =t=))
                   (flag :a-))
                 (defreply tester ((x b) (y =t=))
                   (flag :b-)
                   (call-next-reply))
                 (funcall 'tester b nil))))))

(def-suite reply-dispatch :in messages)
(in-suite reply-dispatch)

(test primary-reply-p)
(test before-reply-p)
(test after-reply-p)
(test around-reply-p)
(test apply-message)
(test apply-replies)
(test next-reply-p)
(test call-next-reply)
(test compute-effective-reply-function)
(test compute-primary-erfun)
(test find-applicable-replies)
(test %find-applicable-replies)

;; caching
(test create-reply-cache)
(test desired-vector-entry-p)
(test fetch-memo-vector-entry)
(test add-entry-to-message)
(test memoize-reply-dispatch)

;; other dispatch stuff
(test sort-applicable-replies)
(test contain-reply)
(test unbox-replies)
(test fully-specified-p)
(test calculate-rank-score)
(test reply-specialized-portion)


