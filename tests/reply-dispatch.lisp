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

(defmacro with-dummy-message (message-arglist &body body)
  (let ((message-name (gensym)))
    `(unwind-protect
          (macrolet ((define-dummy-reply (qualifiers specializers &body body)
                       `(defreply ,',message-name ,@qualifiers
                          ,(mapcar 'list ',message-arglist specializers)
                          ,@body)))
            (flet ((call-dummy-message (,@message-arglist)
                     (funcall ',message-name ,@message-arglist)))
             (defmessage ,message-name ,message-arglist)
              ,@body))
       (forget-message ',message-name)
       (fmakunbound ',message-name))))

(defmacro test-dummy-dispatch (target-flags message-args final-call-args
                               &body reply-definitions)
  `(is (equal ',target-flags
              (with-dummy-message ,message-args
                (with-flag-stack
                  ,@(mapcar (curry 'cons 'define-dummy-reply) reply-definitions)
                  (call-dummy-message ,@final-call-args))))))

(test standard-combination-primary
  (with-object-hierarchy (a (b a))
    (test-dummy-dispatch (:a) (x y) (a nil)
      (() (a =t=) (flag :a)))
    (test-dummy-dispatch (:b :a) (x y) (b nil)
      (() (a =t=) (flag :a))
      (() (b =t=) (flag :b) (call-next-reply)))))

(test standard-combination-before
  (with-object-hierarchy (a (b a))
    (test-dummy-dispatch (:a-before :a) (x) (a)
      ((:before) (a) (flag :a-before))
      (() (a) (flag :a)))
    (test-dummy-dispatch (:b-before :a) (x) (b)
      ((:before) (b) (flag :b-before))
      (() (a) (flag :a)))
    (test-dummy-dispatch (:b-before :a-before :a) (x) (b)
      ((:before) (b) (flag :b-before))
      ((:before) (a) (flag :a-before))
      (() (a) (flag :a)))
    (test-dummy-dispatch (:a-before :a) (x) (a)
      ((:before) (b) (flag :b-before))
      ((:before) (a) (flag :a-before))
      (() (a) (flag :a)))))

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

;; other dispatch stuff
(test sort-applicable-replies)
(test contain-reply)
(test unbox-replies)
(test fully-specified-p)
(test calculate-rank-score)
(test reply-specialized-portion)


