;;;; -*- Mode: lisp; indent-tabs-mode: nil -*-
;;;;
;;;; This file is part of Sheeple

;;;; tests/reply-dispatch.lisp
;;;;
;;;; Unit tests for reply-dispatch and replies
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :sheeple)

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
