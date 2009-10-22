;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; indent-tabs-mode: nil -*-
;;;;
;;;; This file is part of Sheeple
;;;;
;;;; reply-dispatch.lisp
;;;;
;;;; Reply execution and dispatch
;;;;
;;;; TODO
;;;; * Figure out an optimization to make accessors about as fast as calling property-value
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :sheeple)

(defun primary-reply-p (reply)
  (null (reply-qualifiers reply)))

(defun before-reply-p (reply)
  (find :before (reply-qualifiers reply)))

(defun after-reply-p (reply)
  (find :after (reply-qualifiers reply)))

(defun around-reply-p (reply)
  (find :around (reply-qualifiers reply)))

(defun apply-message (message args)
  (let ((relevant-args-length (arg-info-number-required (message-arg-info message))))
    (error-when (< (the fixnum (length args))
                   (the fixnum relevant-args-length))
                insufficient-message-args :message message)
    (let ((relevant-args (subseq args 0 relevant-args-length)))
      (flet ((compute-erfun () ; This local function avoids code duplication
               (compute-erfun message (find-applicable-replies message relevant-args))))
        (declare (dynamic-extent (function compute-erfun)))
        (if *caching-enabled*
            (aif (find-cached-erfun message relevant-args)
                 (funcall it args)
                 (let ((erfun (compute-erfun)))
                   (cache-erfun message relevant-args erfun)
                   (funcall erfun args)))
            (funcall (compute-erfun) args))))))

(defun compute-erfun (message replies)
  (let ((around (car (remove-if-not 'around-reply-p replies)))
        (primaries (remove-if-not 'primary-reply-p replies)))
    (when (null primaries)
      (error 'no-primary-replies :message (message-name message)))
    (if around
        (let ((next-erfun (compute-erfun message (remove around replies))))
          (lambda (args)
            (funcall (reply-function around) args next-erfun)))
        (let ((next-erfun (compute-primary-erfun (cdr primaries)))
              (befores (remove-if-not 'before-reply-p replies))
              (afters (remove-if-not 'after-reply-p replies)))
          (lambda (args)
            (dolist (before befores)
              (funcall (reply-function before) args nil))
            (multiple-value-prog1
                (funcall (reply-function (car primaries)) args next-erfun)
              (dolist (after (reverse afters))
                (funcall (reply-function after) args nil))))))))

(defun compute-primary-erfun (replies)
  ;; Base case for the recursion -- are there any replies left?
  (when replies
    ;; Stitch together the next reply & erfun
    (rcurry (reply-function (car replies))
            (compute-primary-erfun (cdr replies)))))

(defun find-applicable-replies (message args &optional (errorp t))
  "Returns the most specific reply using MESSAGE and ARGS."
  (if (null args)
      (message-replies message)
      (loop with discovered-replies list and contained-applicable-replies list
         for arg in args and index fixnum upfrom 0 do
           (loop
              for hierarchy-object in
                (object-hierarchy-list
                 (if (objectp arg) arg
                     (or (find-boxed-object arg)
                         (box-type-of arg))))
              and hierarchy-position fixnum upfrom 0 do
                (dolist (role (%object-roles hierarchy-object))
                  (declare (role role))
                  (when (and (eq message (role-message role))
                             (= index (the fixnum (role-position role))))
                    (let ((reply (role-reply role)) contained-reply)
                      (aif (find reply discovered-replies
                                 :key #'reply-container-reply :test #'eq)
                           (setf contained-reply it)
                           (push (setf contained-reply (contain-reply reply))
                                 discovered-replies))
                      (setf (elt (reply-container-rank contained-reply) index)
                            hierarchy-position)
                      (when (fully-specified-p (reply-container-rank contained-reply))
                        (pushnew contained-reply contained-applicable-replies
                                 :key #'reply-container-reply))))))
         finally
           (if contained-applicable-replies
               (return (unbox-replies (sort-applicable-replies contained-applicable-replies)))
               (when errorp
                 (error 'no-applicable-replies :message (message-name message) :args args))))))

(defun unbox-replies (replies)
  (mapcar #'reply-container-reply replies))

(defun sort-applicable-replies (reply-list &key (rank-key #'<))
  (sort reply-list rank-key
        :key (compose 'calculate-rank-score 'reply-container-rank)))

(defun contain-reply (reply)
  (make-reply-container
   :reply reply
   :rank (make-vector (length (reply-specialized-portion reply)))))

(defstruct (reply-container (:type vector))
  reply
  rank)

(defun fully-specified-p (rank)
  (notany 'null rank))

(defun calculate-rank-score (rank)
  (declare (simple-array rank))
  (let ((total 0))
    (declare (fixnum total))
    (loop for item across rank
       do (when (numberp item)
            (incf (the fixnum total) (the fixnum item))))
    total))

(defun reply-specialized-portion (msg)
  (parse-lambda-list (reply-lambda-list msg)))
