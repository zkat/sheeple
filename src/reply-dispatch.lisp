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

(defun score-reply (reply)
  (reduce '+ (the simple-vector (reply-rank-vector reply))))

(defun fully-specified-p (reply)
  (every 'numberp (the simple-vector (reply-rank-vector reply))))

(defun sort-applicable-replies (reply-list)
  (sort (the list reply-list) #'< :key 'score-reply))

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

(defun primary-reply-p (reply)
  (null (reply-qualifiers reply)))

(defun before-reply-p (reply)
  (eq :before (car (reply-qualifiers reply))))

(defun after-reply-p (reply)
  (eq :after (car (reply-qualifiers reply))))

(defun around-reply-p (reply)
  (eq :around (car (reply-qualifiers reply))))

(defun compute-erfun (message replies)
  (aif (find-if 'around-reply-p replies)
       (lambda (args)
         (funcall (reply-function it) args
                  (compute-erfun message (remove it replies))))
       (lambda (args)
         (let ((primaries (member-if 'primary-reply-p replies)))
           (when (null primaries)
             (error 'no-primary-replies :message (message-name message)))
           (dolist (reply replies)
             (when (before-reply-p reply)
               (funcall (reply-function reply) args nil)))
           (multiple-value-prog1
               (funcall (reply-function (car primaries))
                        args (compute-primary-erfun (cdr primaries)))
             (let ((reversed (reverse replies)))
               (declare (dynamic-extent reversed)) ; Needs more cowbell!
               (dolist (reply reversed)
                 (when (after-reply-p reply)
                   (funcall (reply-function reply) args nil)))))))))

(defun compute-primary-erfun (replies)
  (reduce (lambda (erfun reply)
            (if (primary-reply-p reply)
                (fun (funcall (reply-function reply) _ erfun))
                erfun))
          replies :initial-value nil))

(defun find-applicable-replies (message args &optional (errorp t))
  "Returns a sorted list of replies on MESSAGE for which appropriate roles
are present in ARGS. If no such replies are found and ERRORP is true, a
condition of type `no-applicable-replies' is signaled."
  (if (null args)
      (message-replies message)
      (loop with discovered-replies list and applicable-replies list
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
                (let ((reply (role-reply role)))
                  (unless (find reply discovered-replies :test #'eq)
                    (clear-reply-rank reply)
                    (push reply discovered-replies))
                  (setf (svref (reply-rank-vector reply) (the fixnum index)) hierarchy-position)
                  (when (fully-specified-p reply)
                    (pushnew reply applicable-replies :test #'eq))))))
         finally
         (if applicable-replies
             (return (sort-applicable-replies applicable-replies))
             (when errorp
               (error 'no-applicable-replies :message (message-name message) :args args))))))

(defun clear-reply-rank (reply)
  (let ((vector (reply-rank-vector reply)))
    (declare (simple-vector vector))
    (loop for i below (length vector)
       do (setf (svref vector i) nil))))
