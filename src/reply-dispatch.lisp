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
  (loop for rank fixnum across (the simple-vector (reply-rank-vector reply))
     with total fixnum = 0 do (setf total (the fixnum (+ total rank)))
     finally (return total)))

(defun fully-specified-p (reply)
  (loop for rank across (the simple-vector (reply-rank-vector reply)) always rank))

(defun sort-applicable-replies (reply-list)
  (sort (the list reply-list) #'< :key 'score-reply))

(defun apply-message (message args)
  (declare (list args))
  (let ((relevant-args-length (message-number-required message)))
    (error-when (< (the fixnum (length args))
                   (the fixnum relevant-args-length))
                insufficient-message-args :message message)
    (let* ((relevant-args (subseq args 0 relevant-args-length))
           (replies (find-applicable-replies message relevant-args))
           (erfun (compute-erfun message replies)))
      (funcall erfun args))))

(defun primary-reply-p (reply)
  (null (reply-qualifiers reply)))

(defun before-reply-p (reply)
  (eq :before (car (reply-qualifiers reply))))

(defun after-reply-p (reply)
  (eq :after (car (reply-qualifiers reply))))

(defun around-reply-p (reply)
  (eq :around (car (reply-qualifiers reply))))

(defun compute-erfun (message replies)
  (declare (list replies))
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
             (do-reversed (afters replies) ; Has more cowbell!
               (dolist (reply afters)
                 (when (after-reply-p reply)
                   (funcall (reply-function reply) args nil)))))))))

(defun compute-primary-erfun (replies)
  (reduce (lambda (erfun reply)
            (if (primary-reply-p reply)
                (fun (funcall (reply-function reply) _ erfun))
                erfun))
          replies :initial-value nil))

(declaim (inline relevant-role-p))
(defun relevant-role-p (role message index)
  (declare (fixnum index))
  (and (eq message (role-message role))
       (= index (the fixnum (role-position role)))))

(defun find-applicable-replies (message args &optional (errorp t))
  "Returns a sorted list of replies on MESSAGE for which appropriate roles
are present in ARGS. If no such replies are found and ERRORP is true, a
condition of type `no-applicable-replies' is signaled."
  (declare (list args))
  (if (null args)
      (message-replies message)
      (let (discovered-replies applicable-replies)
        (declare (list discovered-replies applicable-replies))
        (flet ((find-and-rank-roles (object hposition index)
                 ;; Given an object, and a specified place in the hierarchy,
                 ;; find the roles we want for a lambda-list position, and
                 ;; rank the respective replies.
                 (declare (fixnum hposition index))
                 (dolist (role (%object-roles object))
                   (when (relevant-role-p role message index)
                     (let ((reply (role-reply role)))
                       (unless (find reply discovered-replies :test #'eq)
                         (clear-reply-rank reply)
                         (push reply discovered-replies))
                       (setf (svref (the simple-vector (reply-rank-vector reply)) index) hposition)
                       (when (fully-specified-p reply)
                         (pushnew reply applicable-replies :test #'eq)))))))
          (declare (dynamic-extent #'find-and-rank-roles))
          (loop
             for arg in args and index fixnum upfrom 0
             for obj = (ensure-dispatch-object arg)
             ;; To avoid consing, we call f-a-r-r on the root object first
             ;; Then we iterate over its ordered ancestors
             do (loop initially (find-and-rank-roles obj 0 index)
                   for hierarchy-object in (mold-hierarchy (%object-mold obj))
                   for hierarchy-position fixnum from 1
                   do (find-and-rank-roles hierarchy-object hierarchy-position index))
             finally
               (if applicable-replies
                   (return (sort-applicable-replies applicable-replies))
                   (when errorp
                     (error 'no-applicable-replies :message (message-name message) :args args))))))))

(defun clear-reply-rank (reply &aux (vector (reply-rank-vector reply)))
  (declare (simple-vector vector))
  (loop for i below (length vector)
     do (setf (svref vector i) nil)))
