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

(defun std-compute-discriminating-function (message)
  (lambda (&rest args)
    (let ((replies (find-applicable-replies message (required-portion message args))))
      (when (null replies) (apply 'no-applicable-reply message args))
      (let ((effective-reply (std-compute-effective-reply message replies)))
        (apply (std-compute-erfun message effective-reply) args)))))

(defun primary-reply-p (reply)
  (null (reply-qualifiers reply)))

(defun before-reply-p (reply)
  (eq :before (car (reply-qualifiers reply))))

(defun after-reply-p (reply)
  (eq :after (car (reply-qualifiers reply))))

(defun around-reply-p (reply)
  (eq :around (car (reply-qualifiers reply))))

(defun std-compute-erfun (message effective-reply)
  (with-gensyms (args)
    (labels ((transform-effective-reply (form)
               (if (atom form) form
                   (case (car form)
                     (call-reply (transform-effective-reply
                                  (let ((the-reply (transform-effective-reply (cadr form))))
                                    (with-gensyms (reply-var)
                                      `(let ((,reply-var ,the-reply))
                                         (declare (ignorable ,reply-var))
                                         (funcall (reply-function ,(if (replyp the-reply)
                                                                       the-reply reply-var))
                                                  ,args
                                                  ,@(let ((subforms
                                                           (loop for subform in (cddr form)
                                                              collect `',subform)))
                                                      (if subforms subforms '(())))
                                                  :message ,message
                                                  :reply ,(if (replyp the-reply)
                                                              the-reply reply-var)))))))
                     (make-reply (when (cddr form)
                                   (error "Incorrect make-reply form: ~S" form))
                                 (let ((reply-lambda
                                        (make-reply-lambda
                                         (message-name message) `(&rest ,args) `(,args)
                                         `(,(transform-effective-reply (cadr form))))))
                                   (make-reply message () (message-lambda-list message)
                                               (compile nil reply-lambda))))
                     (t (mapcar #'transform-effective-reply form))))))
      (let ((erf-lambda `(lambda (&rest ,args)
                           (declare (ignorable ,args))
                           ,(transform-effective-reply effective-reply))))
        (multiple-value-bind (function warnings failure)
            (compile nil erf-lambda)
          (declare (ignore warnings))
          (assert (not failure))
          function)))))

(defun std-compute-effective-reply (message applicable-replies)
  (collect (before primary after around)
    (dolist (reply applicable-replies)
      (let ((qualifiers (reply-qualifiers reply)))
        (cond
          ((null qualifiers) (primary reply))
          ((cdr qualifiers) (error "FIXME -- bad qualifiers for a standard-message"))
          (t (case (car qualifiers)
               (:around (around reply))
               (:before (before reply))
               (:after  (after reply))
               (t (error "FIXME -- bad qualifiers for a standard-message")))))))
    (cond
      ((null (primary))
       (error 'no-primary-replies :message (message-name message)))
      ;; SBCL has some optimization here. Pull it in, eventually.
      (t (let ((main-erfun `(multiple-value-prog1
                                (progn
                                  ,@(mapcar (fun `(call-reply ,_)) (before))
                                  (call-reply ,(car (primary)) ,(cdr (primary))))
                              ,@(mapcar (fun `(call-reply ,_)) (reverse (after))))))
           (if (not (around)) main-erfun
               `(call-reply ,(car (around))
                            (,@(cdr (around))
                               (make-reply ,main-erfun)))))))))

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

(defun find-applicable-replies (message args)
  "Returns a sorted list of replies on MESSAGE for which appropriate roles
are present in ARGS."
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
               (when applicable-replies
                 (return (sort-applicable-replies applicable-replies))))))))

(defun clear-reply-rank (reply &aux (vector (reply-rank-vector reply)))
  (declare (simple-vector vector))
  (loop for i below (length vector)
     do (setf (svref vector i) nil)))
