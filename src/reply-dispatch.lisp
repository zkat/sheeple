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
     with total fixnum = 0 do
       (setf total (the fixnum (+ total rank)))
     finally (return total)))

(defun fully-specified-p (reply)
  (loop for rank across (the simple-vector (reply-rank-vector reply)) always rank))

(defun sort-applicable-replies (reply-list)
  (sort (the list reply-list) #'< :key 'score-reply))

(defun std-compute-discriminating-function (message)
  (lambda (&rest args)
    (let ((replies (compute-applicable-replies message (required-portion message args))))
      (apply (or (cached-erfun message replies)
                 (setf (cached-erfun message replies)
                       (compute-effective-reply-function message replies args)))
             args))))

;;; This makes the argument list accessible to error functions like `no-primary-reply'
;;; which are called from deeper within the dispatch machinery. Passing the arguments
;;; down through each function call would suck.

(defvar *reply-combination-args*)

(defun compute-effective-reply-function (message replies *reply-combination-args*)
  ;; This will eventually have some special-casing to break MOP
  ;; metacircularities, and call `sheeple:compute-effective-reply'
  ;; if MESSAGE isn't a standard-message or special message.
  (if (null replies)
      (lambda (&rest args) (apply 'no-applicable-reply message args))
      (compile-effective-reply message (std-compute-effective-reply message replies))))

(defun compile-effective-reply (message effective-reply)
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
                                        (std-make-reply-lambda
                                         `(lambda (&rest ,args)
                                            (declare (ignorable ,args))
                                            ,(transform-effective-reply (cadr form))))))
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
       (apply 'no-primary-reply message *reply-combination-args*))
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

(declaim (inline relevant-role-p))
(defun relevant-role-p (role message index)
  (declare (fixnum index))
  (and (eq message (role-message role))
       (= index (the fixnum (role-position role)))))

(defun compute-applicable-replies (message args)
  "Returns a sorted list of replies on MESSAGE for which appropriate roles
are present in ARGS."
  (declare (list args))
  (if (null args)
      (message-replies message)
      (let (discovered-replies applicable-replies)
        (declare (list discovered-replies applicable-replies))
        (flet ((find-and-rank-roles (object hposition index)
                 ;; Given an object, and a specified place in the precedence list,
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
                   for precedence-object in (mold-precedence-list (%object-mold obj))
                   for precedence-position fixnum from 1
                   do (find-and-rank-roles precedence-object precedence-position index))
             finally
               (when applicable-replies
                 (return (sort-applicable-replies applicable-replies))))))))

(defun clear-reply-rank (reply &aux (vector (reply-rank-vector reply)))
  (declare (simple-vector vector))
  (loop for i below (length vector)
     do (setf (svref vector i) nil)))
