;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; indent-tabs-mode: nil -*-

;;;; This file is part of Sheeple

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
    (assert (>= (length args) relevant-args-length)
            () 'insufficient-message-args :message message)
    (let ((replies (find-applicable-replies message (subseq args 0 relevant-args-length))))
      (apply-replies message replies args))))

(defun apply-replies (message replies args)
  (funcall (compute-effective-reply-function message replies) args))

(defun compute-effective-reply-function (message replies)
  (let ((around (car (remove-if-not 'around-reply-p replies)))
        (primaries (remove-if-not 'primary-reply-p replies)))
    (when (null primaries)
      (error 'no-primary-replies :message (message-name message)))
    (if around
        (let ((next-erfun
               (compute-effective-reply-function message (remove around replies))))
          (lambda (args)
            (funcall (reply-function around) args next-erfun)))
        (let ((next-erfun (compute-primary-erfun (cdr primaries)))
              (befores (remove-if-not 'before-reply-p replies))
              (afters (remove-if-not 'after-reply-p replies)))
          (lambda (args)
            (when befores
              (dolist (before befores)
                (funcall (reply-function before) args nil)))
            (multiple-value-prog1
                (funcall (reply-function (car primaries)) args next-erfun)
              (when afters
                (dolist (after (reverse afters))
                  (funcall (reply-function after) args nil)))))))))

(defun compute-primary-erfun (replies)
  (when replies (rcurry (reply-function (car replies)) (compute-primary-erfun (cdr replies)))))

(defun find-applicable-replies (message args &optional (errorp t))
  "Returns the most specific reply using MESSAGE and ARGS."
  (declare (list args))
  (if (null args)
      (message-replies message)
      (let ((selector (message-name message))
            (n (length args))
            (discovered-replies nil)
            (contained-applicable-replies nil))
        (declare (list discovered-replies contained-applicable-replies))
        (loop
           for arg in args
           for index upto (1- n)
           do (let* ((arg (if (objectp arg)
                              arg
                              (or (find-boxed-object arg)
                                  (box-type-of arg))))
                     (curr-object-list (object-hierarchy-list arg)))
                (loop
                   for curr-object in curr-object-list
                   for hierarchy-position upto (1- (length curr-object-list))
                   do (dolist (role (%object-roles curr-object))
                        (when (and (equal selector (message-name (role-message role))) ;(eql message (role-message role))
                                   (= (the fixnum index) (the fixnum (role-position role))))
                          (let ((curr-reply (role-reply role)))
                            (when (= n (length (the list (reply-specialized-portion curr-reply))))
                              (when (not (member curr-reply
                                                 discovered-replies
                                                 :key #'reply-container-reply))
                                (pushnew (the vector (contain-reply curr-reply))
                                         discovered-replies))
                              (let ((contained-reply (find curr-reply
                                                           discovered-replies
                                                           :key #'reply-container-reply)))
                                (setf (elt (reply-container-rank contained-reply) index)
                                      hierarchy-position)
                                (when (fully-specified-p (reply-container-rank contained-reply))
                                  (pushnew contained-reply contained-applicable-replies :test #'equalp))))))))))
        (if contained-applicable-replies
            (unbox-replies (sort-applicable-replies contained-applicable-replies))
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
