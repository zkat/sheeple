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

(deftype ranked-reply ()
  'simple-vector)

(declaim (inline unbox-ranked-reply))
(defun unbox-ranked-reply (ranked-reply)
  (declare (ranked-reply ranked-reply) (optimize speed (safety 0)))
  (svref ranked-reply 0))

(declaim (inline reply-specialized-length))
(defun reply-specialized-length (reply)
  (count-required-parameters (reply-lambda-list reply)))

(defun box-reply (reply)                ; For lack of a better name
  (declare (reply reply) (optimize speed (safety 0)))
  (aprog1 (make-array (the fixnum (1+ (the fixnum (reply-specialized-length reply))))
                      :initial-element nil)
    (setf (svref it 0) reply)))

(defun nunbox-replies (replies)
  "Unbox in-place each of the contained REPLIES."
  ;; This has a mean disassembly that's worth every dead kitten of the (safety 0)
  (declare (optimize speed (safety 0)))
  (do ((tail replies (cdr tail)))
      ((null tail) replies)
    (declare (list tail))
    (setf (car tail) (unbox-ranked-reply (car tail)))))

(defun score-ranked-reply (ranked-reply)
  ;; All hell breaks loose if you don't give this a simple-vector
  (declare (ranked-reply ranked-reply) (optimize speed (safety 0)))
  (loop for i fixnum downfrom (1- (length ranked-reply))
     for elt = (svref ranked-reply i) with total fixnum = 0
     when (zerop i) return total
     unless (null elt) do
       (setf total (the fixnum (+ total (the fixnum elt))))))

(defun fully-specified-p (ranked-reply)
  ;; Same here, and all the elements better be (or fixnum null)
  (declare (ranked-reply ranked-reply) (optimize speed (safety 0)))
  (loop for i fixnum downfrom (1- (length ranked-reply))
     when (zerop i) return t
     unless (svref ranked-reply i) return nil))

(defun sort-applicable-replies (reply-list)
  ;; Most lisps compile this as a tail call, so this function ends up being a
  ;; macro around SORT, and there's no harm in the (safety 0)
  (declare (optimize speed (safety 0)) (list reply-list))
  (sort reply-list #'< :key 'score-ranked-reply))

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
  (find :before (reply-qualifiers reply)))

(defun after-reply-p (reply)
  (find :after (reply-qualifiers reply)))

(defun around-reply-p (reply)
  (find :around (reply-qualifiers reply)))

(defun compute-erfun (message replies)
  (let ((around (find-if 'around-reply-p replies))
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
  (reduce (lambda (erfun reply)
            (fun (funcall (reply-function reply) _ erfun)))
          replies :initial-value nil))

(defun find-applicable-replies (message args &optional (errorp t))
  "Returns a sorted list of replies on MESSAGE for which appropriate roles
are present in ARGS. If no such replies are found and ERRORP is true, a
condition of type `no-applicable-replies' is signaled."
  (if (null args)
      (message-replies message)
      (loop with discovered-replies list and boxed-applicable-replies list
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
                    (let ((reply (role-reply role)) boxed-reply)
                      (aif (find reply discovered-replies
                                 :key #'unbox-ranked-reply :test #'eq)
                           (setf boxed-reply it)
                           (push (setf boxed-reply (box-reply reply)) discovered-replies))
                      (setf (svref boxed-reply (the fixnum (1+ index))) hierarchy-position)
                      (when (fully-specified-p boxed-reply)
                        (unless (find reply boxed-applicable-replies
                                      :key #'unbox-ranked-reply :test #'eq)
                          (push boxed-reply boxed-applicable-replies)))))))
         finally
           (if boxed-applicable-replies
               (return (nunbox-replies (sort-applicable-replies boxed-applicable-replies)))
               (when errorp
                 (error 'no-applicable-replies :message (message-name message) :args args))))))
