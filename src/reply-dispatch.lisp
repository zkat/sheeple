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
  (when (member :before (reply-qualifiers reply))
    t))

(defun after-reply-p (reply)
  (when (member :after (reply-qualifiers reply))
    t))

(defun around-reply-p (reply)
  (when (member :around (reply-qualifiers reply))
    t))

(defun apply-message (message args)
  (let ((replies (find-applicable-replies message args)))
    (apply-replies replies args)))

(defparameter *caching-active-p* t)
(defstruct (cache (:type vector))
  message
  around
  primary
  before
  after
  replies)

(defun apply-replies (cache args)
  (funcall (compute-effective-reply-function cache)
           args))

(defun compute-effective-reply-function (cache)
  (let ((replies (cache-replies cache))
        (around (car (cache-around cache)))
        (primaries (cache-primary cache)))
    (when (null primaries)
      (error 'no-primary-replies :message (message-name (cache-message cache))))
    (if around
        (let ((next-erfun
               (compute-effective-reply-function (create-reply-cache
                                                    (cache-message cache)
                                                    (remove around replies)))))
          (lambda (args)
            (funcall (reply-function around) args next-erfun)))
        (let ((next-erfun (compute-primary-erfun (cdr primaries)))
              (befores (cache-before cache))
              (afters (cache-after cache)))
          (lambda (args)
            (when befores
              (dolist (before befores)
                (funcall (reply-function before) args nil)))
            (multiple-value-prog1
                (funcall (reply-function (car primaries)) args next-erfun)
              (when afters
                (dolist (after afters)
                  (funcall (reply-function after) args nil)))))))))

(defun compute-primary-erfun (replies)
  (if (null replies)
      nil
      (let ((next-erfun (compute-primary-erfun (cdr replies))))
        (lambda (args)
          (funcall (reply-function (car replies)) args next-erfun)))))

(defun create-reply-cache (message replies)
  (make-cache
   :message message
   :replies replies
   :primary (remove-if-not #'primary-reply-p replies)
   :around (remove-if-not #'around-reply-p replies)
   :before (remove-if-not #'before-reply-p replies)
   :after (reverse (remove-if-not #'after-reply-p replies))))

(defun find-applicable-replies (message args &key (errorp t))
  (declare (message message))
  (declare (list args))
  (let (;; This doesn't seem to be expensive at all..
         (relevant-args-length (the fixnum (arg-info-number-required (message-arg-info message))))
         ;; If I can avoid calling fetch-memo-vector-entry for singly-dispatched readers, that
         ;; would be -lovely-. Not sure how to do that yet, though.
        )
    (when (< (length args) relevant-args-length)
      (error 'insufficient-message-args :message message))
    (if (= 0 relevant-args-length)
        (let ((relevant-args (subseq args 0 relevant-args-length)))
          (create-reply-cache message (%find-applicable-replies
                                          message relevant-args
                                          :errorp errorp)))
        (let ((memo-entry (fetch-memo-vector-entry args message relevant-args-length)))
          (or memo-entry
              memo-entry
              (let* ((relevant-args (subseq args 0 relevant-args-length))
                     (new-msg-list (%find-applicable-replies message
                                                              relevant-args
                                                              :errorp errorp)))
                (memoize-reply-dispatch message relevant-args new-msg-list)))))))

(declaim (inline desired-vector-entry-p))
(defun desired-vector-entry-p (args vector-entry relevant-args-length)
  (declare (fixnum relevant-args-length))
  (declare (list args))
  (when (vectorp vector-entry)
    (let ((vector-args (weak-pointer-value (vector-entry-args vector-entry))))
      (cond ((= 0 relevant-args-length)
             t)
            ((= 1 relevant-args-length)
             (equal (car args) (car vector-args)))
            (t
             (loop
                for i upto relevant-args-length
                for v-arg in vector-args
                for arg in args
                do (when (not (equal v-arg arg))
                     (return-from desired-vector-entry-p nil))))))))

(defun fetch-memo-vector-entry (args message relevant-args-length)
  (let* ((memo-vector (message-memo-vector message))
         (orig-index (mod (the fixnum (sxhash (if (sheep-p (car args))
                                                    (car args)
                                                    (or (find-boxed-object (car args))
                                                        (box-type-of (car args))))))
                          (length memo-vector))))
    ;; I don't know how this could be any faster. My best choice is probably to avoid calling it.
    (declare (vector memo-vector))
    (declare (fixnum orig-index))
    (let ((attempt (aref memo-vector orig-index)))
      (if (desired-vector-entry-p args attempt relevant-args-length)
          (vector-entry-msg-cache attempt)
          (progn
            (loop for entry across memo-vector
               do (when (desired-vector-entry-p args entry relevant-args-length)
                    (return-from fetch-memo-vector-entry (vector-entry-msg-cache entry))))
            nil)))))

(defstruct (vector-entry (:type vector))
  args
  msg-cache)

(declaim (inline add-entry-to-message))
(defun add-entry-to-message (cache message args index)
  (declare (fixnum index))
  (let ((memo-vector (message-memo-vector message))
        (vector-entry (make-vector-entry
                       :args (make-weak-pointer args)
                       :msg-cache cache)))
    (cond ((loop for i from index below (length (the simple-vector memo-vector))
              when (eql (elt (the simple-vector memo-vector) i) 0)
              do (progn
                   (setf (elt memo-vector i) vector-entry)
                   (return t))
              finally (return nil))
           t)
          ((loop for i from index downto 0
             when (eql (elt memo-vector i) 0)
             do (progn
                  (setf (elt memo-vector i) vector-entry)
                  (return t))
              finally (return nil))
           t)
          ((setf (elt memo-vector index) vector-entry)
           t))))

(defun memoize-reply-dispatch (message args msg-list)
  (let ((msg-cache (create-reply-cache message msg-list))
        (maybe-index (mod (the fixnum (sxhash (if (sheep-p (car args))
                                                    (car args)
                                                    (or (find-boxed-object (car args))
                                                        (box-type-of (car args))))))
                          (length (the vector (message-memo-vector message))))))
    (add-entry-to-message msg-cache message args maybe-index)
    msg-cache))

(defun %find-applicable-replies  (message args &key (errorp t))
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
           do (let* ((arg (if (sheep-p arg)
                              arg
                              (or (find-boxed-object arg)
                                  (box-type-of arg))))
                     (curr-sheep-list (sheep-hierarchy-list arg)))
                (loop
                   for curr-sheep in curr-sheep-list
                   for hierarchy-position upto (1- (length curr-sheep-list))
                   do (dolist (role (sheep-direct-roles curr-sheep))
                        (when (and (equal selector (role-name role)) ;(eql message (role-message role))
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
              (error 'no-applicable-replies :message selector :args args))))))

(defun unbox-replies (replies)
  (mapcar #'reply-container-reply replies))

(defun sort-applicable-replies (reply-list &key (rank-key #'<))
  (sort reply-list rank-key
        :key (lambda (contained-reply)
               (calculate-rank-score (reply-container-rank contained-reply)))))

(defun contain-reply (reply)
  (make-reply-container
   :reply reply
   :rank (make-vector (length (reply-specialized-portion reply)))))

(defstruct (reply-container (:type vector))
  reply
  rank)

(defun fully-specified-p (rank)
  (loop for item across rank
     do (when (eql item nil)
          (return-from fully-specified-p nil)))
  t)

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
