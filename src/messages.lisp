;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; indent-tabs-mode: nil -*-

;;;; This file is part of Sheeple

;;;; messages.lisp
;;;;
;;;; Message metasheep, message definition and management
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :sheeple)

#+sheeple3.1
(define-bound-variable =standard-message=)
#+sheeple3.1
(defparameter the-std-message-form '(defproto =standard-message= ()
                                     ((name nil :accessor nil)
                                      (lambda-list nil :accessor nil)
                                      (replies nil :accessor nil)
                                      (memo-vector (make-vector 40) :accessor nil)
                                      (arg-info (make-arg-info) :accessor nil)
                                      (documentation "" :accessor nil))))

#+sheeple3.1
(defun %make-message (&key name lambda-list replies (documentation ""))
  (defsheep (=standard-message=) ((name name) (lambda-list lambda-list)
                                  (replies replies) (documentation documentation)
                                  (memo-vector (make-vector 40))
                                  (arg-info (make-arg-info)))))

(defstruct (message (:constructor %make-message)
                    (:predicate messagep))
  (name nil)
  (lambda-list nil)
  (replies nil)
  (dispatch-cache (make-vector 40))
  (arg-info (make-arg-info))
  (documentation ""))

;;;
;;; Message definition
;;;

;;; Message table
(defvar *message-table* (make-hash-table :test #'equal)
  "Hashtable for storing message objects. Needs to :test #'equal because
of setf methods, whose names are lists.")

(defun message-table-p (table)
  (hash-table-p table))

(defun find-message (name &optional (errorp t))
  "Finds a message object in `*message-table*', given its `name'.
Raises an error if no message is found, unless `errorp' is set to NIL."
  (or (gethash name *message-table*)
      (when errorp (error 'no-such-message :message-name name))))

(defun (setf find-message) (new-value name)
  (setf (gethash name *message-table*) new-value))

(defun forget-message (name)
  (remhash name *message-table*))

(defun forget-all-messages ()
  (clrhash *message-table*) t)

;;; Reply caching
;; NOTE: Even though these two functions are defined here, their tests are in
;;       tests/reply-dispatch.lisp, since it's the most relevant place for them to be.
;;       In the case of implementation, they live here because this is where message
;;       object stuff lives. This could be changed later, ofc. --zkat
(defun clear-dispatch-cache (message)
  (setf (message-dispatch-cache message) (make-vector 40)))

(defun clear-all-message-caches ()
  (maphash-values 'clear-memo-table *message-table*))

;;;
;;; Arg info
;;; - The stuff in here contains the arg-info object, plus code to handle it.
;;;   Present is also the function that confirms validity of reply lambda-lists,
;;;   and the code that updates the valid arg info for a message whenever a reply
;;;   is added. The add-reply function, though, is in reply-generation.lisp
(defstruct (arg-info (:conc-name nil)
                     (:constructor make-arg-info ())
                     (:copier nil))
  (arg-info-lambda-list :no-lambda-list)
  arg-info-precedence
  arg-info-metatypes
  arg-info-number-optional
  arg-info-key/rest-p
  arg-info-keys                         ;nil        no &KEY or &REST allowed
                                        ;(k1 k2 ..) Each reply must accept these &KEY arguments.
                                        ;T          must have &KEY or &REST
  msg-info-simple-accessor-type         ; nil, reader, writer, boundp
  (msg-precompute-dfun-and-emf-p nil)   ; set by set-arg-info
  msg-info-static-c-a-m-emf
  (msg-info-c-a-m-emf-std-p t)
  msg-info-fast-mf-p)

(defun arg-info-valid-p (arg-info)
  (not (null (arg-info-number-optional arg-info))))

(defun arg-info-applyp (arg-info)
  (or (plusp (the fixnum (arg-info-number-optional arg-info)))
      (arg-info-key/rest-p arg-info)))

(defun arg-info-number-required (arg-info)
  (length (the simple-array (arg-info-metatypes arg-info))))

(defun arg-info-nkeys (arg-info)
  (count-if (fun (not (eq _ t))) (arg-info-metatypes arg-info)))

(defun set-arg-info (msg &key new-reply (lambda-list nil lambda-list-p))
  (let* ((arg-info (message-arg-info msg))
         (replies (message-replies msg))
         (first-p (and new-reply (null (cdr replies)))))
    (when (and (not lambda-list-p) replies)
      (setq lambda-list (message-lambda-list msg)))
    (when (or lambda-list-p
              (and first-p
                   (eq (arg-info-lambda-list arg-info) :no-lambda-list)))
      (multiple-value-bind (nreq nopt keysp restp allow-other-keys-p keywords)
          (analyze-lambda-list lambda-list)
        (when (and replies (not first-p))
          (let ((msg-nreq (arg-info-number-required arg-info))
                (msg-nopt (arg-info-number-optional arg-info))
                (msg-key/rest-p (arg-info-key/rest-p arg-info)))
            (unless (and (= (the fixnum nreq) msg-nreq)
                         (= (the fixnum nopt) (the fixnum msg-nopt))
                         (eq (or keysp restp) msg-key/rest-p))
              (error 'reply-lambda-list-conflict
                     :lambda-list lambda-list :message msg))))
        (setf (arg-info-lambda-list arg-info)
              (if lambda-list-p
                  lambda-list
                  (create-msg-lambda-list lambda-list)))
        (setf (arg-info-metatypes arg-info) (make-vector nreq))
        (setf (arg-info-number-optional arg-info) nopt)
        (setf (arg-info-key/rest-p arg-info) (not (null (or keysp restp))))
        (setf (arg-info-keys arg-info)
              (if lambda-list-p
                  (if allow-other-keys-p t keywords)
                  (arg-info-key/rest-p arg-info)))))
    (when new-reply
      (check-reply-arg-info msg arg-info new-reply))
    arg-info))

(defun analyze-lambda-list (lambda-list)
  (labels ((make-keyword (symbol)
             (intern (symbol-name symbol)
                     (find-package 'keyword)))
           (parse-key-arg (arg)
             (if (listp arg)
                 (if (listp (car arg))
                     (caar arg)
                     (make-keyword (car arg)))
                 (make-keyword arg))))
    (let ((nrequired 0)
          (noptional 0)
          (keysp nil)
          (restp nil)
          (nrest 0)
          (allow-other-keys-p nil)
          (keywords nil)
          (keyword-parameters nil)
          (state 'required))
      (dolist (x lambda-list)
        (if (memq x lambda-list-keywords)
            (case x
              (&optional         (setq state 'optional))
              (&key              (setq keysp t
                                       state 'key))
              (&allow-other-keys (setq allow-other-keys-p t))
              (&rest             (setq restp t
                                       state 'rest))
              (&aux           (return t))
              (otherwise
               (error "encountered the non-standard lambda list keyword ~S"
                      x)))
            (ecase state
              (required  (incf (the fixnum nrequired)))
              (optional  (incf (the fixnum noptional)))
              (key       (push (parse-key-arg x) keywords)
                         (push x keyword-parameters))
              (rest      (incf (the fixnum nrest))))))
      (when (and restp (zerop nrest))
        (error "Error in lambda-list:~%~
                After &REST, a DEFMESSAGE lambda-list ~
                must be followed by at least one variable."))
      (values nrequired noptional keysp restp allow-other-keys-p
              (reverse keywords)
              (reverse keyword-parameters)))))

(defun check-reply-arg-info (msg arg-info reply)
  (multiple-value-bind (nreq nopt keysp restp allow-other-keys-p keywords)
      (analyze-lambda-list (reply-lambda-list reply))
    (flet ((lose (string &rest args)
             (error 'sheeple-error
                    :format-control "~@<attempt to add the reply~2I~_~S~I~_~
                                     to the message~2I~_~S;~I~_~
                                     but ~?~:>"
                    :format-args (list reply msg string args)))
           (comparison-description (x y)
             (declare (fixnum x y))
             (if (> x y) "more" "fewer")))
      (let ((msg-nreq (arg-info-number-required arg-info))
            (msg-nopt (arg-info-number-optional arg-info))
            (msg-key/rest-p (arg-info-key/rest-p arg-info))
            (msg-keywords (arg-info-keys arg-info)))
        (unless (= nreq msg-nreq)
          (lose
           "the reply has ~A required arguments than the message."
           (comparison-description nreq msg-nreq)))
        (unless (= nopt msg-nopt)
          (lose
           "the reply has ~A optional arguments than the message."
           (comparison-description nopt msg-nopt)))
        (unless (eq (or keysp restp) msg-key/rest-p)
          (lose
           "the reply and message differ in whether they accept~_~
            &REST or &KEY arguments."))
        (when (consp msg-keywords)
          (unless (or (and restp (not keysp))
                      allow-other-keys-p
                      (every (fun (memq _ keywords)) msg-keywords))
            (lose "the reply does not accept each of the &KEY arguments~2I~_~
                   ~S."
                  msg-keywords)))))))

(defun check-msg-lambda-list (lambda-list)
  (flet ((ensure (arg ok)
           (unless ok
             (error 'message-lambda-list-error :arg arg :lambda-list lambda-list))))
    (multiple-value-bind (required optional restp rest keyp keys allowp
                                   auxp aux morep more-context more-count)
        (parse-lambda-list lambda-list)
      (declare (ignore required)) ; since they're no different in a msg ll
      (declare (ignore restp rest)) ; since they're no different in a msg ll
      (declare (ignore allowp)) ; since &ALLOW-OTHER-KEYS is fine either way
      (declare (ignore aux)) ; since we require AUXP=NIL
      (declare (ignore more-context more-count)) ; safely ignored unless MOREP
      ;; no defaults allowed for &OPTIONAL arguments
      (mapc (fun (ensure _ (or (symbolp _)
                               (and (consp _)
                                    (symbolp (car _))
                                    (null (cdr _))))))
            optional)
      ;; no defaults allowed for &KEY arguments
      (when keyp
        (mapc (fun (ensure _ (or (symbolp _)
                                 (and (consp _)
                                      (or (symbolp (car _))
                                          (and (consp (car _))
                                               (symbolp (caar _))
                                               (symbolp (cadar _))
                                               (null (cddar _))))
                                      (null (cdr _))))))
              keys))
      ;; no &AUX allowed
      (when auxp
        (error "&AUX is not allowed in a message lambda list: ~S" lambda-list)))))

;; Finalizing a message sets the function definition of the message to a
;; lambda that calls the top-level dispatch function on the msg args.
(defun finalize-message (message)
  (let ((name (message-name message)))
    (when (and (fboundp name)
               (not (find-message name nil)))
      (cerror "Replace definition." 'clobbering-function-definition :format-args (list name)))
    (setf (fdefinition name) (lambda (&rest args) (apply-message message args)))))

;; This handles actual setup of the message object (and finalization)
(defun make-message (&key name
                     lambda-list
                     (documentation ""))
  (let ((message (%make-message
                  :name name
                  :lambda-list lambda-list
                  :documentation documentation)))
    (set-arg-info message :lambda-list lambda-list)
    (finalize-message message)
    message))

;; The defmessage macro basically expands to a call to this function (after processing
;; its args, checking lamda-list, etc.)
(defun ensure-message (name
                       &rest all-keys
                       &key lambda-list
                       &allow-other-keys)
  (let* ((existing (find-message name nil))
         (message (or existing
                      (apply 'make-message
                             :name name
                             :lambda-list lambda-list
                             all-keys))))
    (when existing (set-arg-info message :lambda-list lambda-list))
    (setf (find-message name) message)))

;; This is the actual message definition macro.
;; It first verifies that the lambda-list provided is a valid message ll,
;; then expands to a call to ensure-message
;; This pair just pretties up the options during macro expansion
(defun canonize-message-option (option)
  (list `',(car option) `',(cadr option)))
(defun canonize-message-options (options)
  (mapcan #'canonize-message-option options))

(defmacro defmessage (name lambda-list &rest options)
  `(progn
     (check-msg-lambda-list ',lambda-list)
     (eval-when (:compile-toplevel :load-toplevel :execute)
       (ensure-message
        ',name
        :lambda-list ',lambda-list
        ,@(canonize-message-options options)))))


