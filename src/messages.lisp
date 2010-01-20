;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; indent-tabs-mode: nil -*-

;;;; This file is part of Sheeple

;;;; messages.lisp
;;;;
;;;; Message metasheep, message definition and management
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :sheeple)

(defstruct (arg-info (:constructor make-arg-info ()))
  (lambda-list     nil :type list)
  (number-required 0   :type fixnum)
  (number-optional 0   :type fixnum)
  (key/rest-p      nil :type boolean)
  (keys            nil :type list))

(defstruct (%message (:predicate %messagep))
  name message
  (function #'identity :type function)
  (erfun-cache (make-hash-table :test #'equal))
  (replies nil :type list)
  (arg-info (make-arg-info) :type arg-info))

;;;
;;; Funcallable Messages!
;;;

(defvar *funcallable-messages*
  (make-weak-hash-table :test #'eq :weakness :key))

(defun allocate-message ()
  (let ((%message (make-%message)))
    (aprog1 (lambda (&rest args)
              (declare (dynamic-extent args) (optimize speed (safety 0)))
              (apply (%message-function %message) args))
      (setf (gethash it *funcallable-messages*) %message
            (%message-message %message)         it))))

(defun %make-message (name lambda-list)
  (aprog1 (allocate-message)
    (setf (message-name it) name
          (arg-info-lambda-list (message-arg-info it)) lambda-list)))

(macrolet ((with-%message ((name message) &body body)
             (with-gensyms (foundp)
               `(multiple-value-bind (,name ,foundp)
                    (gethash ,message *funcallable-messages*)
                  (if ,foundp (progn ,@body)
                      ;; Note the multiple evaluation of ,message
                      (error 'type-error :datum ,message :expected-type 'message))))))
  (macrolet ((define-message-accessor (slot)
               (let ((%message-accessor (symbolicate '#:%message- slot))
                     (message-accessor (symbolicate '#:message- slot)))
                 `(progn
                    (defun ,message-accessor (message)
                      (with-%message (%message message)
                        (,%message-accessor %message)))
                    (defun (setf ,message-accessor) (new-value message)
                      (with-%message (%message message)
                        (setf (,%message-accessor %message) new-value)))))))
    (define-message-accessor name)
    (define-message-accessor function)
    (define-message-accessor erfun-cache)
    (define-message-accessor replies)
    (define-message-accessor arg-info)))

(defun message-lambda-list (message)
  (arg-info-lambda-list (message-arg-info message)))

(defun messagep (x)
  (nth-value 1 (gethash x *funcallable-messages*)))

(deftype message ()
  ;; FIXME: Now would be a nice time to chat about what a message is
  '(satisfies messagep))

(defun pprint-message (stream message)
  (print-unreadable-object (message stream :identity t)
    (format stream "MESSAGE ~S" (message-name message))))

(set-pprint-dispatch 'message #'pprint-message)

(define-print-object ((%message %message) :type nil)
  (format t "Internal data for ~S" (%message-message %message)))

;;;
;;; Erfun Cache
;;;

(defun cached-erfun (message replies)
  (gethash replies (message-erfun-cache message)))

(defun (setf cached-erfun) (new-erfun message replies)
  (setf (gethash replies (message-erfun-cache message)) new-erfun))

(defun flush-erfun-cache (message)
  (clrhash (message-erfun-cache message)))

;;;
;;; Arg info
;;;

;;; This code ensures that Sheeple follows a simplified form of CLHS 7.6.4
;;; A lot of duplicated code here... FIXME!

(defun check-reply-arg-info (message reply)
  (multiple-value-bind (nreq nopt keysp restp)
      (analyze-lambda-list (reply-lambda-list reply))
    (flet ((lose (string &rest args)
             (error 'reply-argument-conflict
                    :reply reply :message message :reason (apply 'format nil string args)))
           (comparison-description (x y)
             (if (> x y) "more" "fewer")))
      (with-accessors ((msg-nreq       arg-info-number-required)
                       (msg-nopt       arg-info-number-optional)
                       (msg-key/rest-p arg-info-key/rest-p))
          (message-arg-info message)
        (cond ((not (= nreq msg-nreq))
               (lose "the reply has ~A required arguments than the message."
                     (comparison-description nreq msg-nreq)))
              ((not (= nopt msg-nopt))
               (lose "the reply has ~A optional arguments than the message."
                     (comparison-description nopt msg-nopt)))
              ((not (eq (or keysp restp) msg-key/rest-p))
               (lose "the reply and message differ in whether they accept &REST or &KEY arguments."))
              (t (values)))))))

(defun set-arg-info (message lambda-list)
  (multiple-value-bind (nreq nopt keysp restp)
      (analyze-lambda-list lambda-list)
    (with-accessors ((msg-nreq arg-info-number-required)
                     (msg-nopt arg-info-number-optional)
                     (msg-kr-p arg-info-key/rest-p))
        (message-arg-info message)
      (setf msg-nreq nreq
            msg-nopt nopt
            msg-kr-p (or keysp restp))))
  (values))

(defun update-arg-info (message lambda-list)
  (multiple-value-bind (new-nreq new-nopt new-keysp new-restp)
      (analyze-lambda-list lambda-list)
    (let ((new-key/rest-p (or new-keysp new-restp)) remove-conflicts)
      (dolist (reply (message-replies message))
        (multiple-value-bind (reply-nreq reply-nopt reply-keysp reply-restp)
            (analyze-lambda-list (reply-lambda-list reply))
          (unless (and (= new-nreq reply-nreq)
                       (= new-nopt reply-nopt)
                       (eq new-key/rest-p
                           (or reply-keysp reply-restp)))
            (unless remove-conflicts
              (restart-case
                  (cerror "Remove this reply from the message."
                          "~@<The message ~2I~_~S~I~_cannot be updated to have lambda-list ~2I~_~S~
                           ~I~_because it conflicts with reply ~2I~_~S.~:>"
                          message lambda-list reply)
                (remove-all ()
                  :report "Remove all conflicting replies from the message."
                  (setf remove-conflicts t))))
            (delete-reply reply))))
      (with-accessors ((msg-nreq arg-info-number-required)
                       (msg-nopt arg-info-number-optional)
                       (msg-ll   arg-info-lambda-list)
                       (msg-kr-p arg-info-key/rest-p))
          (message-arg-info message)
        (setf msg-ll   lambda-list
              msg-nreq new-nreq
              msg-nopt new-nopt
              msg-kr-p new-key/rest-p))))
  (values))

(defun required-portion (message args)
  (let ((nreq (arg-info-number-required (message-arg-info message))))
    (error-when (< (length args) nreq)
                insufficient-message-args :message message)
    (subseq args 0 nreq)))

;;; The defmessage macro basically expands to a call to this function (after processing
;;; its args, checking lamda-list, etc.)
(defun ensure-message (name &rest all-keys &key lambda-list &allow-other-keys)
  (awhen-prog1 (safe-fdefinition name)
    (when (messagep it)
      (update-arg-info it lambda-list)
      (return-from ensure-message it))
    (cerror "Replace definition." 'clobbering-function-definition :function name))
  (record-message-source name)
  (record-message-arglist name lambda-list)
  (setf (fdefinition name)
        (apply 'make-message :name name :lambda-list lambda-list all-keys)))

;; This handles actual setup of the message object (and finalization)
(defun make-message (&key name lambda-list documentation)
  (aprog1 (%make-message name lambda-list)
    (set-arg-info it lambda-list)
    (finalize-message it)
    (setf (documentation it t) documentation)))

;; Finalizing a message sets the function definition of the message to a
;; lambda that calls the top-level dispatch function on the message args.
(defun finalize-message (message)
  (setf (message-function message) (std-compute-discriminating-function message))
  (flush-erfun-cache message)
  (values))

;;;
;;; Message definition (finally!)
;;;

(defun parse-defmessage (name lambda-list options-and-replies)
  (check-message-lambda-list lambda-list)
  (let (replies options)
    (dolist (option options-and-replies)
      (ecase (car option) ; Choke on unsupported types
        (:reply (push `(defreply ,name ,@(cdr option)) replies))
        (:documentation (push option options))))
    (values replies options)))

(defmacro defmessage (name lambda-list &rest options &environment env)
  (multiple-value-bind (replies options)
      (parse-defmessage name lambda-list options)
    `(progn
       (eval-when (:compile-toplevel)
         ,(record-message-compilation name lambda-list env))
       (aprog1 (ensure-message ',name :lambda-list ',lambda-list
                               ,@(canonize-message-options options))
         ,.replies))))

(defun canonize-message-option (option)
  `(,(car option) ,(cadr option)))

(defun canonize-message-options (options)
  (mapcan 'canonize-message-option options))

(defmacro undefmessage (name)
  `(fmakunbound ',name))
