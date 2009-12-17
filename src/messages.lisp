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
                                      (arg-info (make-arg-info) :accessor nil))))

#+sheeple3.1
(defun %make-message (&key name lambda-list replies (documentation ""))
  (defsheep (=standard-message=) ((name name) (lambda-list lambda-list)
                                  (replies replies)
                                  (memo-vector (make-vector 40))
                                  (arg-info (make-arg-info)))))

;;;
;;; Message object
;;;
;;; - Messages are the Sheeple equivalent of a generic function. Unlike CLOS' generic functions,
;;;   though, the role messages take is smaller. Messages themselves are fairly blind to the dispatch
;;;   mechanism. As far as the user interface goes, they're used as a sort of interface definition.
;;;   The main role they take on (visible to the user) is to maintain lambda-list congruence across
;;;   different replies (sheeple-speak for methods). Sheeple messages follow the same rules as
;;;   CLOS generic functions use for lambda-list congruence, as outlined in
;;;   http://www.lispworks.com/documentation/HyperSpec/Body/07_fd.htm
;;;
;;;   Design-wise, this is a bit of an annoyance, since it means Replies aren't entirely independent.
;;;   I made the decision many tags ago, though, that maintaining CLOS-style lambda-list congruence
;;;   was worth the price. As it turns out, there were other convenient reasons to have a global
;;;   'registry' of replies: All existing replies can be listed for the user, an obvious interface
;;;   can be defined, and the message object can be used as an obvious place to store the cached
;;;   dispatch information.

(defstruct (message (:constructor %make-message)
                    (:predicate messagep))
  name function
  (erfun-cache (make-hash-table :test #'equal))
  (replies nil :type list)
  (documentation nil :type (or string null))
  ;; These are for argument info
  (lambda-list nil :type list)
  (number-required 0 :type fixnum)
  (number-optional 0 :type fixnum)
  (key/rest-p nil :type boolean))

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
      (with-accessors ((msg-nreq       message-number-required)
                       (msg-nopt       message-number-optional)
                       (msg-key/rest-p message-key/rest-p)) message
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
    (setf (message-number-required message) nreq
          (message-number-optional message) nopt
          (message-key/rest-p message) (or keysp restp)))
  (values))

(defun update-arg-info (message lambda-list)
  (multiple-value-bind (new-nreq new-nopt new-keysp new-restp)
      (analyze-lambda-list lambda-list)
    (let ((new-key/rest-p (or new-keysp new-restp)))
      (dolist (reply (message-replies message))
        (multiple-value-bind (reply-nreq reply-nopt reply-keysp reply-restp)
            (analyze-lambda-list (reply-lambda-list reply))
          (unless (and (= new-nreq reply-nreq)
                       (= new-nopt reply-nopt)
                       (eq new-key/rest-p
                           (or reply-keysp reply-restp)))
            (error "The message ~S~%cannot be updated to have lambda-list ~S~@
                    because it conflicts with reply ~S" message lambda-list reply))))
      (setf (message-lambda-list message)     lambda-list
            (message-number-required message) new-nreq
            (message-number-optional message) new-nopt
            (message-key/rest-p message)      new-key/rest-p)))
  (values))

(defun required-portion (message args)
  (let ((number-required (message-number-required message)))
    (error-when (< (length args) number-required)
                insufficient-message-args :message message)
    (subseq args 0 number-required)))

;;;
;;; Message definition (finally!)
;;;

;; The defmessage macro basically expands to a call to this function (after processing
;; its args, checking lamda-list, etc.)
(defun ensure-message (name &rest all-keys &key lambda-list &allow-other-keys)
  ;; FIXME: We really oughtta perform the fboundp check here. We could do things
  ;; properly if we had funcallable messages.
  (or (awhen-prog1 (find-message name nil)
        (update-arg-info it lambda-list))
      (setf (%find-message name)
            (apply 'make-message :name name :lambda-list lambda-list all-keys))))

;; This handles actual setup of the message object (and finalization)
(defun make-message (&key name lambda-list documentation)
  ;; FIXME: This check should really go in ensure-message. We could avoid
  ;; this kind of kludge if we had funcallable messages.
  (when (and (fboundp name) (not (find-message name nil)))
    (cerror "Replace definition." 'clobbering-function-definition :function name))
  (let ((message (%make-message name lambda-list)))
    (set-arg-info message lambda-list)
    (finalize-message message)
    (setf (documentation message t) documentation)
    message))

;; Finalizing a message sets the function definition of the message to a
;; lambda that calls the top-level dispatch function on the message args.
(defun finalize-message (message)
  (setf (message-discriminating-function message) (std-compute-discriminating-function message)
        ;; FIXME: We could avoid this kind of kludge IF WE ONLY HAD FUNCALLABLE MESSAGES.
        (fdefinition (message-name message))      (message-discriminating-function message))
  (flush-erfun-cache message)
  (values))

(defun apply-message (message args)
  (apply (message-discriminating-function message) args))

;;; defmessage macro

;; This is the actual message definition macro.
;; It first verifies that the lambda-list provided is a valid message ll,
;; then expands to a call to ensure-message
;; This pair just pretties up the options during macro expansion
(defmacro defmessage (name lambda-list &rest options)
  (let ((replies (remove-if-not (curry 'eq :reply) options :key 'car))
        (options (delete :reply options :test 'eq :key 'car)))
    `(progn
       (eval-when (:compile-toplevel :load-toplevel :execute)
         (check-message-lambda-list ',lambda-list)
         (ensure-message ',name :lambda-list ',lambda-list
                         ,@(canonize-message-options options)))
       ,@(when replies
           `(,@(mapcar (fun `(defreply ,name ,@(cdr _))) replies)
               (find-message ',name))))))

(defun canonize-message-option (option)
  `(,(car option) ,(cadr option)))

(defun canonize-message-options (options)
  (mapcan 'canonize-message-option options))

;;;
;;; Undefinition
;;;
(defun undefine-message (name)
  (forget-message name)
  (fmakunbound name))

(defmacro undefmessage (name)
  `(undefine-message ',name))
