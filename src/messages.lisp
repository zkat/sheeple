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

(deftype dispatch-cache (&optional size)
  `(simple-vector ,size))

(deftype dispatch-cache-entry ()
  `(cons list function))

(defstruct (message (:constructor %make-message (name lambda-list))
                    (:predicate messagep))
  (name (error "Must supply a name") :read-only t)
  (lambda-list (error "Must supply a lambda-list") :type list)
  (replies nil :type list)
  (documentation nil :type (or string null))
  (dispatch-cache (make-dispatch-cache) :type dispatch-cache)
  (erfun-cache (make-hash-table :test #'equal) :type hash-table)
  ;; These are for argument info
  (number-required 0 :type fixnum)
  (number-optional 0 :type fixnum)
  (key/rest-p nil :type boolean))

(define-print-object ((message message)) (format t "~S" (message-name message)))

;;;
;;; Message Documentation
;;;

(defmethod documentation ((x message) (doc-type (eql 't)))
  (message-documentation x))

(defmethod documentation ((x symbol) (doc-type (eql 'message)))
  (handler-case
      (documentation (find-message x) t)
    (no-such-message ())))

(defmethod (setf documentation) (new-value (x message) (doc-type (eql 't)))
  (setf (message-documentation x) new-value))

(defmethod (setf documentation) (new-value (x symbol) (doc-type (eql 'message)))
  (handler-case
      (setf (documentation (find-message x) t) new-value)
    (no-such-message ())))

;;;
;;; Message definition
;;;

(defvar *message-table* (make-hash-table :test 'equal)
  "Hashtable for storing message objects.") ; EQUAL test accomodates setf messages

(defun message-table-p (table)
  ;; This is sort of stupid, but I'm sort of hoping it's going to help me make the interface more
  ;; generic, in case I ever want to change what the message table is actually represented as.
  (hash-table-p table))

;; We define these two as internal first, so we don't export (setf find-message)
(declaim (inline %find-message))
(defun %find-message (name)
  (values (gethash name *message-table*)))
(defun (setf %find-message) (new-value name)
  (setf (gethash name *message-table*) new-value))

(defun forget-message (name)
  (remhash name *message-table*))

(defun forget-all-messages ()
  (clrhash *message-table*) t)

(defun find-message (name &optional (errorp t))
  "Finds a message object named NAME in `*MESSAGE-TABLE*'.
Raises an error if no message is found, unless ERRORP is NIL."
  (or (%find-message name)
      (when errorp (error 'no-such-message :message-name name))))

;;;
;;; Global dispatch cache
;;;
;;; Currently disabled due to a nuisance with weak pointers
;;;
;;; - We hold a global cache for each message, which is filled as different objects are dispatched on.
;;;   This allows fairly quick lookup of applicable replies when a particular message is called over
;;;   and over on the same arguments.
(defparameter *dispatch-cache-size* 40
  "This variable determines the size of messages' dispatch caches. The bigger the number, the
more entries the cache will be able to hold, but the slower lookup will be.")

(defun make-dispatch-cache (&optional (size *dispatch-cache-size*))
  (make-vector size))

(defun make-dispatch-cache-entry (args replies)
  (cons (mapcar 'maybe-make-weak-pointer args) replies))

;;; There's a certain uglyness here. Too bad this crap is concise. - Adlai
(declaim (inline cache-entry-args cache-entry-replies))
(defun cache-entry-args (entry)
  (car entry))
(defun cache-entry-replies (entry)
  (cdr entry))

(defun cache-replies (message args replies)
  (let* ((dispatch-cache (message-dispatch-cache message))
         (entry (make-dispatch-cache-entry args replies))
         (possible-index (mod (sxhash args) *dispatch-cache-size*)))
    (declare (dispatch-cache dispatch-cache) (fixnum possible-index))
    (acond ((typep (svref dispatch-cache possible-index) 'fixnum)
            (setf (svref dispatch-cache possible-index) entry))
           ((position-if #'atom dispatch-cache)
            (setf (svref dispatch-cache it) entry))
           (t (setf (svref dispatch-cache possible-index) entry)))))

(defun find-cached-replies (message args)
  (let* ((dispatch-cache (message-dispatch-cache message))
         (maybe-entry (svref dispatch-cache 0)))
    (declare (dispatch-cache dispatch-cache))
    (acond ((and (not (typep maybe-entry 'fixnum))
                 (desired-entry-p maybe-entry args))
            (cache-entry-replies maybe-entry))
           ((dotimes (index (length dispatch-cache))
              (awhen (desired-entry-p (svref dispatch-cache index) args)
                (return it)))
            (cache-entry-replies it))
           (t nil))))

(defun desired-entry-p (entry target-args)
  (declare (optimize speed (safety 0)))
  (let ((entry-args (cache-entry-args entry)))
    (do ((pointers entry-args (cdr pointers))
         (pointer (car entry-args) (car pointers))
         (args target-args (cdr args))
         (arg (car target-args) (car args)))
        ((or (null pointers) (null args))
         (when (and (null pointers) (null args))
           entry))
      (unless (eq (maybe-weak-pointer-value pointer) arg)
        (return nil)))))

(defun clear-dispatch-cache (message)
  (setf (message-dispatch-cache message) (make-dispatch-cache))
  (clrhash (message-erfun-cache message))
  t)

(defun clear-all-message-caches ()
  (maphash-values 'clear-dispatch-cache *message-table*))

(defun find-cached-erfun (message replies)
  (gethash replies (message-erfun-cache message)))

(defun cache-erfun (message replies erfun)
  (setf (gethash replies (message-erfun-cache message)) erfun))

;;;
;;; Arg info
;;;
;;; - Arg info objects and the operations on them are meant to check that Message/Reply lambda-lists
;;;   comply with http://www.lispworks.com/documentation/HyperSpec/Body/07_fd.htm
;;;
;;; - These are based on code from SBCL, but have been judiciously frobbed
;;;
;;; - Note that Sheeple doesn't follow CLHS 7.6.4 to the letter.

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
    (setf (message-lambda-list message) lambda-list
          (message-number-required message) nreq
          (message-number-optional message) nopt
          (message-key/rest-p message) (or keysp restp)))
  (values))

;;;
;;; Message definition (finally!)
;;;

;; The defmessage macro basically expands to a call to this function (after processing
;; its args, checking lamda-list, etc.)
(defun ensure-message (name &rest all-keys &key lambda-list &allow-other-keys)
  (or (awhen-prog1 (find-message name nil)
        (set-arg-info it lambda-list))
      (setf (%find-message name)
            (apply 'make-message :name name :lambda-list lambda-list all-keys))))

;; This handles actual setup of the message object (and finalization)
(defun make-message (&key name lambda-list documentation)
  (let ((message (%make-message name lambda-list)))
    (set-arg-info message lambda-list)
    (finalize-message message)
    (setf (documentation message t) documentation)
    message))

;; Finalizing a message sets the function definition of the message to a
;; lambda that calls the top-level dispatch function on the message args.
(defun finalize-message (message)
  (let ((name (message-name message)))
    (when (and (fboundp name) (not (find-message name nil)))
      (cerror "Replace definition." 'clobbering-function-definition :function name))
    (setf (fdefinition name) (lambda (&rest args) (apply-message message args)))))

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
