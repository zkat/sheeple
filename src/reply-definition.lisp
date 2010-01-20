;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; indent-tabs-mode: nil -*-

;;;; This file is part of Sheeple

;;;; reply-definition.lisp
;;;;
;;;; Reply and role metasheeple, reply definition and undefinition, role management.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :sheeple)

;;;
;;; Reply objects
;;;

;;; Replies are the Sheeple equivalent of methods. Replies themselves are objects that hold
;;; some basic information about what the reply does, what kind of reply it is, etc.
;;; When reply objects are 'called', their reply-function is fetched directly. By using lambdas,
;;; we're able to latch on to the lexical environment the reply was defined in (so they can be
;;; closures)

(defstruct (reply (:predicate replyp)
                  (:constructor
                   make-reply (message qualifiers lambda-list function
                                       &aux (rank-vector ; This dies if MESSAGE is not a
                                             (make-array ; message -- as it should!
                                              (arg-info-number-required
                                               (message-arg-info message)))))))
  ;; These are set permanently when the reply is created
  (message (error "Must supply a message") :type message :read-only t)
  (qualifiers (error "Must supply qualifiers") :type list :read-only t)
  (lambda-list (error "Must supply lambda-list") :type list :read-only t)
  (function (error "Must supply a function") :type function :read-only t)
  ;; This can be changed dynamically, but must be (or string null) -- clhs documentation
  (documentation nil :type (or string null))
  ;; This is set at creation, and is frobbed in FIND-APPLICABLE-REPLIES
  (rank-vector (error "Bug in Sheeple") :type simple-vector :read-only t))

(define-print-object ((reply reply)) (format t "~S" (reply-name reply)))

;;;
;;; Reply Documentation
;;;

(defmethod documentation ((x reply) (doc-type (eql 't)))
  (reply-documentation x))

(defmethod (setf documentation) (new-value (x reply) (doc-type (eql 't)))
  (setf (reply-documentation x) new-value))

(defun reply-name (reply)
  (message-name (reply-message reply)))

;;;
;;; Roles
;;;

;;; Roles encapsulate the idea of dispatch. Roles live in object objects themselves and represent
;;; the basic information about what 'role' that particular object has in dispatching on a
;;; particular message. As it turns out, all the information roles have to hold is the position
;;; in which it is supposed to be called, and the actual reply object it's associated with.
;;; The algorithm takes care of putting everything else together.

(deftype role ()
  '(cons reply fixnum))
(defun rolep (maybe-role)
  (typep maybe-role 'role))

(declaim (ftype (function (reply fixnum) role) make-role)
         (ftype (function (role) reply) role-reply)
         (ftype (function (role) fixnum) role-position)
         (inline make-role role-reply role-position))

(defun make-role (reply position)
  (cons reply position))

(defun role-reply (role)
  (car role))
(defun role-position (role)
  (cdr role))

(defun pprint-role (stream role)
  (print-unreadable-object (role stream :identity t)
    (format stream "Role: ~A" (role-name role))))
(set-pprint-dispatch 'role 'pprint-role 1)

(declaim (inline role-message role-name)
         (ftype (function (role) message) role-message))

(defun role-message (role)
  (reply-message (role-reply role)))
(defun role-name (role)
  (reply-name (role-reply role)))

(defun participantp (object reply)
  "Checks if OBJECT is actually involved in dispatching REPLY"
  (when (find-if (curry 'eq reply)
                 (%object-roles object) :key 'role-reply)
    t))

;;;
;;; Reply definition
;;;

;;; FIXME: I do lots of runtime computation on probably constant values.
;;;        Write me a compiler macro!
(defun ensure-reply (name &key qualifiers lambda-list participants function (documentation ""))
  ;; FIXME: This is a slight kludge, because the -same- checks on
  ;; `fboundp' and `messagep' will be done in `ensure-message'.
  (unless (messagep (safe-fdefinition name))
    (warn 'automatic-message-creation :message-name name))
  (assert (= (length participants) (count-required-parameters lambda-list))
          (participants lambda-list)
          "~&The number of participants conflicts with the lambda list.~@
             Participants: ~S~%Lambda List:  ~S~%" participants lambda-list)
  ;; FIXME: This is a quick and dirty way of not clobbering reply-less messages with a
  ;; conflicting arglist. Ideally, we should use some arg-info functions or something...
  (awhen (safe-fdefinition name)
    (assert (= (length participants) (arg-info-number-required (message-arg-info it)))))
  (ensure-message name :lambda-list (create-msg-lambda-list lambda-list))
  (let ((message (fdefinition name)))
    (aprog1 (make-reply message qualifiers lambda-list function)
      (setf (documentation it 't) documentation) ; same as dox for CLOS methods
      ;; In order to replace existing replies, we must remove them before actually adding them again.
      (let ((boxed-participants (ensure-boxed-objects participants)))
        (remove-specific-reply message qualifiers boxed-participants)
        (add-reply-to-objects it boxed-participants))
      (add-reply-to-message it message))))

(defun add-reply-to-message (reply message)
  (check-reply-arg-info message reply)
  (push reply (message-replies message))
  (finalize-message message))

(defun add-reply-to-objects (reply objects)
  (loop
     for object in objects
     for i from 0
     do (push (make-role reply i)
              (%object-roles object))))

(defun available-replies (object)
  (delete-duplicates
   (nconc (mapcar 'car (%object-roles object))
          (mapcan 'available-replies
                  (object-parents object)))))

(defun add-reader-to-object (reader prop-name object)
  (ensure-message reader :lambda-list '(object))
  (ensure-reply reader
                :lambda-list '(object)
                :participants (list object)
                :function (eval (std-make-reply-lambda
                                 `(lambda (object) (property-value object ',prop-name))))))

(defun add-readers-to-object (readers prop-name object)
  (map nil (fun (add-reader-to-object _ prop-name object)) readers)
  object)

(defun add-writer-to-object (writer prop-name object)
  (ensure-message writer :lambda-list '(new-value object))
  (ensure-reply writer
                :lambda-list '(new-value object)
                :participants (list =t= object)
                :function (eval (std-make-reply-lambda
                                 `(lambda (new-value object)
                                    (setf (property-value object ',prop-name) new-value))))))

(defun add-writers-to-object (writers prop-name object)
  (map nil (fun (add-writer-to-object _ prop-name object)) writers)
  object)

;;;
;;; Reply undefinition
;;;

;;; These functions all need clear documentation on what they're supposed to do,
;;; and what they're supposed to return.

(defun undefine-reply (name &key qualifiers participants)
  (awhen (safe-fdefinition name)
    (remove-applicable-reply it qualifiers (ensure-boxed-objects participants))))

(defun remove-specific-reply (message qualifiers participants)
  (let ((reply (find-if (fun (equal qualifiers (reply-qualifiers _)))
                        (compute-applicable-replies message participants))))
    (when (and reply
               (every (rcurry 'participantp reply) participants))
      (loop
         for object in participants
         for i from 0
         do (map nil (fun (when (and (eq reply (role-reply _))
                                     (= i (role-position _)))
                            (delete-role _ object)))
                 (%object-roles object)))
      (delete-reply reply))))

(defun remove-applicable-reply (message qualifiers participants)
  (let ((reply (find-if (fun (equal qualifiers (reply-qualifiers _)))
                        (compute-applicable-replies message participants))))
    (when reply
      (loop
         for object in participants
         for i from 0
         do (map nil (fun (when (and (eq reply (role-reply _))
                                     (= i (role-position _)))
                            (delete-role _ object)))
                 (%object-roles object)))
      (delete-reply reply)
      t)))

(defun delete-reply (reply &aux (message (reply-message reply)))
  (deletef (message-replies message) reply)
  (finalize-message message)
  (setf (documentation reply 't) nil))

(defun delete-role (role object)
  (deletef (%object-roles object) role))

;;;
;;; User interface
;;;

(defmacro defreply (&whole whole name lambda-list &body body)
  (declare (ignore lambda-list body))
  (multiple-value-bind (qualifiers reply-ll declarations docstring body)
      (parse-defreply whole)
    (multiple-value-bind (parameters lambda-list participants required ignorable)
        (parse-specialized-lambda-list reply-ll)
      (declare (ignore parameters required))
      `(progn
         (eval-when (:compile-toplevel)
           (unless (fboundp ',name)
             (warn 'automatic-message-creation :message-name ',name)
             (ensure-message ',name :lambda-list ',(create-msg-lambda-list reply-ll))))
         (ensure-reply ',name
                       :qualifiers ',qualifiers
                       :lambda-list ',lambda-list
                       :participants (list ,@participants)
                       :documentation ,docstring
                       :function ,(std-make-reply-lambda
                                   `(lambda ,(allow-other-keys lambda-list)
                                      (declare (ignorable ,@ignorable)) ,@declarations
                                      (block ,(if (listp name) (cadr name) name) ,@body))))))))

(defun std-make-reply-lambda (lambda-expression)
  (with-gensyms (args next-replies reply-function more-args)
    `(lambda (,args ,next-replies &rest ,more-args)
       (declare (ignorable ,args ,next-replies ,more-args))
       (flet ((next-reply-p ()
                (not (null ,next-replies)))
              (call-next-reply (&rest args)
                (if (null ,next-replies)
                    (apply 'no-next-reply (getf ,more-args :message)
                           (getf ,more-args :reply) (or args ,args))
                    (apply (reply-function (car ,next-replies))
                           (or args ,args) (cdr ,next-replies) ,more-args))))
         (declare (ignorable #'next-reply-p #'call-next-reply))
         (flet ((,reply-function ,@(cdr lambda-expression)))
           (declare (inline ,reply-function))
           (apply #',reply-function ,args))))))

(defun parse-defreply (whole &aux qualifiers current (body (cddr whole)))
  (loop
     (setf current (car body))
     (if (not (listp current)) (push (pop body) qualifiers)
         (return
           (multiple-value-bind (real-body declarations docstring)
               (parse-body (cdr body) :documentation t :whole whole)
             (values (nreverse qualifiers) current ; current = reply lambda list
                     declarations docstring real-body))))))

(defmacro undefreply (name &rest args)
  (multiple-value-bind (qualifiers specializers)
      (parse-undefreply args)
    `(undefine-reply ',name :qualifiers ',qualifiers
                     :participants `(,,@specializers))))

(defun parse-undefreply (args)
  (let (qualifiers specializers)
    (dolist (arg args (values qualifiers specializers))
      (if (and (atom arg) (not (null arg)))
          (push arg qualifiers)
          (setf specializers arg)))))
