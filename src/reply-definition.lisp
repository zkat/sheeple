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
(defstruct (reply (:predicate replyp)
                  (:print-object
                   (lambda (reply stream)
                     (print-unreadable-object (reply stream :identity t)
                       (format stream "Reply: ~a" (reply-name reply))))))
  ;; Replies are the Sheeple equivalent of methods. Replies themselves are objects that hold
  ;; some basic information about what the reply does, what kind of reply it is, etc.
  ;; When reply objects are 'called', their reply-function is fetched directly. By using lambdas,
  ;; we're able to latch on to the lexical environment the reply was defined in (so they can be
  ;; closures)
  message qualifiers lambda-list documentation
  (function (constantly nil)))

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
;;; - Roles encapsulate the idea of dispatch. Roles live in object objects themselves and represent
;;;   the basic information about what 'role' that particular object has in dispatching on a
;;;   particular message. As it turns out, all the information roles have to hold is the position
;;;   in which it is supposed to be called, and the actual reply object it's associated with.
;;;   The algorithm takes care of putting everything else together.
(defun make-role (reply position)
  (cons reply position))
(defun role-reply (role)
  (car role))
(defun role-position (role)
  (cdr role))

(deftype role ()
  '(cons reply fixnum))
(defun rolep (maybe-role)
  (typep maybe-role 'role))

(defun pprint-role (stream role)
  (print-unreadable-object (role stream :identity t)
    (format stream "Role: ~A" (role-name role))))
(set-pprint-dispatch 'role 'pprint-role 1)

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
(defun ensure-reply (name &key qualifiers lambda-list participants function (documentation ""))
  ;; shouldn't this just be a plain call to ensure-message? -- syko
  ;; maybe not. Look into it more -- syko
  (let* ((message (or (find-message name nil)
                      (progn (warn 'automatic-message-creation :message-name name)
                             (ensure-message name :lambda-list
                                             (create-msg-lambda-list lambda-list)))))
         (reply (make-reply :message (find-message name)
                            :lambda-list lambda-list
                            :qualifiers qualifiers
                            :function function))
         (objectified-participants (objectify-list participants)))
    (setf (documentation reply 't) documentation) ; same as dox for CLOS methods
    (clear-dispatch-cache message)                ; because the dispatch landscape has changed
    ;; In order to replace existing replies, we must remove them before actually adding them again.
    (remove-specific-reply message qualifiers objectified-participants)
    (add-reply-to-message reply message)
    (add-reply-to-objects reply objectified-participants)
    reply))

(defun add-reply-to-message (reply message)
  (set-arg-info message :new-reply reply)
  (push reply (message-replies message)))

(defun add-reply-to-objects (reply objects)
  (loop
     for object in objects
     for i from 0
     do (push (make-role reply i)
              (%object-roles object))))

(defun available-replies (object)
  (delete-duplicates
   (append (%object-roles object) (mapcan 'available-replies (object-parents object)))
   :test 'equal))

(defun add-reader-to-object (reader prop-name object)
  (ensure-message reader :lambda-list '(object))
  (ensure-reply reader
                :lambda-list '(object)
                :participants (list object)
                :function (eval (make-reply-lambda reader '(object) ()
                                                   `((property-value object ',prop-name))))))

(defun add-readers-to-object (readers prop-name object)
  (map nil (fun (add-reader-to-object _ prop-name object)) readers)
  object)

(defun add-writer-to-object (writer prop-name object)
  (ensure-message writer :lambda-list '(new-value object))
  (ensure-reply writer
                :lambda-list '(new-value object)
                :participants (list =t= object)
                :function (eval (make-reply-lambda writer '(new-value object) ()
                                                   `((setf (property-value object ',prop-name)
                                                           new-value))))))

(defun add-writers-to-object (writers prop-name object)
  (map nil (fun (add-writer-to-object _ prop-name object)) writers)
  object)

;;;
;;; Reply undefinition
;;;
(defun undefine-reply (name &key qualifiers participants)
  (awhen (find-message name nil)
    (remove-applicable-reply it qualifiers (objectify-list participants))
    (clear-dispatch-cache it)
    t))

(defun remove-specific-reply (message qualifiers participants)
  (let ((reply (find-if (fun (equal qualifiers (reply-qualifiers _)))
                        (find-applicable-replies ;defined in reply-dispatch.lisp
                         message participants nil))))
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
                        (find-applicable-replies
                         message participants nil))))
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

(defun delete-reply (reply)
  (deletef (message-replies (reply-message reply)) reply)
  (setf (documentation reply 't) nil))

(defun delete-role (role object)
  (deletef (%object-roles object) role))

;;;
;;; User interface
;;;

;;; Definition
(defmacro defreply (&rest args)
  (multiple-value-bind (name qualifiers specialized-lambda-list docstring body)
      (parse-defreply args)
    `(%defreply-expander ,name ,qualifiers ,specialized-lambda-list ,docstring ,body)))

(defmacro %defreply-expander (name qualifiers specialized-lambda-list docstring body)
  (multiple-value-bind (parameters ll participants required ignorable)
      (parse-specialized-lambda-list specialized-lambda-list)
    (declare (ignore parameters required))
    `(ensure-reply
      ',name
      :qualifiers ',qualifiers
      ;; TODO - use the new stuff
      :lambda-list ',ll
      :participants (list ,@participants)
      :documentation ,docstring
      :function ,(make-reply-lambda name ll ignorable body))))

(defun make-reply-lambda (name lambda-list ignorable body)
  (let* ((msg (find-message name nil))
         (key/restp (when msg (arg-info-key/rest-p (message-arg-info msg))))
         (ll (if (and key/restp (arg-info-keys (message-arg-info msg)))
                 (append lambda-list '(&allow-other-keys))
                 lambda-list)))
    `(lambda (args next-erfun)
       (declare (ignorable next-erfun))
       (flet ((next-reply-p ()
                (not (null next-erfun)))
              (call-next-reply (&rest cnm-args)
                (if (null next-erfun)
                    (error "No next reply")
                    (funcall next-erfun (or cnm-args args)))))
         (declare (ignorable #'next-reply-p #'call-next-reply))
         (block ,(if (listp name)
                     (cadr name)
                     name)
           (apply
            (lambda ,ll (declare (ignorable ,@ignorable))
              ,@body) args))))))

(defun parse-defreply (args)
  (let ((name (car args))
        (qualifiers nil)
        (lambda-list nil)
        (docstring nil)
        (body nil)
        (parse-state :qualifiers))
    (dolist (arg (cdr args))
      (ecase parse-state
        (:qualifiers
         (if (and (atom arg) (not (null arg)))
             (push arg qualifiers)
             (progn (setf lambda-list arg)
                    (setf parse-state :docstring))))
        (:docstring
         (if (stringp arg)
             (setf docstring arg)
             (push arg body))
         (setf parse-state :body))
        (:body (push arg body))))
    (values name
            qualifiers
            lambda-list
            docstring
            (nreverse body))))

;;; Undefinition
(defmacro undefreply (name &rest args)
  (multiple-value-bind (qualifiers lambda-list)
      (parse-undefreply args)
    `(undefine-reply
      ',name :qualifiers ',qualifiers
      :participants `(,,@(nth-value 3 (parse-specialized-lambda-list lambda-list))))))

(defun parse-undefreply (args)
  (let ((qualifiers nil)
        (lambda-list nil))
    (dolist (arg args)
      (if (and (atom arg) (not (null arg)))
          (push arg qualifiers)
          (setf lambda-list arg)))
    (values qualifiers lambda-list)))
