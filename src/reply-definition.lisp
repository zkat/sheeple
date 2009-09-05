;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; indent-tabs-mode: nil -*-

;;;; This file is part of Sheeple

;;;; reply-definition.lisp
;;;;
;;;; Reply and role metasheeple, reply definition and undefinition, role management.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :sheeple)

(defstruct (reply (:constructor %make-reply))
  ;; Replies are the Sheeple equivalent of methods. Replies themselves are objects that hold
  ;; some basic information about what the reply does, what kind of reply it is, etc.
  ;; When reply objects are 'called', their reply-function is fetched directly. By using lambdas,
  ;; we're able to latch on to the lexical environment the reply was defined in (so they can be
  ;; closures)
  name qualifiers lambda-list
  (body '(lambda () nil)) ; I could get rid of this. It makes messages pretty heavy...
  (function (lambda () nil)))

(defun reply-message (reply)
  ;; Instead of adding a backlink to the message, we just do a global search for that message.
  (find-message (reply-name reply) nil))

(defstruct (role (:constructor %make-role))
  ;; Roles are objects stored directly in sheeple objects that represent some information
  ;; necessary to dispatching a particular reply.
  name position reply)

(defun role-message (role)
  ;; Again, we just have a global lookup instead of a backlink.
  (find-message (role-name role) nil))

(defun participantp (sheep reply-name)
  (when (member-if (compose (curry 'equal reply-name) 'role-name)
                   (sheep-direct-roles sheep))
    t))

(defun ensure-reply (name &rest all-keys
                     &key lambda-list participants
                     &allow-other-keys)
  (when (not (find-message name nil))
    (progn
      (warn 'style-warning)
      ;; FIXME: can't just give the lambda-list over. Should prepare it for messages
      (ensure-message
       name :lambda-list (create-msg-lambda-list lambda-list))))
  (let* ((message (find-message name))
         (target-sheeple (sheepify-list participants))
         (reply (apply
                 'generate-reply
                 :message message
                 :lambda-list lambda-list
                 :participants target-sheeple
                 all-keys)))
    (clear-memo-table message)
    reply))

(defun generate-reply (&key qualifiers
                       lambda-list
                       participants
                       message
                       function
                       body
                       (documentation ""))
  (let ((reply (%make-reply
                :name (message-name message)
                :lambda-list lambda-list
                :qualifiers qualifiers
                :function function
                :body body
                :documentation documentation)))
    (remove-specific-reply message qualifiers participants)
    (add-reply-to-message reply message)
    (add-reply-to-sheeple message reply participants)
    reply))

(defun create-msg-lambda-list (lambda-list)
  ;;; Create a message lambda list from a reply lambda list
  (loop for x in lambda-list
     collect (if (consp x) (list (car x)) x)
     if (eq x '&key) do (loop-finish)))

(defun add-reply-to-message (reply message)
  (set-arg-info message :new-reply reply)
  (push reply (message-replies message)))

(defun remove-specific-reply (message qualifiers participants)
  (let ((reply (find-if (lambda (msg)
                          (equal (reply-qualifiers msg)
                                 qualifiers))
                        (%find-applicable-replies
                         message participants :errorp nil))))
    (when (and reply
               (every (lambda (sheep)
                        (participantp sheep (reply-name reply)))
                      participants))
      (loop for sheep in participants
         for i from 0
         do (loop for role in (sheep-direct-roles sheep)
               do (let ((role-reply (role-reply role)))
                    (when (and
                           (equal reply role-reply)
                           (= i (role-position role)))
                      (delete-role role sheep)))))
      (delete-reply reply))))

(defun delete-reply (reply)
  (let ((message (reply-message reply)))
    (setf (message-replies message)
          (delete reply (message-replies message)))))

(defun delete-role (role sheep)
  (setf (sheep-direct-roles sheep)
        (delete role (sheep-direct-roles sheep))))

(defun add-reply-to-sheeple (message reply sheeple)
  (loop
     for sheep in sheeple
     for i upto (1- (length sheeple))
     do (let ((role (%make-role
                     :name (message-name message)
                     :position i
                     :reply reply)))
          (push role
                (sheep-direct-roles sheep)))))

(defun undefine-reply (name &key qualifiers participants)
  (let ((msg (find-message name nil)))
    (when msg
      (remove-applicable-reply msg qualifiers participants)
      (clear-memo-table msg)
      t)))

(defun remove-applicable-reply (message qualifiers participants)
  (let ((reply (find-if (lambda (reply)
                          (equal (reply-qualifiers reply)
                                 qualifiers))
                        (%find-applicable-replies
                         message participants :errorp nil))))
    (when reply
      (loop for sheep in participants
         for i from 0
         do (loop for role in (sheep-direct-roles sheep)
               do (let ((role-reply (role-reply role)))
                    (when (and
                           (equal reply role-reply)
                           (= i (role-position role)))
                      (delete-role role sheep)))))
      (delete-reply reply))))

(defun available-replies (sheep)
  (let ((roles (loop for role in (sheep-direct-roles sheep)
                  collect (vector (role-name role) (role-position role)))))
    (remove-duplicates
     (flatten
      (append roles (mapcar 'available-replies (sheep-parents sheep)))))))

(defun add-readers-to-sheep (readers prop-name sheep)
  (loop for reader in readers
     do
       (ensure-message reader :lambda-list '(sheep))
       (ensure-reply reader
                     :lambda-list '(sheep)
                     :participants (list sheep)
                     :body `(property-value sheep ',prop-name)
                     :function (eval (make-reply-lambda reader
                                                        '(sheep)
                                                        `((property-value sheep ',prop-name)))))))

(defun add-writers-to-sheep (writers prop-name sheep)
  (loop for writer in writers
     do
       (ensure-message writer :lambda-list '(new-value sheep))
       (ensure-reply writer
                     :lambda-list '(new-value sheep)
                     :participants (list =t= sheep)
                     :body `(setf (property-value sheep ',prop-name) new-value)
                     :function (eval (make-reply-lambda writer
                                                        '(new-value sheep)
                                                        `((setf (property-value sheep ',prop-name)
                                                                new-value)))))))

;;; macro
(defmacro defreply (&rest args)
  (multiple-value-bind (name qualifiers specialized-lambda-list docstring body)
      (parse-defreply args)
    `(eval-when (:load-toplevel :execute)
       (%defreply-expander ,name ,qualifiers ,specialized-lambda-list ,docstring ,body))))

(defmacro %defreply-expander (name qualifiers specialized-lambda-list docstring body)
  (multiple-value-bind (parameters ll participants required)
      (parse-specialized-lambda-list specialized-lambda-list)
    (declare (ignore parameters required))
    `(ensure-reply
      ',name
      :qualifiers ',qualifiers
      ;; TODO - use the new stuff
      :lambda-list ',ll
      :participants (list ,@participants)
      :documentation ,docstring
      :function ,(make-reply-lambda name ll body)
      :body '(block ,name ,@body))))

(defun make-reply-lambda (name lambda-list body)
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
            (lambda ,ll
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

(defun extract-var-name (item)
  (if (listp item)
      `',(car item)
      `(confirm-var-name ',item)))

(defun confirm-var-name (var-name)
  (if (symbolp var-name)
      var-name
      (error "Invalid var name.")))

(defmacro undefreply (&rest args)
  (multiple-value-bind (name qualifiers lambda-list)
      (parse-undefreply args)
    (multiple-value-bind (iggy1 iggy2 participants iggy3)
        (parse-specialized-lambda-list lambda-list)
      (declare (ignore iggy1 iggy2 iggy3))
      `(undefine-reply
        ',name
        :qualifiers ',qualifiers
        :participants (list ,@participants)))))

(defun parse-undefreply (args)
  (let ((name (car args))
        (qualifiers nil)
        (lambda-list nil))
    (dolist (arg (cdr args))
      (if (and (atom arg) (not (null arg)))
          (push arg qualifiers)
          (setf lambda-list arg)))
    (values name
            qualifiers
            lambda-list)))
