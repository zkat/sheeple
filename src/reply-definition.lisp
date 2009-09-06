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
(defstruct (reply (:predicate replyp))
  ;; Replies are the Sheeple equivalent of methods. Replies themselves are objects that hold
  ;; some basic information about what the reply does, what kind of reply it is, etc.
  ;; When reply objects are 'called', their reply-function is fetched directly. By using lambdas,
  ;; we're able to latch on to the lexical environment the reply was defined in (so they can be
  ;; closures)
  message qualifiers lambda-list
  (function (lambda () nil)))

(defun reply-name (reply)
  (message-name (reply-message reply)))

;;;
;;; Roles
;;;
;;; - Roles encapsulate the idea of dispatch. Roles live in sheep objects themselves and represent
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

(defun role-message (role)
  (reply-message (role-reply role)))
(defun role-name (role)
  (reply-name (role-reply role)))

(defun participantp (sheep reply)
  "Checks if SHEEP is actually involved in dispatching REPLY"
  (when (find-if (curry 'eq reply)
                 (%sheep-roles sheep) :key 'role-reply)
    t))

;;;
;;; Reply definition
;;;
(defun ensure-reply (name &key qualifiers lambda-list participants function (documentation ""))
  ;; shouldn't this just be a plain call to ensure-message? -- syko
  (let ((message (find-message name nil)))
    (when (null message)
      ;; TODO - this style warning could be -much- nicer.
      (warn 'style-warning )
      (setf message (ensure-message name :lambda-list (create-msg-lambda-list lambda-list))))
    (let ((reply (make-reply :message (find-message name)
                             :lambda-list lambda-list
                             :qualifiers qualifiers
                             :function function))
          (sheepified-participants (sheepify-list participants)))
      (setf (documentation reply 't) documentation) ; same as dox for CLOS methods
      (clear-dispatch-cache message) ; because the dispatch landscape has changed
      (remove-specific-reply message qualifiers sheepified-participants)
      (add-reply-to-message reply message)
      (add-reply-to-sheeple message reply sheepified-participants)
      reply)))

(defun create-msg-lambda-list (lambda-list)
  "Create a message lambda list from a reply lambda list"
  (loop for x in lambda-list
     collect (if (consp x) (list (car x)) x)
     if (eq x '&key) do (loop-finish)))

(defun add-reply-to-message (reply message)
  (set-arg-info message :new-reply reply)
  (push reply (message-replies message)))

(defun add-reply-to-sheeple (message reply sheeple)
  (declare (ignore message)) ; Am I missing something?
  (loop
     for sheep in sheeple
     for i below (length sheeple)
     do (push (make-role reply i)
              (%sheep-roles sheep))))

(defun available-replies (sheep)
  (delete-duplicates
   (append (%sheep-roles sheep) (mapcan 'available-replies (sheep-parents sheep)))
   :test 'equal))

(defun add-reader-to-sheep (reader prop-name sheep)
  (ensure-message reader :lambda-list '(sheep))
  (ensure-reply reader
                :lambda-list '(sheep)
                :participants (list sheep)
                :function (eval (make-reply-lambda reader
                                                   '(sheep)
                                                   `((property-value sheep ',prop-name))))))

(defun add-readers-to-sheep (readers prop-name sheep)
  (map nil (fun (add-reader-to-sheep _ prop-name sheep)) readers)
  sheep)

(defun add-writer-to-sheep (writer prop-name sheep)
  (ensure-message writer :lambda-list '(new-value sheep))
  (ensure-reply writer
                :lambda-list '(new-value sheep)
                :participants (list =t= sheep)
                :function (eval (make-reply-lambda writer
                                                   '(new-value sheep)
                                                   `((setf (property-value sheep ',prop-name)
                                                           new-value))))))

(defun add-writers-to-sheep (writers prop-name sheep)
  (map nil (fun (add-writer-to-sheep _ prop-name sheep)) writers)
  sheep)

;;;
;;; Reply undefinition
;;;
(defun undefine-reply (name &key qualifiers participants)
  (awhen (find-message name nil)
    ;; TODO -- Remove documentation - Adlai
    (remove-applicable-reply it qualifiers participants)
    (clear-dispatch-cache it)
    t))

(defun remove-specific-reply (message qualifiers participants)
  (let ((reply (find-if (fun (equal qualifiers (reply-qualifiers _)))
                        (%find-applicable-replies ;defined in reply-dispatch.lisp
                         message participants :errorp nil))))
    (when (and reply
               (every (rcurry 'participantp (reply-name reply)) participants))
      (loop
         for sheep in participants
         for i from 0
         do (map nil (fun (when (and (eq reply (role-reply _))
                                     (= i (role-position _)))
                            (delete-role _ sheep)))
                 (%sheep-roles sheep)))
      (delete-reply reply))))

(defun remove-applicable-reply (message qualifiers participants)
  (let ((reply (find-if (fun (equal qualifiers (reply-qualifiers _)))
                        (%find-applicable-replies
                         message participants :errorp nil))))
    (when reply
      (loop
         for sheep in participants
         for i from 0
         do (map nil (fun (when (and (eq reply (role-reply _))
                                     (= i (role-position _)))
                            (delete-role _ sheep)))
                 (%sheep-roles sheep)))
      (delete-reply reply))))

(defun delete-reply (reply)
  (let ((message (reply-message reply)))
    (setf (message-replies message)
          (delete reply (message-replies message)))))

(defun delete-role (role sheep)
  (setf (%sheep-roles sheep)
        (delete role (%sheep-roles sheep))))

;;;
;;; User interface
;;;

;;; Definition
(defmacro defreply (&rest args)
  (multiple-value-bind (name qualifiers specialized-lambda-list docstring body)
      (parse-defreply args)
    `(%defreply-expander ,name ,qualifiers ,specialized-lambda-list ,docstring ,body)))

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
      :function ,(make-reply-lambda name ll body))))

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

;;; Undefinition
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
