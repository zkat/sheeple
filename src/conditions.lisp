;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; indent-tabs-mode: nil -*-

;; conditions.lisp
;;
;; Holds all special conditions used by Sheeple
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :sheeple)

(define-condition sheeple-condition ()
  ((format-control :initarg :format-control :reader sheeple-condition-format-control))
  (:report (lambda (condition stream)
             (apply #'format stream (sheeple-condition-format-control condition)))))

(defmacro define-sheeple-condition (name super (&optional string &rest args)
                                    &rest condition-options)
  (let (reader-names)
    `(define-condition ,name ,(if (listp super) super (list super))
       ,(loop for arg in args for reader = (intern (format nil "~A-~A" name arg))
           collect
             `(,arg :initarg ,(intern (symbol-name arg) :keyword) :reader ,reader)
           do (push reader reader-names))
       (:report
        (lambda (condition stream)
          (funcall #'format stream (sheeple-condition-format-control condition)
                   ,@(mapcar #'(lambda (reader) `(,reader condition))
                             reader-names))))
       (:default-initargs :format-control ,string
         ,@(cdr (assoc :default-initargs condition-options)))
       ,@(remove :default-initargs condition-options :key #'car))))

(define-sheeple-condition sheeple-warning sheeple-condition ())
(define-sheeple-condition sheeple-error sheeple-condition ())

;;; Sheeple
(define-sheeple-condition sheeple-hierarchy-error sheeple-error
    ("A circular precedence graph was generated for ~A." sheep)
  (:documentation "Signaled whenever there is a problem computing the hierarchy list."))

;;; Properties

(define-sheeple-condition sheeple-property-error sheeple-error ()
  (:documentation "Encompasses all that can go wrong with properties."))

(define-sheeple-condition unbound-direct-property sheeple-property-error
    ("Sheep ~A has no direct property named ~A" sheep property-name))

(define-sheeple-condition unbound-property sheeple-property-error
    ("Property ~A is unbound for sheep ~A" property-name sheep))

;;; Looks like somebody's a long way from home. - Adlai
;;; (define-condition property-locked (sheeple-error) ())

;;; Messages

(define-sheeple-condition clobbering-function-definition sheeple-warning
  ("Clobbering regular function or generit function definition for ~A" function))

(define-sheeple-condition sheeple-message-error sheeple-error ()
  (:documentation "Encompasses all that can go wrong with messages."))

(define-sheeple-condition insufficient-message-args sheeple-message-error
  ("Too few arguments were passed to message ~A" message))

(define-sheeple-condition no-such-message sheeple-message-error
  ("There is no message named ~A" message-name))

(define-sheeple-condition message-lambda-list-error sheeple-message-error
  ("~@<invalid ~S ~_in the message lambda list ~S~:>" arg lambda-list))

;;; Replies

(define-sheeple-condition sheeple-reply-error sheeple-message-error ()
  (:documentation "Encompasses all that can go wrong with replies."))

(define-sheeple-condition reply-lambda-list-conflict sheeple-reply-error
  ("The lambda list ~S conflicts with that of ~S" lambda-list message))

(define-sheeple-condition no-applicable-replies sheeple-reply-error
  ("No applicable replies for message ~A when called with args:~%~S" message args))

;;; Another lonely leftover. - Adlai
;;; (define-condition no-most-specific-reply (sheeple-error) ())

(define-sheeple-condition no-primary-replies sheeple-reply-error
  ("There are no primary replies for message ~A." message))

(define-condition specialized-lambda-list-error (sheeple-error) ())
