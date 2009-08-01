;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; indent-tabs-mode: nil -*-

;; conditions.lisp
;;
;; Holds all special conditions used by Sheeple
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :sheeple)

(define-condition sheeple-error (error)
  ((format-control :initarg :format-control :reader sheeple-error-format-control)
   (format-args :initarg :format-args :reader sheeple-error-format-args))
  (:report (lambda (condition stream)
             (apply #'format stream
                    (sheeple-error-format-control condition)
                    (sheeple-error-format-args condition)))))

(define-condition sheeple-warning (warning)
  ((format-control :initarg :format-control :reader sheeple-error-format-control)
   (format-args :initarg :format-args :reader sheeple-error-format-args))
  (:report (lambda (condition stream)
             (apply #'format stream
                    (sheeple-error-format-control condition)
                    (sheeple-error-format-args condition)))))

;;; Sheeple
(define-condition sheep-hierarchy-error (sheeple-error) ()
  (:default-initargs :format-control "A circular precedence graph was generated.")
  (:documentation "Signaled whenever there is a problem computing the hierarchy list."))

;;; Properties
(define-condition unbound-direct-property (sheeple-error) ()
  (:default-initargs :format-control "Sheep ~A has no direct property named ~A"))

(define-condition unbound-property (sheeple-error) ()
  (:default-initargs :format-control "Property ~A is unbound for sheep ~A"))

(define-condition property-locked (sheeple-error) ())

;;; Messages
(define-condition no-such-message (sheeple-error) ()
  (:default-initargs :format-control "There is no message named ~A"))

(define-condition clobbering-function-definition (sheeple-warning) ())
(define-condition message-lambda-list-error (sheeple-error) ())

;;; Replies
(define-condition no-applicable-replies (sheeple-error) ())
(define-condition no-most-specific-reply (sheeple-error) ())
(define-condition no-primary-replies (sheeple-error) ())
(define-condition specialized-lambda-list-error (sheeple-error) ())
