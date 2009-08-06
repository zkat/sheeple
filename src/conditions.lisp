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

;;;
;;; Sheeple
;;;
(define-condition unbound-direct-property (sheeple-error)
  ())

(define-condition sheep-hierarchy-error (sheeple-error)
  ((format-control :initform "A circular precedence graph was generated."))
  (:documentation "Signaled whenever there is a problem computing the hierarchy list."))

;;; properties
(define-condition unbound-property (sheeple-error) ())
(define-condition property-locked (sheeple-error) ())

;;;
;;; Messages
;;;
(define-condition no-such-message (sheeple-error) ())
(define-condition clobbering-function-definition (sheeple-warning) ())
(define-condition message-lambda-list-error (sheeple-error) ())

;;;
;;; Messages
;;;
(define-condition no-applicable-replies (sheeple-error) ())
(define-condition no-most-specific-reply (sheeple-error) ())
(define-condition no-primary-replies (sheeple-error) ())
(define-condition specialized-lambda-list-error (sheeple-error) ())
