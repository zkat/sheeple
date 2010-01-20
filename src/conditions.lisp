;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; indent-tabs-mode: nil -*-

;;;; conditions.lisp
;;;;
;;;; Holds all special conditions used by Sheeple
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :sheeple)

(define-condition sheeple-condition ()
  ((format-control :initarg :format-control :reader sheeple-condition-format-control))
  (:report (lambda (condition stream)
             (format stream (sheeple-condition-format-control condition)))))

(defmacro define-sheeple-condition (name super (&optional string &rest args)
                                    &rest condition-options)
  (let (reader-names)
    `(define-condition ,name ,(ensure-list super)
       ,(loop for arg in args for reader = (intern (format nil "~A-~A" name arg))
           collect
             `(,arg :initarg ,(intern (symbol-name arg) :keyword) :reader ,reader)
           do (push reader reader-names))
       (:report
        (lambda (condition stream)
          (format stream (sheeple-condition-format-control condition)
                  ,@(mapcar #'(lambda (reader) `(,reader condition))
                            (nreverse reader-names)))))
       (:default-initargs :format-control ,string
         ,@(cdr (assoc :default-initargs condition-options)))
       ,@(remove :default-initargs condition-options :key #'car))))

(define-sheeple-condition sheeple-warning (sheeple-condition warning) ())
(define-sheeple-condition sheeple-error (sheeple-condition error) ())
(define-sheeple-condition sheeple-style-warning (style-warning sheeple-warning) ())

;;;
;;; Misc
;;;

(define-sheeple-condition deprecated-feature sheeple-style-warning
  ("This feature has been deprecated since version ~A:~%  ~A" version feature))

(define-sheeple-condition topological-sort-conflict sheeple-error
  ("A conflict arose during a topological sort. There's probably also a bug in
Sheeple, because this condition should always get handled internally.
Current sort status:
  Conflicting elements: ~A
  Sorted elements: ~A
  Conflicting constraints: ~A"
   conflicting-elements sorted-elements constraints))

;;;
;;; Molds
;;;

(define-sheeple-condition mold-error sheeple-error
  ("An error has occured in Sheeple's backend data structures -- this is a bug ~
    in Sheeple itself."))

(define-sheeple-condition mold-collision mold-error
  ("Can't link ~A, because doing so would conflict with the already-linked ~A."
   new-mold collision-mold))

;;;
;;; Objects
;;;

(define-sheeple-condition object-precedence-error sheeple-error
  ("A conflict was encountered while generating a precedence list for ~A.
The conflict information was:~%~A"
   object conflict)
  (:documentation "Signaled whenever there is a problem computing the precedence list."))

;;;
;;; Properties
;;;

(define-sheeple-condition object-property-error sheeple-error ()
  (:documentation "Encompasses all that can go wrong with properties."))

(define-sheeple-condition unbound-property object-property-error
  ("Property ~A is unbound for object ~A" property-name object))

(define-sheeple-condition unbound-direct-property unbound-property
  ("Object ~A has no direct property named ~A" object property-name))

;;;
;;; Messages
;;;

(define-sheeple-condition clobbering-function-definition sheeple-warning
  ("Clobbering regular function or generic function definition for ~A" function))

(define-sheeple-condition sheeple-message-error sheeple-error ()
  (:documentation "Encompasses all that can go wrong with messages."))

(define-sheeple-condition insufficient-message-args sheeple-message-error
  ("Too few arguments were passed to message ~A" message))

(define-sheeple-condition no-such-message sheeple-message-error
  ("There is no message named ~A" message-name))

(define-sheeple-condition message-lambda-list-error sheeple-message-error
  ("~@<Invalid ~S ~_in the message lambda list ~S~:>" arg lambda-list))

;;;
;;; Replies
;;;

(define-sheeple-condition sheeple-reply-error sheeple-message-error ()
  (:documentation "Encompasses all that can go wrong with replies."))

(define-sheeple-condition reply-argument-conflict sheeple-reply-error
  ("The reply ~S~%can't be added to the message ~S~%because ~A"
   reply message reason))

(define-sheeple-condition automatic-message-creation (sheeple-warning style-warning)
  ("Automatically creating message ~A from a DEFREPLY form." message-name))

(define-sheeple-condition reply-lambda-list-conflict sheeple-reply-error
  ("The lambda list ~S conflicts with that of ~S" lambda-list message))

(define-sheeple-condition no-applicable-reply sheeple-reply-error
  ("~@<There is no applicable reply for the message ~2I~_~S~
    ~I~_when called with arguments ~2I~_~S.~:>" message args))

(define-sheeple-condition no-next-reply sheeple-reply-error
  ("~@<There is no next reply for the message ~2I~_~S~I~_when called ~
       from reply ~2I~_~S~I~_with arguments ~2I~_~S.~:>" message reply args))

(define-sheeple-condition no-primary-reply sheeple-reply-error
  ("~@<There is no primary reply for the message ~2I~_~S~
    ~I~_when called with arguments ~2I~_~S.~:>" message args))

(define-condition specialized-lambda-list-error (sheeple-error) ())

;;; For special occasions
(define-sheeple-condition fuck-off sheeple-error ())
