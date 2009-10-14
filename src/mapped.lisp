(in-package :cl-user)

;;; some utils copied from utils.lisp

(defun ensure-list (x)
  "X if X is a list, otherwise (list X)."
  (if (listp x) x (list x)))

(defmacro fun (&body body)
  "This macro puts the FUN back in FUNCTION."
  `(lambda (&optional _) (declare (ignorable _)) ,@body))

(defmacro aif (test-form then-form &optional else-form)
  `(let ((it ,test-form))
     (if it ,then-form ,else-form)))

(defmacro awhen (test-form &body body)
  `(aif ,test-form (progn ,@body)))

;;; and some new ones!

(declaim (inline aconsf-helper))
(defun aconsf-helper (alist key value)
  (acons key value alist))

(define-modify-macro aconsf (key value)
  aconsf-helper
  "CONS is to PUSH as ACONS is to ACONSF; it pushes (cons KEY VALUE) to the PLACE.")

(defmacro check-list-type (list typespec &optional (string nil string-supplied-p))
  "Calls CHECK-TYPE with each element of LIST, with TYPESPEC and STRING."
  (let ((var (gensym)))
    `(dolist (,var ,list)
       ;; Evaluates STRING multiple times, due to lazyness and spec ambiguity. - Adlai
       (check-type ,var ,typespec ,@(when string-supplied-p `(,string))))))

(defmacro define-print-object (((object class) &key (identity t) (type t)) &body body)
  (let ((stream (gensym)))
    `(defmethod print-object ((,object ,class) ,stream)
      (print-unreadable-object (,object ,stream :type ,type :identity ,identity)
        (let ((*standard-output* ,stream)) ,@body)))))

;;; This condition framework is straight from conditions.lisp

(define-condition mold-condition ()
  ((format-control :initarg :format-control :reader mold-condition-format-control))
  (:report (lambda (condition stream)
             (format stream (mold-condition-format-control condition)))))

(defmacro define-mold-condition (name super (&optional string &rest args)
                                    &rest condition-options)
  (let (reader-names)
    `(define-condition ,name ,(ensure-list super)
       ,(loop for arg in args for reader = (intern (format nil "~A-~A" name arg))
           collect
             `(,arg :initarg ,(intern (symbol-name arg) :keyword) :reader ,reader)
           do (push reader reader-names))
       (:report
        (lambda (condition stream)
          (format stream (mold-condition-format-control condition)
                  ,@(mapcar #'(lambda (reader) `(,reader condition))
                            (nreverse reader-names)))))
       (:default-initargs :format-control ,string
         ,@(cdr (assoc :default-initargs condition-options)))
       ,@(remove :default-initargs condition-options :key #'car))))

(define-mold-condition mold-warning (mold-condition warning) ())
(define-mold-condition mold-error (mold-condition error) ())

;;; Now for an original condition:

(define-mold-condition mold-collision mold-error
  ("Can't link ~A, because doing so would conflict with the already-linked ~A."
   new-mold collision-mold))
