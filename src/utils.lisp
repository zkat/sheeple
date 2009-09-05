;;;; -*- Mode: lisp; indent-tabs-mode: nil -*-
;;;;
;;;; This file is part of Sheeple

;;;; utils.lisp
;;;;
;;;; Miscellaneous utilities for Sheeple
;;;;
;;;; TODO:
;;;; * Move conditions in here, or into a new file?
;;;; * DOCUMENTATION!
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :sheeple)

(defun ensure-list (x)
  "X if X is a list, otherwise (list X)."
  (if (listp x) x (list x)))

(defun make-vector (size &key initial-element)
  "Constructs a vector of SIZE elements set to INITIAL-ELEMENT. See `make-list'."
  (make-array size :initial-element initial-element))

;;; This only gets called once, during the macroexpansion of collect.
(defun proper-list-of-length-p (list min &optional (max min))
  "Returns T if the length of X is between MIN and MAX, NIL otherwise."
  (let ((length (list-length list)))
    (when (numberp length)
      (<= min length max))))

(defun collect-normal-expander (n-value fun forms)
    `(progn
       ,@(mapcar (lambda (form) `(setq ,n-value (,fun ,form ,n-value))) forms)
       ,n-value))

(defun collect-list-expander (n-value n-tail forms)
    (let ((n-res (gensym)))
      `(progn
         ,@(mapcar (lambda (form)
                     `(let ((,n-res (cons ,form nil)))
                        (cond (,n-tail
                               (setf (cdr ,n-tail) ,n-res)
                               (setq ,n-tail ,n-res))
                              (t
                               (setq ,n-tail ,n-res  ,n-value ,n-res)))))
                   forms)
         ,n-value)))

(defmacro collect (collections &body body)
  (let ((macros ())
        (binds ()))
    (dolist (spec collections)
      (assert (proper-list-of-length-p spec 1 3) ()
              "Malformed collection specifier: ~S" spec)
      (let* ((name (first spec))
             (default (second spec))
             (kind (or (third spec) 'collect))
             (n-value (gensym (concatenate 'string
                                           (symbol-name name)
                                           "-N-VALUE-"))))
        (push `(,n-value ,default) binds)
        (if (eq kind 'collect)
            (let ((n-tail (gensym (concatenate 'string
                                               (symbol-name name)
                                               "-N-TAIL-"))))
              (if default
                  (push `(,n-tail (last ,n-value)) binds)
                  (push n-tail binds))
              (push `(,name (&rest args)
                            (collect-list-expander ',n-value ',n-tail args))
                    macros))
            (push `(,name (&rest args)
                          (collect-normal-expander ',n-value ',kind args))
                  macros))))
    `(macrolet ,macros (let* ,(nreverse binds) ,@body))))

(defmacro once-only ((&rest names) &body body)
  "Modified from a macro in Practical Common Lisp, by Peter Seibel."
  (let ((gensyms (loop repeat (length names) collect (gensym))))
    `(let (,@(loop for g in gensyms collect `(,g (gensym))))
      `(let (,,@(loop for g in gensyms for n in names collect ``(,,g ,,n)))
        ,(let (,@(loop for n in names for g in gensyms collect `(,n ,g)))
           ,@body)))))

(defun memq (item list)
  "Return tail of LIST beginning with first element EQ to ITEM."
  (do ((i list (cdr i)))
      ((null i))
    (when (eq (car i) item)
      (return i))))

(declaim (inline maybe-weak-pointer-value))
(defun maybe-weak-pointer-value (x)
  (when (weak-pointer-p x)
    (weak-pointer-value x)))

(defmacro aif (test-form then-form &optional else-form)
  `(let ((it ,test-form))
     (if it ,then-form ,else-form)))

(defmacro awhen (test-form &body body)
  `(aif ,test-form
	(progn ,@body)))

(defmacro awhen-prog1 (test-form &body body)
  "A combination of AWHEN and PROG1; always returns the result of TEST-FORM."
  `(aif ,test-form
        (prog1 it ,@body)))

(defmacro aand (&rest args)
  (cond ((null args) t)
	((null (cdr args)) (car args))
	(t `(aif ,(car args) (aand ,@(cdr args))))))

(defmacro fun (&body body)
  "This macro puts the FUN back in FUNCTION."
  (let ((var (intern "_")))
    `(lambda (,var) (declare (ignorable ,var)) ,@body)))

;; from alexandria:
(declaim (inline delete/swapped-arguments))
(defun delete/swapped-arguments (sequence item &rest keyword-arguments)
  (apply #'delete item sequence keyword-arguments))

(define-modify-macro deletef (item &rest remove-keywords)
  delete/swapped-arguments
  "Modify-macro for DELETE. Sets place designated by the first argument to
the result of calling DELETE with ITEM, place, and the REMOVE-KEYWORDS.")

(declaim (inline maphash-keys))
(defun maphash-keys (function table)
  "Like MAPHASH, but calls FUNCTION with each key in the hash table TABLE."
  (maphash (lambda (k v)
             (declare (ignore v))
             (funcall function k))
           table))

(declaim (inline maphash-values))
(defun maphash-values (function table)
  "Like MAPHASH, but calls FUNCTION with each value in the hash table TABLE."
  (maphash (lambda (k v)
             (declare (ignore k))
             (funcall function v))
           table))

(defmacro define-bound-variable (variable &optional docstring)
  "Define a global dynamic variable. If the variable is already bound, the binding
will not be affected; otherwise, it will be bound to a recognizeable and unique value."
  `(defvar ,variable (make-symbol ,(symbol-name variable))
     ,@(when (stringp docstring) (list docstring))))

(defmacro define-bound-variables (&rest variables)
  `(progn ,@(mapcar (fun `(define-bound-variable ,@(ensure-list _))) variables)))
