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

(defmacro fun (&body body)
  "This macro puts the FUN back in FUNCTION."
  `(lambda (&optional _) (declare (ignorable _)) ,@body))

(defmacro with-gensyms (names &body body)
  `(let ,(mapcar (fun `(,_ (gensym ,(string _)))) names)
     ,@body))

(defmacro once-only ((&rest names) &body body)
  (let ((gensyms (mapcar (fun (gensym)) names)))
    `(let (,@(loop for g in gensyms collect `(,g (gensym))))
      `(let (,,@(loop for g in gensyms for n in names collect ``(,,g ,,n)))
        ,(let (,@(loop for n in names for g in gensyms collect `(,n ,g)))
           ,@body)))))

(declaim (inline error-when))
(defun error-when (condition error-datum &rest error-args)
  "Like `ASSERT', but with fewer bells and whistles."
  (when condition (apply 'error error-datum error-args)))

(defmacro check-list-type (list typespec &optional (string nil string-supplied-p))
  "Calls CHECK-TYPE with each element of LIST, with TYPESPEC and STRING."
  (let ((var (gensym)))
    `(dolist (,var ,list)
       ;; Evaluates STRING multiple times, due to lazyness and spec ambiguity. - Adlai
       (check-type ,var ,typespec ,@(when string-supplied-p `(,string))))))

(defun ensure-list (x)
  "X if X is a list, otherwise (list X)."
  (if (listp x) x (list x)))

;;; This code is so optimized that the only useful declarations are (safety 0)
;;; and (debug 0), the latter only on SBCL; on CLISP any declarations make no
;;; difference at all, probably because it's all bytecode.
(defun nunzip-alist (alist)
  "Destructively unzips ALIST into two flat lists"
  ;; Once we secure all call sites, (safety 0)? I think so. - Adlai
  (let ((keys alist) (vals (car alist)))
    (do* ((key-cons keys (cdr key-cons))
          (val-cons vals (cdr val-cons)))
         ((null (car key-cons)) (values keys vals))
      (setf (car key-cons) (caar key-cons)
            (car val-cons) (cdr  val-cons)
            (cdr val-cons) (cadr key-cons)))))

(defun parallel-delete (item list-1 list-2)
  "Destructively removes ITEM from both lists, keeping them \"in sync\"
by deleting items at the same position from both lists."
  ;; This implementation could use some speed, but at least it doesn't cons.
  (cond ((null list-1) (values nil nil))
        ((or (eq item (car list-1)) (eq item (car list-2)))
         (parallel-delete item (cdr list-1) (cdr list-2)))
        (T (setf (values (cdr list-1) (cdr list-2))
                 (parallel-delete item (cdr list-1) (cdr list-2)))
           (values list-1 list-2))))

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
      (error-when (not (proper-list-of-length-p spec 1 3))
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

(declaim (inline memq))
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

(declaim (inline aconsf-helper))
(defun aconsf-helper (alist key value)
  (acons key value alist)
  value)

(defmacro aconsf (place key value &environment env)
  "CONS is to PUSH as ACONS is to ACONSF; it pushes (cons KEY VALUE) to the PLACE."
  (multiple-value-bind (temps vals stores set-value get-value)
      (get-setf-expansion place env)
    (unless (null (cdr stores))
      (error "ACONSF can't store to this form: ~:_~S" place))
    (once-only (key value)
      `(let* (,@(mapcar 'list temps vals)
              (,(car stores)
               (acons ,key ,value ,get-value)))
         ,set-value
         ,value))))

(define-modify-macro nconcf (&rest lists)
  nconc
  "Modify-macro for NCONC. Sets place designated by the first argument to
the result of calling NCONC with the place and the LISTS.")

;;; There's a fuller version of this that uses a tail-pointer, but we
;;; only need this simple version here. - Adlai
(defmacro pushend (value place)
  "Destructively adds VALUE as the last element of the list PLACE."
  `(nconcf ,place (list ,value)))

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

(defmacro define-unbound-variables (&rest variables)
  `(progn ,@(mapcar (fun `(defvar ,@(ensure-list _))) variables)))

(defmacro define-print-object (((object class) &key (identity t) (type t)) &body body)
  (with-gensyms (stream)
    `(defmethod print-object ((,object ,class) ,stream)
      (print-unreadable-object (,object ,stream :type ,type :identity ,identity)
        (let ((*standard-output* ,stream)) ,@body)))))
