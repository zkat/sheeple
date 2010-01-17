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

;;; From Alexandria
(deftype string-designator ()
  "A string designator is either a string, a symbol, or a character."
  `(or symbol string character))

(defmacro fun (&body body)
  "This macro puts the FUN back in FUNCTION."
  `(lambda (&optional _) (declare (ignorable _)) ,@body))

;;; Adapted from Alexandria
(defmacro with-gensyms (names &body forms)
  "Binds each variable named by a symbol in NAMES to a unique symbol around
FORMS. Each of NAMES must either be either a symbol, or of the form:

 (symbol string-designator)

Bare symbols appearing in NAMES are equivalent to:

 (symbol symbol)

The string-designator is used as the argument to GENSYM when constructing the
unique symbol the named variable will be bound to."
  `(let ,(mapcar (fun (multiple-value-bind (symbol string)
                          (etypecase _
                            (symbol
                             (values _ (symbol-name _)))
                            ((cons symbol (cons string-designator null))
                             (values (car _) (string (cadr _)))))
                        `(,symbol (gensym ,string))))
                 names)
     ,@forms))

;;; Heavily adapted from Alexandria
(defmacro once-only (specs &body forms)
  "Each SPEC must be either a NAME, or a (NAME INITFORM), with plain
NAME using the named variable as initform.

Evaluates FORMS with names rebound to temporary variables, ensuring
that each is evaluated only once.

Example:
  (defmacro cons1 (x) (once-only (x) `(cons ,x ,x)))
  (let ((y 0)) (cons1 (incf y))) => (1 . 1)"
  (let ((gensyms (mapcar (fun (gensym "ONCE-ONLY")) specs))
        (real-specs (mapcar (fun (etypecase _
                                   (list (cons (car _) (cadr _)))
                                   (symbol (cons _ _))))
                            specs)))
    (flet ((mapcar-gars (thunction) (mapcar thunction gensyms real-specs)))
      `(let ,(mapcar-gars (lambda (g n) `(,g (gensym ,(string (car n))))))
         `(let (,,@(mapcar-gars (lambda (g n) ``(,,g ,,(cdr n)))))
            ,(let ,(mapcar-gars (lambda (g n) `(,(car n) ,g)))
                  ,@forms))))))

;;; Lightly adapted from Parse-Declarations
(defun parse-body (body &key documentation whole)
  "Parses BODY into (values remaining-forms declarations doc-string).
Documentation strings are recognized only if DOCUMENTATION is true.
Syntax errors in body are signalled and WHOLE is used in the signal
arguments when given."
  (flet ((starts-with (thing list)
	   (and (consp list) (eq (first list) thing))))
    (prog (current decls doc)
     :scan-body
       (setf current (car body))
       (cond ((and documentation (stringp current)) (go :scan-string))
	     ((starts-with 'declare current)
              (push (pop body) decls)               (go :scan-body))
	     (t (go :finish)))
     :scan-string
       (cond ((null (cdr body))                     (go :finish))
	     (doc (error "Too many documentation strings in ~S." (or whole body)))
	     (t (setf doc (pop body))               (go :scan-body)))
     :finish (return (values body (nreverse decls) doc)))))

;;; Also adapted from Alexandria
(defun symbolicate (&rest things &aux (index 0))
  "Concatenate together the names of some strings and symbols,
producing a symbol in the current package."
  (let ((name (make-string (reduce #'+ things :key (compose 'length 'string)))))
    (dolist (thing things (values (intern name)))
      (let ((x (string thing)))
        (replace name x :start1 index)
        (incf index (length x))))))

(defmacro error-when (condition error-datum &rest error-args)
  "Like `ASSERT', but with fewer bells and whistles."
  `(when ,condition (error ',error-datum ,@error-args)))

(defmacro check-list-type (list typespec &optional (string nil string-supplied-p))
  "Calls CHECK-TYPE with each element of LIST, with TYPESPEC and STRING."
  (with-gensyms (var)
    (symbol-macrolet
        ((check `(dolist (,var ,list) (check-type ,var ,typespec ,string))))
      (if (not string-supplied-p) check
          (once-only (string) check)))))

(defun ensure-list (x)
  "X if X is a list, otherwise (list X)."
  (if (listp x) x (list x)))

(defun plist-to-wide-alist (plist)
  "Builds a fresh alist corresponding to the elements of PLIST. The alist
is \"wide\", ie, each pair is (key value) rather than (key . value)"
  ;; I could go nuts with DO, like I do below... or I could LOOP   - Adlai
  (loop for (key value) on plist by #'cddr collect (list key value)))

(defun nunzip-alist (alist)
  "Destructively unzips ALIST into two flat lists"
  (declare (list alist) (optimize speed (safety 0)))
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

(defmacro collect (collections &body body)
  (let (macros binds)
    (dolist (spec collections)
      (destructuring-bind (name &optional default (kind 'collect))
          (ensure-list spec)
        (let ((value (gensym (format nil "~A-VALUE-" name))))
          (push (if (null default) value `(,value ,default)) binds)
          (macrolet ((collect-macro (fun-form)
                       `(push `(,name (&rest forms)
                                      `(progn ,@(mapcar (fun ,,fun-form) forms) ,',value))
                              macros)))
            (if (eq kind 'collect)
                (let ((tail (gensym (format nil "~A-TAIL-" name))))
                  (if (null default) (push tail binds)
                      (push `(,tail (last ,value)) binds))
                  (collect-macro `(with-gensyms (n-res)
                                    `(let ((,n-res (list ,_)))
                                       (cond ((null ,',tail)
                                              (setf ,',tail  ,n-res ,',value ,n-res))
                                             (t (setf (cdr ,',tail) ,n-res ,',tail ,n-res)))))))
                (collect-macro ``(setf ,',value (,',kind ,',value ,_))))))))
    `(let* ,(nreverse binds) (macrolet ,macros ,@body))))

(declaim (inline memq))
(defun memq (item list)
  "Return tail of LIST beginning with first element EQ to ITEM."
  (do ((i list (cdr i)))
      ((null i))
    (declare (list i))
    (when (eq (car i) item)
      (return i))))

(defmacro aif (test-form then-form &optional else-form)
  `(let ((it ,test-form))
     (if it ,then-form ,else-form)))

(defmacro awhen (test-form &body body)
  `(aif ,test-form
        (progn ,@body)))

(defmacro aprog1 (valform &body body)
  `(let ((it ,valform)) ,@body it))

(defmacro awhen-prog1 (test-form &body body)
  "A combination of AWHEN and PROG1; always returns the result of TEST-FORM."
  `(awhen ,test-form (prog1 it ,@body)))

(defmacro aand (&rest args)
  (cond ((null args) t)
        ((null (cdr args)) (car args))
        (t `(aif ,(car args) (aand ,@(cdr args))))))

;;; from anaphora
(defmacro anaphoric (op test &body body)
  `(let ((it ,test))
     (,op it ,@body)))

(defmacro acond (&body clauses)
  "Like COND, except result of each test-form is bound to IT (via LET) for the
scope of the corresponding clause."
  (labels ((rec (clauses)
             (if clauses
                 (destructuring-bind ((test &body body) . rest)  clauses
                   (if body
                       `(anaphoric if ,test (progn ,@body) ,(rec rest))
                       `(anaphoric if ,test it ,(rec rest))))
                 nil)))
    (rec clauses)))

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

(defmacro do-reversed ((name listform) &body body)
  (with-gensyms (label tail)
    `(labels ((,label (,tail &rest ,name)
                (declare (dynamic-extent ,name))
                (if (endp ,tail)
                    (progn ,@body)
                    (apply #',label (cdr ,tail) (car ,tail) ,name))))
       (declare (dynamic-extent #',label))
       (,label ,listform))))

(defmacro feature-case (default &body ports)
  "Expands to the first form in PORTS. If there are no PORTS, the DEFAULT is used.

For example, to exit a few different lisps with a reasonable default, do:

  (feature-case
      (break \"Program Terminated\")
    #+ccl  (ccl:quit)
    #+sbcl (sb-ext:quit)
    #+ecl  (si:quit)"
  (or (car ports) default))
