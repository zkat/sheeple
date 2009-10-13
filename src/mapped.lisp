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

(defmacro check-list-type (list typespec &optional string)
  "Calls CHECK-TYPE with each element of LIST, with TYPESPEC and STRING."
  (let ((var (gensym)))
    `(dolist (,var ,list)
       ;; Evaluates STRING multiple times, due to lazyness and spec ambiguity. - Adlai
       (check-type ,var ,typespec ,@(when string `(,string))))))

(defmacro define-print-object (((object class) &key (identity t) (type t)) &body body)
  (let ((stream (gensym)))
    `(defmethod print-object ((,object ,class) ,stream)
      (print-unreadable-object (,object ,stream :type ,type :identity ,identity)
        (let ((*standard-output* ,stream)) ,@body)))))

;; Now for the code

(deftype property-name ()
  "A valid name for an object's property"
  'symbol)

(defstruct (mold (:conc-name   mold-)
                 (:predicate   moldp)
                 (:constructor make-mold)
                 (:copier      copy-mold))
  (parents        nil :read-only t)
  (properties     nil :read-only t)
  (hierarchy-list nil)
  (sub-molds      nil)
  (transitions    nil))

(define-print-object ((mold mold)))

(defstruct (object (:conc-name   %object-)
                   (:predicate   objectp)
                   (:constructor %make-object)
                   (:copier      %copy-object))
  mold property-values roles)

;;; This condition framework is straight from conditions.lisp

(define-condition mold-condition ()
  ((format-control :initarg :format-control :reader mold-condition-format-control))
  (:report (lambda (condition stream)
             (apply #'format stream (mold-condition-format-control condition)))))

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
          (funcall #'format stream (mold-condition-format-control condition)
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

;;; And now for some code:

(defun find-transition (mold property-name)
  "Returns the mold which adds a property named PROPERTY-NAME to MOLD.
If no such mold exists, returns NIL."
  (cdr (assoc property-name (mold-transitions mold) :test 'eq)))

(defvar *maps* (make-hash-table :test 'equal))

(defun tree-find-if (test tree &key (key #'identity))
  (cond ((null tree) nil)
        ((atom tree)
         (when (funcall test (funcall key tree))
           tree))
        (t (or (tree-find-if test (car tree) :key key)
               (tree-find-if test (cdr tree) :key key)))))

(defun find-map (parents properties)
  (tree-find-if (lambda (map) (equal properties (map-properties map)))
                (gethash parents *maps*)))

(defun make-object (parents properties)
  (let ((maybe-map (find-map parents properties)))
    (if (and maybe-map 
             (every #'eq properties (map-properties maybe-map)))
        (%make-object :map maybe-map
                      :property-values (make-array (length (map-properties map))))
        (%make-object :map (make-map :parents parents 
                                     :properties properties)
                      :property-values (make-array (length properties))))))
