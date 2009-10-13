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

;; Now for the code

(deftype property-name ()
  "A valid name for an object's property"
  'symbol)

(defstruct (mold (:predicate   moldp)
                 (:constructor make-mold (parents properties)))
  (parents        nil :read-only t)
  (properties     nil :read-only t)
  (hierarchy-list nil)
  (sub-molds      nil)
  (transitions    nil))

(define-print-object ((mold mold)))

(defstruct (object (:conc-name   %object-)
                   (:predicate   objectp)
                   (:constructor %make-object (mold property-values))
                   (:copier      %copy-object))
  mold property-values (roles nil))

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

;;; And now for some code:

(defun find-transition (mold property-name)
  "Returns the mold which adds a property named PROPERTY-NAME to MOLD.
If no such mold exists, returns NIL."
  (cdr (assoc property-name (mold-transitions mold) :test 'eq)))

;;; TODO: ensure-transition-by-property  - Adlai

(defun add-transition-by-property (from-mold property-name to-mold)
  "Adds a link from FROM-MOLD to TO-MOLD, indexed by PROPERTY-NAME.
If a new link was created, FROM-MOLD is returned; otherwise, an error of type
`mold-collision' is signaled."
  (check-type from-mold mold)
  (check-type property-name property-name)
  (check-type to-mold mold)
  (assert (null (set-difference (mold-properties from-mold)
                                (mold-properties to-mold))) ()
          "~A does not contain all the properties of ~A, and is thus not a ~
           valid transition to it." to-mold from-mold)
  (assert (equal (list property-name)
                 (set-difference (mold-properties to-mold)
                                 (mold-properties from-mold)))
          () "~A is not a unique property transition from ~A to ~A."
          property-name from-mold to-mold)
  (awhen (assoc property-name (mold-transitions from-mold))
    (error 'mold-collision :new-mold to-mold :collision-mold (cdr it)))
  (aconsf (mold-transitions from-mold) property-name to-mold)
  from-mold)

(defun find-mold-by-transition (start-mold goal-properties)
  "Searches the transition tree from START-MOLD to find the mold containing
GOAL-PROPERTIES, returning that mold if found, or NIL on failure."
  (check-type start-mold mold)
  (check-list-type goal-properties property-name)
  ;; This algorithm is very concise, but it's not optimal AND it's unclear.
  ;; Probably the first target for cleaning up. - Adlai
  (let ((path (set-difference goal-properties (mold-properties start-mold))))
    (if (null path) start-mold
        (awhen (some (fun (find-transition start-mold _)) path)
          (find-mold-by-transition it path)))))

(defvar *molds* (make-hash-table :test 'equal))

(defun find-mold (parents properties)
  "Searches the mold cache for one with parents PARENTS and properties PROPERTIES,
returning that mold if found, or NIL on failure."
  (check-list-type parents object)
  (check-list-type properties property-name)
  (awhen (gethash parents *molds*)
    (find-mold-by-transition it properties)))

(defun build-mold-transition-between (mold bounds)
  "Returns a linear mold transition tree leading to MOLD. BOUNDS is a strict subset
of MOLD's properties, representing the inclusive upper bound for the new tree."
  (check-type mold mold)
  (check-list-type bounds property-name)
  (assert (and (subsetp bounds (mold-properties mold))
               (not (subsetp (mold-properties mold) bounds)))
          () "~A is not a strict subset of the properties of ~A"
          bounds mold)
  (labels ((build-up-links (mold path)
             (if (null path) mold
                 (let ((new-mold (make-mold (mold-parents mold)
                                            (remove (car path)
                                                    (mold-properties mold)))))
                   (build-up-links (add-transition-by-property new-mold (car path) mold)
                                   (cdr path))))))
    (build-up-links mold (set-difference (mold-properties mold) bounds))))

(defun link-mold (mold)
  "Links MOLD into the mold cache, returning NIL if MOLD was already linked, or MOLD
if it successfully linked MOLD into the cache."
  (check-type mold mold)
  (let ((parents (mold-parents mold)) (props (mold-properties mold)))
    ;; Do we need to create a new tree in the mold cache?
    (aif (find-mold parents nil)
         ;; No; so we find the common base, and link the remaining tree to that.
         (do* ((base it next-base)
               (prop (car props) (car props-left))
               (props-left (cdr props) (cdr props-left))
               (next-base (find-transition base prop)
                          (find-transition next-base prop)))
              ((eq next-base mold))
           (when (null next-base)
             (add-transition-by-property
              base prop (build-mold-transition-between mold (mold-properties base)))))
         ;; Yes; so this builds an entire tree, from no props, to MOLD.
         (prog1 mold
           (setf (gethash parents *molds*)
                 (build-mold-transition-between mold nil))))))

(defun ensure-mold (parents properties)
  (or (find-mold parents properties)
      (link-mold (make-mold parents properties))))

(defun make-object (parents properties)
  (%make-object (ensure-mold parents properties)
                (make-array (length properties))))
