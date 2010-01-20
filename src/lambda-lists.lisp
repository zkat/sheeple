;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; indent-tabs-mode: nil -*-

;;;; This file is part of Sheeple
;;;;
;;;; This software is derived from the SBCL system. See COPYING for more information
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING for
;;;; more information.

(in-package :sheeple)

(defun allow-other-keys (lambda-list)
  (declare (optimize speed) (list lambda-list))
  (if (aand (memq '&key lambda-list)
            (not (memq '&allow-other-keys it)))
      (aprog1 (cons (car lambda-list) (cdr lambda-list))
        (do ((tail it (cdr tail))
             (next (cdr it) (cdr next)))
            ((null next) (setf (cdr tail) '(&allow-other-keys)))
          (declare (list next) (cons tail))
          (if (eq (car next) '&aux)
              (return (setf (cdr tail) (cons '&allow-other-keys (cdr tail))))
              (setf (cdr tail) (cons (car next) (cdr next))))))
      lambda-list))

;;; Break something like a lambda list (but not necessarily actually a
;;; lambda list, e.g. the representation of argument types which is
;;; used within an FTYPE specification) into its component parts. We
;;; return twelve values:
;;;  1. a list of the required args;
;;;  2. a list of the &OPTIONAL arg specs;
;;;  3. true if a &REST arg was specified;
;;;  4. the &REST arg;
;;;  5. true if &KEY args are present;
;;;  6. a list of the &KEY arg specs;
;;;  7. true if &ALLOW-OTHER-KEYS was specified.;
;;;  8. true if any &AUX is present (new in SBCL vs. CMU CL);
;;;  9. a list of the &AUX specifiers;
;;; 10. true if any lambda list keyword is present (only for
;;;     PARSE-LAMBDA-LIST-LIKE-THING).
;;;
;;; The top level lambda list syntax is checked for validity, but the
;;; arg specifiers are just passed through untouched. If something is
;;; wrong, we use COMPILER-ERROR, aborting compilation to the last
;;; recovery point.

;; (declaim (ftype (sfunction (list &key (:silent boolean))
;;                            (values list list boolean t boolean list boolean
;;                                    boolean list boolean t t boolean))
;;                 parse-lambda-list-like-thing))
;; (declaim (ftype (sfunction (list)
;;                            (values list list boolean t boolean list boolean
;;                                    boolean list boolean t t))
;;                 parse-lambda-list))

(defun parse-lambda-list-like-thing (list &key silent)
  (collect (required optional keys aux)
    (let ((restp nil)
          (rest nil)
          (keyp nil)
          (auxp nil)
          (allowp nil)
          (state :required))
      (declare (type (member :allow-other-keys :aux
                             :key :required :rest
                             :optional :post-rest)
                     state))
      (dolist (arg list)
        (if (member arg lambda-list-keywords)
            (case arg
              (&optional
               (unless (eq state :required)
                 (error "misplaced &OPTIONAL in lambda list: ~S"
                        list))
               (setq state :optional))
              (&rest
               (unless (member state '(:required :optional))
                 (error "misplaced &REST in lambda list: ~S" list))
               (setq state :rest))
              (&key
               (unless (member state
                               '(:required :optional :post-rest :post-more))
                 (error "misplaced &KEY in lambda list: ~S" list))
               (when (optional)
                 (unless silent
                   (warn
                    "&OPTIONAL and &KEY found in the same lambda list: ~S" list)))
               (setq keyp t
                     state :key))
              (&allow-other-keys
               (unless (eq state ':key)
                 (error "misplaced &ALLOW-OTHER-KEYS in ~
                                  lambda list: ~S"
                        list))
               (setq allowp t
                     state :allow-other-keys))
              (&aux
               (when (member state '(:rest :more-context :more-count))
                 (error "misplaced &AUX in lambda list: ~S" list))
               (when auxp
                 (error "multiple &AUX in lambda list: ~S" list))
               (setq auxp t
                     state :aux))
              (t
               ;; It could be argued that &WHOLE and friends would be
               ;; just ordinary variables in an ordinary lambda-list,
               ;; but since (1) that seem exceedingly to have been the
               ;; programmers intent and (2) the spec can be
               ;; interpreted as giving as licence to signal an
               ;; error[*] that is what we do.
               ;;
               ;; [* All lambda list keywords used in the
               ;; implementation appear in LAMBDA-LIST-KEYWORDS. Each
               ;; member of a lambda list is either a parameter
               ;; specifier ot a lambda list keyword. Ergo, symbols
               ;; appearing in LAMBDA-LIST-KEYWORDS cannot be
               ;; parameter specifiers.]
               (error 'simple-error
                      :format-control "Bad lambda list keyword ~S in: ~S"
                      :format-args (list arg list))))
            (progn
              (when (symbolp arg)
                (let ((name (symbol-name arg)))
                  (when (and (plusp (length name))
                             (char= (char name 0) #\&))
                    (unless silent
                      (warn
                       "suspicious variable in lambda list: ~S." arg)))))
              (case state
                (:required (required arg))
                (:optional (optional arg))
                (:rest
                 (setq restp t
                       rest arg
                       state :post-rest))
                (:key (keys arg))
                (:aux (aux arg))
                (t
                 (error "found garbage in lambda list when expecting ~
                                  a keyword: ~S"
                        arg))))))
      (when (eq state :rest)
        (error "&REST without rest variable"))
      (values (required) (optional) restp rest keyp (keys) allowp auxp (aux)
              (not (eq state :required))))))

;;; like PARSE-LAMBDA-LIST-LIKE-THING, except our LAMBDA-LIST argument
;;; really *is* a lambda list, not just a "lambda-list-like thing", so
;;; can barf on things which're illegal as arguments in lambda lists
;;; even if they could conceivably be legal in not-quite-a-lambda-list
;;; weirdosities

(defun parse-lambda-list (lambda-list)
  ;; Classify parameters without checking their validity individually.
  (multiple-value-bind (required optional restp rest keyp keys allowp auxp aux)
      (parse-lambda-list-like-thing lambda-list)
    ;; Check validity of parameters.
    (flet ((need-symbol (x why)
             (unless (symbolp x)
               (error "~A is not a symbol: ~S" why x))))
      (dolist (i required)
        (need-symbol i "Required argument"))
      (dolist (i optional)
        (typecase i
          (symbol)
          (cons
           (destructuring-bind (var &optional init-form supplied-p) i
             (declare (ignore init-form supplied-p))
             (need-symbol var "&OPTIONAL parameter name")))
          (t
           (error "&OPTIONAL parameter is not a symbol or cons: ~S"
                  i))))
      (when restp
        (need-symbol rest "&REST argument"))
      (when keyp
        (dolist (i keys)
          (typecase i
            (symbol)
            (cons
             (destructuring-bind (var-or-kv &optional init-form supplied-p) i
               (declare (ignore init-form supplied-p))
               (if (consp var-or-kv)
                   (destructuring-bind (keyword-name var) var-or-kv
                     (declare (ignore keyword-name))
                     (need-symbol var "&KEY parameter name"))
                   (need-symbol var-or-kv "&KEY parameter name"))))
            (t
             (error "&KEY parameter is not a symbol or cons: ~S"
                    i))))))
    ;; Voila.
    (values required optional restp rest keyp keys allowp auxp aux)))

(defun count-required-parameters (lambda-list)
  ;; Count the required parameters, without consing or safety checks
  ;; This is for reply dispatch, and should be given the axe once an even
  ;; marginally better dispatch scheme comes along.
  (declare (optimize speed (safety 0)))
  (do* ((n   0           (1+ n))
        (ll  lambda-list (cdr ll))
        (arg (car ll)    (car ll)))
       ((or (null ll) (memq arg '(&optional &rest &key &aux))) n)
    (declare (fixnum n))))

(defun parse-specialized-lambda-list
    (arglist
     &optional supplied-keywords (allowed-keywords '(&optional &rest &key &aux))
     &aux (specialized-lambda-list-keywords
           '(&optional &rest &key &allow-other-keys &aux)))
  (let ((arg (car arglist)))
    (cond ((null arglist) (values nil nil nil nil))
          ((eq arg '&aux)
           (values nil arglist nil nil nil))
          ((memq arg lambda-list-keywords)
           ;; non-standard lambda-list-keywords are errors.
           (unless (memq arg specialized-lambda-list-keywords)
             (error 'specialized-lambda-list-error
                    :format-control "unknown specialized-lambda-list ~
                                     keyword ~S~%"
                    :format-args (list arg)))
           ;; no multiple &rest x &rest bla specifying
           (when (memq arg supplied-keywords)
             (error 'specialized-lambda-list-error
                    :format-control "multiple occurrence of ~
                                     specialized-lambda-list keyword ~S~%"
                    :format-args (list arg)))
           ;; And no placing &key in front of &optional, either.
           (unless (memq arg allowed-keywords)
             (error 'specialized-lambda-list-error
                    :format-control "misplaced specialized-lambda-list ~
                                     keyword ~S~%"
                    :format-args (list arg)))
           ;; When we are at a lambda-list keyword, the parameters
           ;; don't include the lambda-list keyword; the lambda-list
           ;; does include the lambda-list keyword; and no
           ;; specializers are allowed to follow the lambda-list
           ;; keywords (at least for now).
           (multiple-value-bind (parameters lambda-list)
               (parse-specialized-lambda-list (cdr arglist)
                                              (cons arg supplied-keywords)
                                              (if (eq arg '&key)
                                                  (cons '&allow-other-keys
                                                        (cdr (member arg allowed-keywords)))
                                                  (cdr (member arg allowed-keywords))))
             (when (and (eq arg '&rest)
                        (or (null lambda-list)
                            (memq (car lambda-list)
                                  specialized-lambda-list-keywords)
                            (not (or (null (cadr lambda-list))
                                     (memq (cadr lambda-list)
                                           specialized-lambda-list-keywords)))))
               (error 'specialized-lambda-list-error
                      :format-control
                      "in a specialized-lambda-list, excactly one ~
                       variable must follow &REST.~%"
                      :format-args nil))
             (values parameters
                     (cons arg lambda-list)
                     ()
                     ()
                     ())))
          (supplied-keywords
           ;; After a lambda-list keyword there can be no specializers.
           (multiple-value-bind (parameters lambda-list)
               (parse-specialized-lambda-list (cdr arglist)
                                              supplied-keywords
                                              allowed-keywords)
             (values (cons (if (listp arg) (car arg) arg) parameters)
                     (cons arg lambda-list)
                     ()
                     ()
                     ())))
          (t
           (multiple-value-bind (parameters lambda-list specializers required ignorable)
               (parse-specialized-lambda-list (cdr arglist))
             (let ((symbol (if (listp arg) (car arg) arg)))
               (unless (symbolp symbol)
                 (error "Specializer argument is not a symbol: ~S" symbol))
               (values (cons symbol parameters)
                       (cons symbol lambda-list)
                       (cons (if (listp arg) (cadr arg) '=t=) specializers)
                       (cons symbol required)
                       (if (listp arg) (cons (car arg) ignorable) ignorable))))))))

(defun check-message-lambda-list (lambda-list)
  (flet ((check-no-defaults (list)
           (awhen (find-if (complement (rcurry 'typep '(or symbol (cons * null)))) list)
             (error 'message-lambda-list-error :arg it :lambda-list lambda-list))))
    (multiple-value-bind (required optional restp rest keyp keys aok auxp)
        (parse-lambda-list lambda-list)
      (declare (ignore required restp rest keyp aok))
      (check-no-defaults optional)
      (check-no-defaults keys)
      (when auxp (error 'message-lambda-list-error :arg '&aux :lambda-list lambda-list)))))

(defun create-msg-lambda-list (lambda-list)
  "Create a message lambda list from a reply's lambda list"
  (loop for x in lambda-list
     collect (if (consp x) (car x) x)
     if (eq x '&key) do (loop-finish)))

(defun analyze-lambda-list (lambda-list)
  ;; Need to specify exactly what this function does -- is it analyzing
  ;; any lambda-list, or defmessage lambda-lists? Should it do error
  ;; reporting, or no error reporting? Right now it's not clear what
  ;; the answers are...
  (labels ((parse-key-arg (arg)
             (flet ((make-keyword (symbol)
                      (intern (symbol-name symbol)
                              (find-package 'keyword))))
               (cond ((not (listp arg)) (make-keyword arg))
                     ((listp (car arg)) (caar arg))
                     (t (make-keyword (car arg)))))))
    (let ((nrequired 0) (noptional 0) (keysp nil) (restp nil) (nrest 0)
          (allow-other-keys-p nil) (keywords nil) (keyword-parameters nil)
          (state 'required))
      (dolist (x lambda-list)
        (if (memq x lambda-list-keywords)
            (case x
              (&optional         (setf state 'optional))
              (&rest             (setf state 'rest restp t))
              (&key              (setf state 'key  keysp t))
              (&allow-other-keys (setf allow-other-keys-p t))
              (&aux              (return))
              (otherwise
               (error "encountered the non-standard lambda list keyword ~S" x)))
            (ecase state
              (required  (incf nrequired))
              (optional  (incf noptional))
              (key       (push (parse-key-arg x) keywords)
                         (push x keyword-parameters))
              (rest      (incf nrest)))))
      (when (and restp (zerop nrest))
        (error "A &REST keyword in a lambda-list ~
                must be followed by at least one variable."))
      (values nrequired noptional keysp restp allow-other-keys-p
              (reverse keywords) (reverse keyword-parameters)))))
