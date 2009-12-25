;;;; -*- Mode: lisp; indent-tabs-mode: nil -*-
;;;;
;;;; This file is part of Sheeple.

;;;; tests/setup-tests.lisp
;;;;
;;;; Initial setup for sheeple unit tests.
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :sheeple)

;;; This is a bad situation -- the :SHEEPLE package gets cluttered with useless garbage
;;; after running the tests. Maybe tests should happen in a :SHEEPLE-TESTS package, which
;;; automatigally imports all symbols in the :SHEEPLE package?

;;; Setting up the :SHEEPLE package to include :Eos stuff
(eval-when (:compile-toplevel :load-toplevel :execute)
  (import '(Eos:def-suite Eos:run! Eos:is Eos:in-suite Eos:signals))
  (export 'run-all-tests))

(defun gen-vector (&key (length (gen-integer :min 0 :max 10))
                        (elements (gen-integer :min -10 :max 10)))
  (lambda ()
    (apply #'vector
           (funcall (gen-list :length length
                              :elements elements)))))

(defmacro test (name &body body)
  `(Eos:test ,name ,@body))

(macrolet ((import-Eos-test-macros (&rest names)
             `(progn
                ,@(mapcar #'(lambda (name)
                              `(defmacro ,name (&rest message-args)
                                 `(,(intern (symbol-name ',name) :Eos)
                                    ,@message-args)))
                          names))))
  (import-Eos-test-macros pass fail skip))

;;; Preparing the test suite
(def-suite sheeple)

(defun run-all-tests ()
  (run! 'sheeple))

(in-suite sheeple)

;;; Hooking into ASDF
(defmethod asdf:perform ((o asdf:test-op) (c (eql (asdf:find-system :sheeple-tests))))
  (format t "~&~%*******************~%~
                 ** Starting test **~%~
                 *******************~%")
  (run-all-tests)
  (format t "~&*****************************************~%~
               **            Tests finished           **~%~
               *****************************************~%~
               ** If there were any failures on your  **~%~
               ** platform, please report them to me: **~%~
               **  (sykopomp at sykosomatic dot org)  **~%~
               ** or just file a bugreport on github: **~%~
               ** github.com/sykopomp/sheeple/issues  **~%~
               *****************************************~%"))
