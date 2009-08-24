;;;; -*- Mode: lisp; indent-tabs-mode: nil -*-
;;;;
;;;; This file is part of Sheeple.

;;;; tests/setup-tests.lisp
;;;;
;;;; Initial setup for sheeple unit tests.
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :sheeple)

;;; Setting up the :SHEEPLE package to include :5AM stuff
(eval-when (:compile-toplevel :load-toplevel :execute)
  (import '(5am:def-suite 5am:run! 5am:is 5am:in-suite 5am:signals 5am:def-fixture
            5am:for-all 5am:gen-integer 5am:gen-list 5am:gen-tree))
  (export 'run-all-tests))

(defmacro test (name &body body)
  `(5am:test ,name ,@body))

(macrolet ((import-5am-test-macros (&rest names)
             `(progn
                ,@(mapcar #'(lambda (name)
                              `(defmacro ,name (&rest message-args)
                                 `(,(intern (symbol-name ',name) :5am)
                                    ,@message-args)))
                          names))))
  (import-5am-test-macros pass fail skip))

;;; Preparing the test suite
(def-suite sheeple)

(defun run-all-tests ()
  (run! 'sheeple))

(in-suite sheeple)

(test bootstrapped?
 (if *bootstrappedp*
     (pass "Sheeple has bootstrapped")
     (skip "Sheeple has not yet bootstrapped")))

(defmacro postboot-test (name &body body)
  `(test (,name :depends-on bootstrapped?) ,body))

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
               **  (zkat at sykosomatic dot org)  **~%~
               ** or just file a bugreport on github: **~%~
               ** github.com/zkat/sheeple/issues  **~%~
               *****************************************~%"))