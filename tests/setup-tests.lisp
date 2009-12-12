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

;;; Setting up the :SHEEPLE package to include :5AM stuff
(eval-when (:compile-toplevel :load-toplevel :execute)
  (import '(5am:def-suite 5am:run! 5am:is 5am:in-suite 5am:signals 5am:def-fixture
            5am:for-all 5am:gen-integer 5am:gen-list 5am:gen-tree))
  (export 'run-all-tests))

(defun gen-vector (&key (length (gen-integer :min 0 :max 10))
                        (elements (gen-integer :min -10 :max 10)))
  (lambda ()
    (apply #'vector
           (funcall (gen-list :length length
                              :elements elements)))))

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
