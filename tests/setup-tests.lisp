;;;; -*- Mode: lisp; indent-tabs-mode: nil -*-
;;;;
;;;; This file is part of Sheeple.

;;;; tests/setup-tests.lisp
;;;;
;;;; Initial setup for sheeple unit tests.
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :sheeple)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (import '(5am:def-suite 5am:run! 5am:is 5am:in-suite 5am:signals 5am:def-fixture)))

(defmacro test (name &body body)
  `(5am:test ,name ,@body))

(export 'run-all-tests)

(def-suite sheeple)
(defun run-all-tests ()
  (run! 'sheeple))
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