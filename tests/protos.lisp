;; This file is part of Sheeple.

;; tests/protos.lisp
;;
;; Unit tests for src/protos.lisp
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :sheeple)

(def-suite protos :in cloning)
(in-suite protos)

(test find-proto ;; (setf find-proto) goes here, too.
  )
(test remove-proto)
(test defproto)
(test spawn-or-reinitialize-sheep)
(test find-proto-transformer)
(test find-proto-reader-macro)