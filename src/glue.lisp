;; This file is part of Sheeple

;; glue.lisp
;;
;; Extra glue things
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :sheeple)

(defmethod print-object ((sheep sheep) stream)
  (print-sheep sheep stream))