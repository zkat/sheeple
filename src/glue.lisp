;; This file is part of Sheeple

;; glue.lisp
;;
;; Extra things that need to be run once sheeple itself is loaded.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :sheeple)

(defmethod print-object :around (sheep stream)
  ;; I have to put this here because trying to lump the compilation of print-object
  ;; in the same file as the print-sheep definition was giving me shit.
  (if (sheep-p sheep)
      (print-sheep sheep stream)
      (call-next-method)))