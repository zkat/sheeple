;;;; -*- Mode: lisp; indent-tabs-mode: nil -*-
;;;;
;;;; This file is part of Sheeple

;;;; utils.lisp
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :sheeple)

(def-suite utils :in sheeple)

(test pushend)
(test flatten)
(test proper-list-of-length-p)

(test topological-sort)
(test once-only)
(test memq)
(test collect-normal-expander)
(test collect-list-expander)
(test maybe-weak-pointer-value)
