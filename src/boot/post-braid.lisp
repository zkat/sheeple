;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; indent-tabs-mode: nil -*-

;;;; This file is part of Sheeple

;;;; boot/post-braid.lisp
;;;;
;;;; Stuff that needs to happen right after the initial bootstrap is done.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :sheeple)

;; Now that we can create and initialize propds and all that jazz, we can actually
;; start setting properties.
(macrolet ((set-name (name) `(setf (property-value ,name 'nickname) ',name)))
  (set-name =t=)
  (set-name =standard-object=)
  (set-name =standard-metaobject=))