;;;; -*- Mode: lisp; indent-tabs-mode: nil -*-
;;;;
;;;; This file is part of Sheeple

;;;; utils.lisp
;;;;
;;;; Miscellaneous utilities for Sheeple
;;;;
;;;; TODO:
;;;; * Move conditions in here, or into a new file?
;;;; * DOCUMENTATION!
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :sheeple)

;;; This is only here because it gets called once in src/properties.lisp
;;; It gets called to mitigate a hierarchy traversal. Maybe get rid of it?
(defun flatten (x)
  "Flattens a list."
  (labels ((rec (x acc)
             (cond ((null x) acc)
                   ((atom x) (cons x acc))
                   (t (rec (car x) (rec (cdr x) acc))))))
    (rec x nil)))

(defun proper-list-of-length-p (x min &optional (max min))
  (cond ((minusp max) nil)
        ((null x) (zerop min))
        ((consp x)
         (and (plusp max)
              (proper-list-of-length-p (cdr x)
                                       (if (plusp (1- min))
                                           (1- min)
                                           0)
                                       (1- max))))
        (t nil)))

;;; <<<<<<< BEGIN OUTDATED CODE BLOCK >>>>>>>
(defun mapappend (fun &rest args)
  (if (some #'null args)
      ()
      (append (apply fun (mapcar #'car args))
              (apply #'mapappend fun (mapcar #'cdr args)))))

(defun topological-sort-old (elements constraints tie-breaker)
  (let ((remaining-constraints constraints)
        (remaining-elements elements)
        (result ()))
    (loop
       (let ((minimal-elements
              (remove-if
               (lambda (sheep)
                 (member sheep remaining-constraints
                         :key #'cadr))
               remaining-elements)))
         (when (null minimal-elements)
           (if (null remaining-elements)
               (return-from topological-sort-old result)
               (error "Inconsistent precedence graph.")))
         (let ((choice (if (null (cdr minimal-elements))
                           (car minimal-elements)
                           (funcall tie-breaker
                                    minimal-elements
                                    result))))
           (setf result (append result (list choice)))
           (setf remaining-elements
                 (remove choice remaining-elements))
           (setf remaining-constraints
                 (remove choice
                         remaining-constraints
                         :test #'member)))))))
;;; <<<<<<< END OUTDATED CODE BLOCK >>>>>>>

(defun topological-sort (elements constraints tie-breaker)
  "Sorts ELEMENTS such that they satisfy the CONSTRAINTS, falling back
on the TIE-BREAKER in the case of ambiguous constraints. On the assumption
that they are freshly generated, this implementation is destructive with
regards to the CONSTRAINTS. A future version will undo this change."
  (loop
     :for minimal-elements :=
     (remove-if (lambda (sheep)
                  (member sheep constraints
                          :key #'cadr))
                elements)
     :while minimal-elements
     :for choice := (if (null (cdr minimal-elements))
                        (car minimal-elements)
                        (funcall tie-breaker minimal-elements result))
     :collect choice :into result
     :do (setf constraints (delete choice constraints :test #'member)
               elements (remove choice elements))
     :finally
     (if (null elements)
         (return-from topological-sort result)
         (error "Inconsistent precedence graph."))))

(defmacro once-only ((&rest names) &body body)
  "Modified from a macro in Practical Common Lisp, by Peter Seibel."
  (let ((gensyms (loop repeat (length names) collect (gensym))))
    `(let (,@(loop for g in gensyms collect `(,g (gensym))))
      `(let (,,@(loop for g in gensyms for n in names collect ``(,,g ,,n)))
        ,(let (,@(loop for n in names for g in gensyms collect `(,n ,g)))
           ,@body)))))

(defun memq (item list)
  "Return tail of LIST beginning with first element EQ to ITEM."
  (do ((i list (cdr i)))
      ((null i))
    (when (eq (car i) item)
      (return i))))

(defun collect-normal-expander (n-value fun forms)
    `(progn
       ,@(mapcar (lambda (form) `(setq ,n-value (,fun ,form ,n-value))) forms)
       ,n-value))
(defun collect-list-expander (n-value n-tail forms)
    (let ((n-res (gensym)))
      `(progn
         ,@(mapcar (lambda (form)
                     `(let ((,n-res (cons ,form nil)))
                        (cond (,n-tail
                               (setf (cdr ,n-tail) ,n-res)
                               (setq ,n-tail ,n-res))
                              (t
                               (setq ,n-tail ,n-res  ,n-value ,n-res)))))
                   forms)
         ,n-value)))

(defun maybe-weak-pointer-value (x)
  (when (weak-pointer-p x)
    (weak-pointer-value x)))
