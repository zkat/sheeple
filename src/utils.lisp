;;;; -*- Mode: lisp; indent-tabs-mode: nil -*-
;;;
;; This file is part of Sheeple

;; utils.lisp
;;
;; Miscellaneous utilities for Sheeple
;;
;; TODO:
;; * Move conditions in here, or into a new file?
;; * DOCUMENTATION!
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :sheeple)

(defmacro pushend (obj list)
  `(setf ,list (nconc ,list (cons ,obj nil))))

(defun mapappend (fun &rest args)
  (if (some #'null args)
      ()
      (append (apply fun (mapcar #'car args))
              (apply #'mapappend fun (mapcar #'cdr args)))))

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
#+ (and)
(defun topological-sort (elements constraints tie-breaker)
  (loop
     :with remaining-constraints := constraints
     :and remaining-elements := elements and result
     :for minimal-elements :=
       (remove-if (lambda (sheep)
                    (member sheep remaining-constraints
                            :key #'cadr))
                  remaining-elements)
     :when (null minimal-elements) :do
       (if (null remaining-elements)
           (return-from topological-sort result)
           (error "Inconsistent precedence graph."))
     :do
       (let ((choice (if (null (cdr minimal-elements))
                         (car minimal-elements)
                         (funcall tie-breaker minimal-elements result))))
         (setf result (append result (list choice)))
         (setf remaining-elements
               (remove choice remaining-elements))
         (setf remaining-constraints
               (remove choice
                       remaining-constraints
                       :test #'member)))))

#+ (or)
(defun topological-sort (elements constraints tie-breaker)
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
               (return-from topological-sort result)
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
