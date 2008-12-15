;; utils.lisp
;;
;; Miscellaneous utilities for Sheeple
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :sheeple)

(defun pushend (obj list)
  (setf list (nconc list (cons obj nil))))

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

(defun topological-sort (elements constraints tie-breaker)
  (let ((remaining-constraints constraints)
        (remaining-elements elements)
        (result ())) 
    (loop
     (let ((minimal-elements 
            (remove-if
             #'(lambda (sheep)
                 (member sheep  remaining-constraints
                         :key #'cadr))
             remaining-elements)))
       (when (null minimal-elements)
             (if (null remaining-elements)
                 (return-from topological-sort result)
		 (error "Inconsistent precedence graph.")
		 ))
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
