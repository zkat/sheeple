;;;; protos.lisp
;;;;
;;;; Contains facilities to define proto sheep
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :sheeple)

(let ((proto-table (make-hash-table :test #'eq)))
  
  (defun proto (name &optional (errorp t))
    (multiple-value-bind (proto hasp)
        (gethash name proto-table)
      (if hasp
          proto
          (if errorp
              (error "No such prototype: ~A" name)
              nil))))

  (defun (setf proto) (new-value name)
    (unless (sheep-p new-value)
      (error "~A is not a sheep object." new-value))
    (setf (gethash name proto-table) new-value))
  
  (defun remove-proto (name)
    (remhash name proto-table))

  ) ; end prototype table

(defmacro defproto (name sheeple properties &rest options)
  `(let ((sheep (spawn-or-reinit-sheep
		 (proto ',name nil) 
		 ,(canonize-sheeple sheeple)
		 :properties ,(canonize-properties properties t)
		 ,@(canonize-clone-options options))))
     (unless (sheep-nickname sheep)
       (setf (sheep-nickname sheep) ',name))
     (setf (proto ',name) sheep)))

(defun spawn-or-reinit-sheep (maybe-sheep parents &rest options)
  (if maybe-sheep
      (apply #'reinit-sheep maybe-sheep :new-parents parents options)
      (apply #'spawn-sheep parents options)))

;; ;; This reader macro lets us do #@foo instead of having to do (proto 'foo).
;; ;; It's needed enough that it's useful to have this around.
;; (defun proto-transformer (stream subchar arg)
;;   (declare (ignore subchar arg))
;;   `(proto ',(read stream t)))

;; (set-dispatch-macro-character #\# #\@ #'proto-transformer) 