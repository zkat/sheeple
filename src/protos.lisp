(in-package :sheeple)

(let ((proto-table (make-hash-table :test #'eq)))
  
  (defun find-proto (name &optional errorp)
    (multiple-value-bind (proto hasp)
        (gethash name proto-table)
      (if hasp
          proto
          (if errorp
              (error "No such prototype: ~A" name)
              nil))))

  (defun (setf find-proto) (new-value name)
    (unless (sheep-p new-value)
      (error "~A is now a sheep object." new-value))
    (setf (gethash name proto-table) new-value))
  
  (defun delete-proto (name)
    (values (remhash name proto-table)))

  ) ; end prototype table

(defmacro defproto (name sheeple properties &rest options)
  `(let ((sheep (replace-or-reinitialize-sheep 
                 (find-proto ,name nil) 
                 ,(canonize-sheeple sheeple)
                 ,(canonize-properties* properties) 
                 ,@(canonize-clone-options options))))
     (unless (sheep-nickname sheep)
       (setf (sheep-nickname sheep) ',name))
     (setf (find-proto ,name) sheep)
     ',name))


(defun replace-or-reinitialize-sheep (maybe-sheep parents properties &rest options)
  (if (sheep-p maybe-sheep)
      (apply #'reinitialize-sheep maybe-sheep :new-parents parents :new-properties properties options)
      (apply #'spawn-sheep parents properties options)))