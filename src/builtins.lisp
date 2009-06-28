;;;; This file is part of Sheeple

;;;; builtins.lisp
;;;;
;;;; Boxing of built-in lisp types
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :sheeple)

(declaim (optimize (speed 3) (safety 1)))
(defun box-type-of (x)
  (if (sheep-p x)
      (progn
	(warn "This is already a sheep!")
	x)
      (typecase x
	(null                                          (find-proto 'null))
	((and symbol (not null))                       (find-proto 'symbol))
	((complex *)                                   (find-proto 'complex))
	((integer * *)                                 (find-proto 'integer))
	((float * *)                                   (find-proto 'float))
	(cons                                          (find-proto 'cons))
	(character                                     (find-proto 'character))
	(hash-table                                    (find-proto 'hash-table))
	(package                                       (find-proto 'package))
	(pathname                                      (find-proto 'pathname))
	(readtable                                     (find-proto 'readtable))
	(stream                                        (find-proto 'stream))
	((and number (not (or integer complex float))) (find-proto 'number))
	((string *)                                    (find-proto 'string))
	((bit-vector *)                                (find-proto 'bit-vector))
	((and vector (not string))                     (find-proto 'vector))
	((and array (not vector))                      (find-proto 'array))
;	((and sequence (not (or vector list)))         (find-proto 'sequence))
	(function                                      (find-proto 'function))
	(t                                             (find-proto 'boxed-object)))))

;; Boxed object table
(let ((boxed-object-table (make-hash-table :test #'equal)))

  (defun find-boxed-object (object &optional (errorp nil))
    "Finds a previously-boxed object in the boxed object table.
If ERRORP is T, this signals an error if OBJECT is a sheep, or if OBJECT
has not already been boxed."
    (if (sheep-p object)
        (if errorp (error "~S seems to already be a sheep." object) nil)
        (multiple-value-bind (sheep hasp)
            (gethash object boxed-object-table)
          (if hasp
              sheep
              (if errorp (error "~S has not been boxed." object) nil)))))

  (defun box-object (object)
    "Wraps OBJECT with a sheep."
    (if (sheep-p object)
        (error "~S seems to already be a sheep." object)
        (setf (gethash object boxed-object-table) 
              (defclone ((box-type-of object))
                  ((wrapped-object object)) (:nickname object)))))

  (defun remove-boxed-object (object)
    "Kills object dead"
    (remhash object boxed-object-table))
    
  ) ; end boxed object table

(defun sheepify-list (obj-list)
  "Converts OBJ-LIST to a list where each item is either a sheep or a fleeced wolf."
  (mapcar #'sheepify obj-list))

(defun sheepify (object)
  "Returns OBJECT or fleeces it."
   (cond ((eq object t)
          (find-proto 't))
         ((not (sheep-p object))
          (or (find-boxed-object object)
              (values (box-object object) t)))
         (t
          (values object nil))))
