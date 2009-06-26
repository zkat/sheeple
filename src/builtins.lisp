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
	(null                                          (find-sheep 'null))
	((and symbol (not null))                       (find-sheep 'symbol))
	((complex *)                                   (find-sheep 'complex))
	((integer * *)                                 (find-sheep 'integer))
	((float * *)                                   (find-sheep 'float))
	(cons                                          (find-sheep 'cons))
	(character                                     (find-sheep 'character))
	(hash-table                                    (find-sheep 'hash-table))
	(package                                       (find-sheep 'package))
	(pathname                                      (find-sheep 'pathname))
	(readtable                                     (find-sheep 'readtable))
	(stream                                        (find-sheep 'stream))
	((and number (not (or integer complex float))) (find-sheep 'number))
	((string *)                                    (find-sheep 'string))
	((bit-vector *)                                (find-sheep 'bit-vector))
	((and vector (not string))                     (find-sheep 'vector))
	((and array (not vector))                      (find-sheep 'array))
	((and sequence (not (or vector list)))         (find-sheep 'sequence))
	(function                                      (find-sheep 'function))
	(t                                             (find-sheep 'boxed-object)))))

;; Boxed object table
(let ((boxed-object-table (make-hash-table :test #'equal)))

  (defun find-boxed-object (object)
    (if (sheep-p object)
	(error "~S seems to already be a sheep." object)
	(gethash object boxed-object-table)))

  (defun box-object (object)
    "Wraps OBJECT with a sheep."
    (setf (gethash object boxed-object-table) 
          (defclone ((box-type-of object))
              ((wrapped-object object)) (:nickname object))))

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
