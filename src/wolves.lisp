;; This file is part of Sheeple

;; wolves.lisp
;;
;; Fleecing and sheepification
;;
;; TODO: This is such a mess...
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :sheeple)

(declaim (optimize (speed 3) (safety 1)))
(defun fleece-of (x)
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

  (defun find-fleeced-wolf (wolf)
    (if (sheep-p wolf)
	(error "~S seems to already be a sheep." wolf)
	(gethash wolf boxed-object-table)))

  (defun wear-wool (wolf)
    "Autoboxes WOLF"
    (setf (gethash wolf boxed-object-table) (clone ((fleece-of wolf)) ((wolf wolf)) (:nickname wolf))))

  (defun shoot-wolf (wolf)
    "Kills wolf dead"
    (remhash wolf boxed-object-table))
    
  ) ; end boxed object table

(defun sheepify-list (obj-list)
  "Converts OBJ-LIST to a list where each item is either a sheep or a fleeced wolf."
  (mapcar #'sheepify obj-list))

(defun sheepify (sheep)
  "Returns SHEEP or fleeces it."
   (if (not (sheep-p sheep))
       (or (find-fleeced-wolf sheep)
	   (values (wear-wool sheep) t))
       (values sheep nil)))
