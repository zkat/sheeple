;; This file is part of Sheeple

;; bootstrap.lisp
;;
;; This creates all the base objects that will be used during cloning.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :sheeple)

(defvar proto-t
  (let ((sheep (make-instance 'standard-sheep)))
    (setf (sheep-nickname sheep) 'proto-t)
    (finalize-sheep sheep)
    sheep))

(defvar dolly
  (let ((sheep (make-instance 'standard-sheep)))
    (setf (sheep-direct-parents sheep) (list (find-sheep 'proto-t)))
    (setf (sheep-nickname sheep) 'dolly)
    (finalize-sheep sheep)
    sheep))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun spawn-sheep (sheeple &rest all-keys &key (metaclass 'standard-sheep) &allow-other-keys)
    "Creates a new sheep with SHEEPLE as its parents, and PROPERTIES as its properties"
    (let ((sheep (apply #'initialize-sheep
                        (apply #'allocate-sheep metaclass
                               :direct-parents (if sheeple
                                                   (sheepify-list sheeple)
                                                   (list =dolly=))
                               all-keys)
                        all-keys)))
      sheep))

    (defun clone (&rest sheeple)
      (spawn-sheep sheeple)))

(defbuzzword initialize-sheep (sheep &key))
(defmessage initialize-sheep (sheep
                              &key 
                              properties
                              nickname
                              documentation)
  (set-up-properties sheep properties)
  (setf (sheep-nickname sheep) nickname)
  (when documentation
    (setf (sheep-documentation sheep) documentation))
  sheep)

(defbuzzword reinitialize-sheep (sheep &key))
(defmessage reinitialize-sheep (sheep
                                &key new-parents
                                new-properties
                                nickname
                                documentation
                                deep-copy shallow-copy)
  ;; cleanup
  (loop for parent in (sheep-direct-parents sheep)
     do (remove-parent parent sheep))
  (clrhash (sheep-property-value-table sheep))
  (if new-parents
      (sheepify-list new-parents)
      (list =dolly=))
  ;; initialize again
  (initialize-sheep sheep 
                    :properties new-properties
                    :nickname nickname
                    :deep-copy deep-copy
                    :shallow-copy shallow-copy
                    :documentation documentation))

(defproto dolly (proto-t) ())

;;; Wolves and wolf-handling
(defproto boxed-object (proto-t) ())
(defproto symbol  (boxed-object) ())
(defproto sequence  (boxed-object) ())
(defproto array  (boxed-object) ())
(defproto number  (boxed-object) ())
(defproto character (boxed-object) ())
(defproto function  (boxed-object) ())
(defproto hash-table (boxed-object) ())
(defproto package  (boxed-object) ())
(defproto pathname  (boxed-object) ())
(defproto readtable  (boxed-object) ())
(defproto stream  (boxed-object) ())
(defproto list  (sequence) ())
(defproto null  (symbol list) ())
(defproto cons  (list) ())
(defproto vector  (array sequence) ())
(defproto bit-vector  (vector) ())
(defproto string  (vector) ())
(defproto complex  (number) ())
(defproto integer  (number) ())
(defproto float  (number) ())
