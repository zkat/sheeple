;; This file is part of Sheeple

;; bootstrap.lisp
;;
;; This creates all the base objects that will be used during cloning.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :sheeple)

(setf (proto 't)
      (let ((sheep (allocate-sheep)))
        (setf (sheep-nickname sheep) 't)
        (finalize-sheep sheep)
        sheep))

(setf (proto 'dolly)
  (let ((sheep (allocate-sheep)))
    (setf (sheep-nickname sheep) 'dolly)
    (add-parent (proto 't) sheep)
    sheep))

(defmessage init-sheep (sheep &key &allow-other-keys))
(defreply init-sheep (sheep
		      &key 
		      nickname
		      documentation
		      properties)
  (when properties
    (mapcar (lambda (prop-spec)
	      (apply #'add-property sheep prop-spec))
	    properties))
  (when nickname
    (setf (sheep-nickname sheep) nickname))
  (when documentation
    (setf (sheep-documentation sheep) documentation))
  sheep)

(defmessage reinit-sheep (sheep &key  &allow-other-keys)
  (:documentation "Resets the sheep's parents and properties."))
(defreply reinit-sheep (sheep &key new-parents
                              documentation
			      properties)
  "If :NEW-PARENTS is  provided, those parents are used when reinitializing,
so DOLLY doesn't end up on the list by default."
  ;; CLOBBER TIME
  (loop for parent in (sheep-parents sheep)
     do (remove-parent parent sheep))
  (remove-all-direct-properties sheep)
  ;; MOAR PARENTS
  (add-parents (if new-parents (sheepify-list new-parents) (list (proto 'dolly)))
               sheep)
  ;; set up some new properties.
  (mapc (lambda (prop-spec)
	  (apply #'add-property sheep prop-spec))
	properties)
  ;; DOX PLOX
  (when documentation
    (setf (sheep-documentation sheep) documentation))
  sheep)

(defproto dolly (t) ())

;;; Boxed built-ins
(defproto boxed-object (t) ())
(defproto symbol ((proto 'boxed-object)) ())
(defproto sequence ((proto 'boxed-object)) ())
(defproto array ((proto 'boxed-object)) ())
(defproto number ((proto 'boxed-object)) ())
(defproto character ((proto 'boxed-object)) ())
(defproto function ((proto 'boxed-object)) ())
(defproto hash-table ((proto 'boxed-object)) ())
(defproto package ((proto 'boxed-object)) ())
(defproto pathname ((proto 'boxed-object)) ())
(defproto readtable ((proto 'boxed-object)) ())
(defproto stream ((proto 'boxed-object)) ())
(defproto list ((proto 'sequence)) ())
(defproto null ((proto 'symbol) (proto 'list)) ())
(defproto cons ((proto 'list)) ())
(defproto vector ((proto 'array) (proto 'sequence)) ())
(defproto bit-vector ((proto 'vector)) ())
(defproto string ((proto 'vector)) ())
(defproto complex ((proto 'number)) ())
(defproto integer ((proto 'number)) ())
(defproto float ((proto 'number)) ())
