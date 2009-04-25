;; This file is part of Sheeple

;; bootstrap.lisp
;;
;; This creates all the base objects that will be used during cloning.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :sheeple)

(defvar =t=
  (let ((sheep (%make-sheep)))
    (setf (sheep-nickname sheep) "=t=")
    (finalize-sheep sheep)
    sheep))

(defun spawn-sheep (sheeple &rest all-keys)
  "Creates a new sheep with SHEEPLE as its parents, and PROPERTIES as its properties"
  (let ((sheep (apply #'initialize-sheep
		      (let ((sheep (add-parents (%make-sheep) sheeple)))
			(finalize-sheep sheep)
			sheep)
		      all-keys)))
    sheep))

(defbuzzword initialize-sheep (sheep &key))
(defmessage initialize-sheep (sheep
			      &key 
			      properties
			      nickname
			      deep-copy
			      shallow-copy)
  (set-up-properties sheep properties)
  (execute-clonefunctions sheep)
  (setf (sheep-nickname sheep) nickname)
  (finalize-sheep sheep)
  (when shallow-copy
    (shallow-copy sheep))
  (when deep-copy
    (deep-copy sheep))
  sheep)

(defbuzzword reinitialize-sheep (sheep &key))
(defmessage reinitialize-sheep (sheep
				&key new-parents
				new-properties
				nickname
				deep-copy shallow-copy)
  ;; cleanup
  (loop for parent in (sheep-direct-parents sheep)
       do (remove-parent parent sheep))
  (clrhash (sheep-cloneforms sheep))
  (clrhash (sheep-clonefunctions sheep))
  (clrhash (sheep-direct-properties sheep))
  (add-parents sheep new-parents)
  ;; initialize again
  (initialize-sheep sheep 
		    :properties new-properties
		    :nickname nickname
		    :deep-copy deep-copy
		    :shallow-copy shallow-copy))

(defsheep =dolly= (=t=)
  () (:nickname "=dolly="))

;;; Wolves and wolf-handling
(defsheep =white-fang= (=t=)
  () (:nickname "=white-fang="))

(defsheep =symbol=  (=white-fang=)
  () (:nickname "=symbol="))

(defsheep =sequence=  (=white-fang=)
  () (:nickname "=sequence="))

(defsheep =array=  (=white-fang=)
  () (:nickname "=array="))

(defsheep =number=  (=white-fang=)
  () (:nickname "=number="))

(defsheep =character= (=white-fang=)
  () (:nickname "=character="))

(defsheep =function=  (=white-fang=)
  () (:nickname "=function="))

(defsheep =hash-table= (=white-fang=)
  () (:nickname "=hash-table="))

(defsheep =package=  (=white-fang=)
  () (:nickname "=package="))

(defsheep =pathname=  (=white-fang=)
  () (:nickname "=pathname="))

(defsheep =readtable=  (=white-fang=)
  () (:nickname "=readtable="))

(defsheep =stream=  (=white-fang=)
  () (:nickname "=stream="))

(defsheep =list=  (=sequence=)
  () (:nickname "=list="))

(defsheep =null=  (=symbol= =list=)
  () (:nickname "=null="))

(defsheep =cons=  (=list=)
  () (:nickname "=cons="))

(defsheep =vector=  (=array= =sequence=)
  () (:nickname "=vector="))

(defsheep =bit-vector=  (=vector=)
  () (:nickname "=bit-vector="))

(defsheep =string=  (=vector=)
  () (:nickname "=string="))

(defsheep =complex=  (=number=)
  () (:nickname "=complex="))

(defsheep =integer=  (=number=)
  () (:nickname "=integer="))

(defsheep =float=  (=number=)
  () (:nickname "=float="))

