;; This file is part of Sheeple

;; bootstrap.lisp
;;
;; This creates all the base objects that will be used during cloning.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :sheeple)

(progn
  (setf =t=
	(let ((sheep (%make-sheep)))
	  (setf (sheep-nickname sheep) "=t=")
	  (finalize-sheep sheep)
	  sheep))

  (setf =dolly= 
	(clone (=t=)
	       ()
	       (:nickname "=dolly="))))

;;; Wolves and wolf-handling
(setf =white-fang= (clone (=t=) () (:nickname "=white-fang=")))
(setf =symbol= (clone (=white-fang=)()(:nickname "=symbol=")))
(setf =sequence= (clone (=white-fang=)()(:nickname "=sequence=")))
(setf =array= (clone (=white-fang=)()(:nickname "=array=")))
(setf =number= (clone (=white-fang=) () (:nickname "=number=")))
(setf =character= (clone (=white-fang=) () (:nickname "=character=")))
(setf =function= (clone (=white-fang=) () (:nickname "=function=")))
(setf =hash-table= (clone (=white-fang=) () (:nickname "=hash-table=")))
(setf =package= (clone (=white-fang=) () (:nickname "=package=")))
(setf =pathname= (clone (=white-fang=) () (:nickname "=pathname=")))
(setf =readtable= (clone (=white-fang=) () (:nickname "=readtable=")))
(setf =stream= (clone (=white-fang=) () (:nickname "=stream=")))
(setf =list= (clone (=sequence=) () (:nickname "=list=")))
(setf =null= (clone (=symbol= =list=) () (:nickname "=null=")))
(setf =cons= (clone (=list=) () (:nickname "=cons=")))
(setf =vector= (clone (=array= =sequence=) () (:nickname "=vector=")))
(setf =bit-vector= (clone (=vector=) () (:nickname "=bit-vector=")))
(setf =string= (clone (=vector=) () (:nickname "=string=")))
(setf =complex= (clone (=number=) () (:nickname "=complex=")))
(setf =integer= (clone (=number=) () (:nickname "=integer=")))
(setf =float= (clone (=number=) () (:nickname "=float=")))

;;; Now we create the buzzword, message, and role metasheeps
(setf =standard-role-metasheep=
  (eval the-standard-role-metasheep-form))
(setf =standard-message-metasheep=
  (eval the-standard-message-metasheep-form))
(setf =standard-buzzword-metasheep=
  (eval the-standard-buzzword-metasheep-form))
