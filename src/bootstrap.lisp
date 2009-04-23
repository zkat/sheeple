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

(defsheep =hash= (=white-fang=)
  () (:nickname "=hash-table=") (clone  ()))

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

