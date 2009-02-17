;; bootstrap.lisp
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :sheeplette)

(defvar =standard-sheep-metasheep=
  (let ((object (make-hash-table :test #'equal)))
    (setf (gethash *secret-sheep-identifier* object) *secret-sheep-identifier*)
    (setf (gethash 'metasheep object) object)
    object))

(defvar =t=
  (let ((sheep (std-generate-sheep-instance nil)))
    (setf (gethash 'nickname sheep) "=t=")
    (setf (gethash 'parents sheep) nil)
    (setf (gethash 'properties sheep) (make-hash-table :test #'equal))
    (setf (gethash 'roles sheep) nil)
    (setf (gethash 'cloneforms sheep) (make-hash-table :test #'equal))
    (setf (gethash 'clonefunctions sheep) (make-hash-table :test #'equal))
    (setf (gethash 'children sheep) nil)
    (setf (gethash 'property-owners sheep) (make-weak-hash-table :weakness :value :test #'equal))
    (setf (gethash 'hierarchy-list sheep) nil)
    (std-finalize-sheep sheep)
    sheep))

(defvar =dolly= 
  (clone (=t=)
	 ()
	 (:nickname "=dolly=")))

(setf =standard-sheep-metasheep= (eval the-standard-sheep-metasheep-form))

;;; Wolves and wolf-handling
(defvar =white-fang= (clone (=t=) () (:nickname "=white-fang=")))
(defvar =symbol= (clone (=white-fang=)()(:nickname "=symbol=")))
(defvar =sequence= (clone (=white-fang=)()(:nickname "=sequence=")))
(defvar =array= (clone (=white-fang=)()(:nickname "=array=")))
(defvar =number= (clone (=white-fang=) () (:nickname "=number=")))
(defvar =character= (clone (=white-fang=) () (:nickname "=character=")))
(defvar =function= (clone (=white-fang=) () (:nickname "=function=")))
(defvar =hash-table= (clone (=white-fang=) () (:nickname "=hash-table=")))
(defvar =package= (clone (=white-fang=) () (:nickname "=package=")))
(defvar =pathname= (clone (=white-fang=) () (:nickname "=pathname=")))
(defvar =readtable= (clone (=white-fang=) () (:nickname "=readtable=")))
(defvar =stream= (clone (=white-fang=) () (:nickname "=stream=")))
(defvar =list= (clone (=sequence=) () (:nickname "=list=")))
(defvar =null= (clone (=symbol= =list=) () (:nickname "=null=")))
(defvar =cons= (clone (=list=) () (:nickname "=cons=")))
(defvar =vector= (clone (=array= =sequence=) () (:nickname "=vector=")))
(defvar =bit-vector= (clone (=vector=) () (:nickname "=bit-vector=")))
(defvar =string= (clone (=vector=) () (:nickname "=string=")))
(defvar =complex= (clone (=number=) () (:nickname "=complex=")))
(defvar =integer= (clone (=number=) () (:nickname "=integer=")))
(defvar =float= (clone (=number=) () (:nickname "=float=")))

;;; Now we create the buzzword, message, and role metasheeps
(defvar =standard-message-metasheep=
  (eval the-standard-message-metasheep-form))
(defvar =standard-role-metasheep=
  (eval the-standard-role-metasheep-form))
(defvar =standard-buzzword-metasheep=
  (eval the-standard-buzzword-metasheep-form))
