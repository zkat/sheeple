(defvar =standard-sheep-metasheep=
  (let ((object (make-hash-table :test #'equal)))
    (setf (gethash *secret-sheep-identifier* object) *secret-sheep-identifier*)
    (setf (gethash 'metasheep object) object)
    (setf (gethash 'nickname object) nil)
    (setf (gethash 'parents object) nil)
    (setf (gethash 'children object) nil)
    (setf (gethash 'properties object) (make-hash-table :test #'equal))
    (setf (gethash 'property-owners object) (make-weak-hash-table :weakness :value :test #'equal))
    (setf (gethash 'roles object) nil)
    (setf (gethash 'hierarchy-list object) nil)
    object))
(std-finalize-sheep =standard-sheep-metasheep=)
(defvar =t=
  (let ((obj (std-generate-sheep-instance =standard-sheep-metasheep=)))
    (setf (gethash 'nickname obj) "=t=")
    obj))

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

(defun fleece-of (x)
  (if (sheep-p x)
      (progn
	(warn "This is already a sheep!")
	x)
      (typecase x
	(null                                          =null=)
	((and symbol (not null))                       =symbol=)
	((complex *)                                   =complex=)
	((integer * *)                                 =integer=)
	((float * *)                                   =float=)
	(cons                                          =cons=)
	(character                                     =character=)
	(hash-table                                    =hash-table=)
	(package                                       =package=)
	(pathname                                      =pathname=)
	(readtable                                     =readtable=)
	(stream                                        =stream=)
	((and number (not (or integer complex float))) =number=)
	((string *)                                    =string=)
	((bit-vector *)                                =bit-vector=)
	((and vector (not string))                     =vector=)
	((and array (not vector))                      =array=)
	((and sequence (not (or vector list)))         =sequence=)
	(function                                      =function=)
	(t                                             =white-fang=))))

;; Boxed object table
(let ((boxed-object-table (make-hash-table :test #'equal)))

  (defun find-fleeced-wolf (wolf)
    (if (sheep-p wolf)
	(error "~S seems to already be a sheep." wolf)
	(if (gethash wolf boxed-object-table)
	    (gethash wolf boxed-object-table)
	    (values (wear-wool wolf) nil))))

  (defun wear-wool (wolf)
    "Autoboxes WOLF"
    (setf (gethash wolf boxed-object-table) (clone ((fleece-of wolf)) ((wolf wolf)))))

  (defun shoot-wolf (wolf)
    "Kills wolf dead"
    (remhash wolf boxed-object-table))
    
  ) ; end boxed object table

;;;
;;; Now we create the buzzword, message, and role metasheeps
;;;
(defvar =standard-buzzword-metasheep=
  (eval the-standard-buzzword-metasheep-form))
(defvar =standard-message-metasheep=
  (eval the-standard-message-metasheep-form))
(defvar =standard-role-metasheep=
  (eval the-standard-role-metasheep-form))


;; These are what reinitializing metasheep is for...
;; (defun set-up-standard-sheep-metasheep-cloneforms (cloneform-table)
;;   (setf (gethash 'metasheep cloneform-table) '=standard-sheep-metasheep=)
;;   (setf (gethash 'nickname cloneform-table) 'nil)
;;   (setf (gethash 'parents cloneform-table) 'nil)
;;   (setf (gethash 'children cloneform-table) 'nil)
;;   (setf (gethash 'properties cloneform-table) '(make-hash-table :test #'equal))
;;   (setf (gethash 'property-owners cloneform-table) '(make-weak-hash-table :weakness :value :test #'equal))
;;   (setf (gethash 'roles cloneform-table) 'nil)
;;   (setf (gethash 'hierarchy-list cloneform-table) 'nil)
;;   (setf (gethash 'cloneforms cloneform-table) '(make-hash-table :test #'equal))
;;   (setf (gethash 'clonefunctions cloneform-table) '(make-hash-table :test #'equal)))

;; (defun set-up-standard-sheep-metasheep-clonefunctions (clonefun-table)
;;   (setf (gethash 'metasheep clonefun-table) (lambda () =standard-sheep-metasheep=))
;;   (setf (gethash 'nickname clonefun-table) (lambda () nil))
;;   (setf (gethash 'parents clonefun-table) (lambda () nil))
;;   (setf (gethash 'children clonefun-table) (lambda () nil))
;;   (setf (gethash 'properties clonefun-table) (lambda () (make-hash-table :test #'equal)))
;;   (setf (gethash 'property-owners clonefun-table) (lambda () (make-weak-hash-table :weakness :value :test #'equal)))
;;   (setf (gethash 'roles clonefun-table) (lambda () nil))
;;   (setf (gethash 'hierarchy-list clonefun-table) (lambda () nil))
;;   (setf (gethash 'cloneforms clonefun-table) (lambda () (make-hash-table :test #'equal)))
;;   (setf (gethash 'clonefunctions clonefun-table) (lambda () (make-hash-table :test #'equal))))

