;; This file is part of Sheeple

;; bootstrap.lisp
;;
;; This creates all the base objects that will be used during cloning.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :sheeple)

(defvar =t=
  (let ((sheep (%make-sheep)))
    (finalize-sheep sheep)
    sheep))

(defvar =dolly=
  (let ((sheep (%make-sheep)))
    (setf (sheep-direct-parents sheep) (list =t=))
    (finalize-sheep sheep)
    sheep))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun spawn-sheep (sheeple &rest all-keys &key (metaclass 'standard-sheep) &allow-other-keys)
    "Creates a new sheep with SHEEPLE as its parents, and PROPERTIES as its properties"
    (let ((sheep (apply #'initialize-sheep
                        (let ((sheep (add-parents (%make-sheep metaclass) (sheepify-list sheeple))))
                          (finalize-sheep sheep)
                          sheep)
                        all-keys)))
      sheep)))

(defbuzzword initialize-sheep (sheep &key))
(defmessage initialize-sheep (sheep
                              &key 
                              properties
                              nickname
                              documentation
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
  (clrhash (sheep-cloneforms sheep))
  (clrhash (sheep-clonefunctions sheep))
  (clrhash (sheep-property-value-table sheep))
  (add-parents sheep new-parents)
  ;; initialize again
  (initialize-sheep sheep 
                    :properties new-properties
                    :nickname nickname
                    :deep-copy deep-copy
                    :shallow-copy shallow-copy
                    :documentation documentation))

(setf (sheep-nickname =t=) "=T=")
(defsheep =dolly= (=t=) ())

;;; Wolves and wolf-handling
(defsheep =white-fang= (=t=) ())
(defsheep =symbol=  (=white-fang=) ())
(defsheep =sequence=  (=white-fang=) ())
(defsheep =array=  (=white-fang=) ())
(defsheep =number=  (=white-fang=) ())
(defsheep =character= (=white-fang=) ())
(defsheep =function=  (=white-fang=) ())
(defsheep =hash-table= (=white-fang=) ())
(defsheep =package=  (=white-fang=) ())
(defsheep =pathname=  (=white-fang=) ())
(defsheep =readtable=  (=white-fang=) ())
(defsheep =stream=  (=white-fang=) ())
(defsheep =list=  (=sequence=) ())
(defsheep =null=  (=symbol= =list=) ())
(defsheep =cons=  (=list=) ())
(defsheep =vector=  (=array= =sequence=) ())
(defsheep =bit-vector=  (=vector=) ())
(defsheep =string=  (=vector=) ())
(defsheep =complex=  (=number=) ())
(defsheep =integer=  (=number=) ())
(defsheep =float=  (=number=) ())
