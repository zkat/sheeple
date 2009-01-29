;; Copyright 2008, 2009 Kat Marchan

;; Permission is hereby granted, free of charge, to any person
;; obtaining a copy of this software and associated documentation
;; files (the "Software"), to deal in the Software without
;; restriction, including without limitation the rights to use,
;; copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the
;; Software is furnished to do so, subject to the following
;; conditions:

;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
;; OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
;; HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
;; WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
;; FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
;; OTHER DEALINGS IN THE SOFTWARE.

;; compatibility.lisp
;;
;; Ensures that sheeple is fully-compatible with regular Common Lisp.
;; Fleecing (autoboxing) is implemented here.
;;
;; TODO:
;; * Write unit tests
;; * DOCUMENTATION!!!1
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :sheeple)

;; Fleeces
(defparameter =white-fang= (clone () () (:nickname "=white-fang=")))
(defparameter =symbol= (clone (=white-fang=)()(:nickname "=symbol=")))
(defparameter =sequence= (clone (=white-fang=)()(:nickname "=sequence=")))
(defparameter =array= (clone (=white-fang=)()(:nickname "=array=")))
(defparameter =number= (clone (=white-fang=) () (:nickname "=number=")))
(defparameter =character= (clone (=white-fang=) () (:nickname "=character=")))
(defparameter =function= (clone (=white-fang=) () (:nickname "=function=")))
(defparameter =hash-table= (clone (=white-fang=) () (:nickname "=hash-table=")))
(defparameter =package= (clone (=white-fang=) () (:nickname "=package=")))
(defparameter =pathname= (clone (=white-fang=) () (:nickname "=pathname=")))
(defparameter =readtable= (clone (=white-fang=) () (:nickname "=readtable=")))
(defparameter =stream= (clone (=white-fang=) () (:nickname "=stream=")))
(defparameter =list= (clone (=sequence=) () (:nickname "=list=")))
(defparameter =null= (clone (=symbol= =list=) () (:nickname "=null=")))
(defparameter =cons= (clone (=list=) () (:nickname "=cons=")))
(defparameter =vector= (clone (=array= =sequence=) () (:nickname "=vector=")))
(defparameter =bit-vector= (clone (=vector=) () (:nickname "=bit-vector=")))
(defparameter =string= (clone (=vector=) () (:nickname "=string=")))
(defparameter =complex= (clone (=number=) () (:nickname "=complex=")))
(defparameter =integer= (clone (=number=) () (:nickname "=integer=")))
(defparameter =float= (clone (=number=) () (:nickname "=float=")))

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
	(t                                             =dolly=))))

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
    
  );; end boxed objects

;; (defun define-fleeced-wolf (lisp-object &optional nickname)
;;   ;;TODO -- This could be useful for integrating with deftype and CLOS
;;   )

(defun sheepify-list (obj-list)
  "Converts OBJ-LIST to a list where each item is either a sheep or a fleeced wolf."
  (mapcar #'sheepify obj-list))

(defun sheepify (sheep)
  "Returns SHEEP or fleeces it."
   (if (not (sheep-p sheep))
       (find-fleeced-wolf sheep)
       sheep))

