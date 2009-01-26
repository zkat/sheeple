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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :sheeple)

;; Fleeces
(defparameter =symbol= (clone ()))
(defparameter =sequence= (clone ()))
(defparameter =array= (clone ()))
(defparameter =number= (clone ()))
(defparameter =character= (clone ()))
(defparameter =function= (clone ()))
(defparameter =hash-table= (clone ()))
(defparameter =package= (clone ()))
(defparameter =pathname= (clone ()))
(defparameter =readtable= (clone ()))
(defparameter =stream= (clone ()))
(defparameter =list= (clone (=sequence=)))
(defparameter =null= (clone (=symbol= =list=)))
(defparameter =cons= (clone (=list=)))
(defparameter =vector= (clone (=array= =sequence=)))
(defparameter =bit-vector= (clone (=vector=)))
(defparameter =string= (clone (=vector=)))
(defparameter =complex= (clone (=number=)))
(defparameter =integer= (clone (=number=)))
(defparameter =float= (clone (=number=)))

(defun fleece-of (x)
  (if (sheep-p x)
      (error "~S is a real sheep!" x)
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
	((and array vector)                            =array=)
	((and sequence (not (or vector list)))         =sequence=)
	(function                                      =function=)
	(t                                             =dolly=))))

;; Boxed object table
(let ((boxed-object-table (make-hash-table)))

  (defun find-fleeced-wolf (wolf)
    (if (sheep-p wolf)
	(error "~S seems to already be a sheep." wolf)
	(if (gethash wolf boxed-object-table)
	    (gethash wolf boxed-object-table)
	    (wear-wool wolf))))

  (defun wear-wool (wolf)
    (setf (gethash wolf boxed-object-table) (clone ((fleece-of wolf)) ((wolf wolf)))))

  (defun shoot-wolf (wolf)
    (remhash wolf boxed-object-table))
    
  );; end boxed objects
