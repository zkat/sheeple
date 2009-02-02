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

;; clone.lisp
;;
;; Contains the Clone macro, and helper stuff. This is all interface fluff.
;;
;; TODO:
;; * Unit tests, please
;; * DOCUMENTATION!!
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :sheeple)

;;;
;;; The Macro
;;;
;; Example: (clone (sheep1 sheep2 sheep3) ((property1 value1) (property2 value2)))
(defmacro clone (sheeple properties &rest options)
  "Standard sheep-generation macro"
  `(create-sheep
    ,(canonize-sheeple sheeple)
    ,(canonize-properties properties)
    ,(canonize-options options)))

;;;
;;; Canonizers
;;;
(defun canonize-sheeple (sheeple)
  `(list ,@(mapcar #'canonize-sheep sheeple)))

(defun canonize-sheep (sheep)
  `(confirm-sheep ,sheep))

(defun confirm-sheep (sheep)
  (if (eql (class-of sheep)
	   (find-class 'standard-sheep))
      sheep
      (sheepify sheep)))

(defun canonize-properties (properties)
  `(list ,@(mapcar #'canonize-property properties)))

(defun canonize-property (property)
  (if (symbolp property)
      `(list :name ',property)
      (let ((name (car property))
	    (value (cadr property))
            (readers nil)
            (writers nil)
	    (locked-p nil)
            (other-options nil))
        (do ((olist (cddr property) (cddr olist)))
            ((null olist))
	  (case (car olist)
	    (:value
	     (setf value (cadr olist)))
	    (:val
	     (setf value (cadr olist)))
            (:reader 
             (pushnew (cadr olist) readers))
            (:writer 
             (pushnew (cadr olist) writers))
            (:accessor
             (pushnew (cadr olist) readers)
             (pushnew `(setf ,(cadr olist)) writers))
	    (:locked-p
	     (setf locked-p (cadr olist)))
	    (otherwise 
             (pushnew (cadr olist) other-options)
             (pushnew (car olist) other-options))))
	(if other-options
	    (error "Invalid property option(s)")
	    `(list
	      :name ',name
	      :value ,value
	      ,@(when readers `(:readers ',readers))
	      ,@(when writers `(:writers ',writers))
	      ,@(when locked-p `(:locked-p ,locked-p)))))))


(defun canonize-options (options)
  `(list ,@(mapcar #'canonize-option options)))

(defun canonize-option (option)
  `(list ,@option))
