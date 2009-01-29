;; Copyright 2008 Kat Marchan

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

;; tests/compatibility.lisp
;;
;; Unit tests for src/compatibility.lisp
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :sheeple-tests)

(def-suite compat-tests :in sheeple)

(in-suite compat-tests)

(test fleece-of-basic
  "Tests that the fleece-of function returns the right fleeced-wolf for each lisp type"
  (is (equal =null= (fleece-of nil)))
  (is (equal =symbol= (fleece-of 'foo)))
  (is (equal =complex= (fleece-of #C (10 10))))
  (is (equal =integer= (fleece-of 1)))
  (is (equal =float= (fleece-of 1.0)))
  (is (equal =cons= (fleece-of (cons 1 2))))
  (is (equal =character= (fleece-of #\a)))
  (is (equal =hash-table= (fleece-of (make-hash-table))))
  (is (equal =package= (fleece-of (find-package :sheeple-tests))))
  (is (equal =pathname= (fleece-of #P"compatibility.lisp")))
;;  (is (equal =readtable= (fleece-of ???))) ; uhhh??
;;  (is (equal =stream= (fleece-of ???))) ; dunno
  (is (equal =number= (fleece-of 1/2)))
  (is (equal =string= (fleece-of "foo")))
  (is (equal =bit-vector= (fleece-of #*)))
  (is (equal =vector= (fleece-of (vector 1 2 3))))
  (is (equal =array= (fleece-of (make-array '(1 2)))))
;;  (is (equal =sequence= (fleece-of ???))) ;hm
  (is (equal =function= (fleece-of (lambda () 1)))))

(test clos-fleecing
  "These will all fail for now."
  (defclass foo () ())
  (is (not (equal =dolly= (fleece-of (make-instance 'foo))))))