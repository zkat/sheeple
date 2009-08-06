;; This file is part of Sheeple.

;; tests/compatibility.lisp
;;
;; Unit tests for src/builtins.lisp (and later, for clos-boxing)
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :sheeple)

(def-suite compat-tests :in sheeple)

(in-suite compat-tests)

(test box-type-of
  "Tests that the box-type-of function returns the right fleeced-wolf for each lisp type"
  (is (equal =null= (box-type-of nil)))
  (is (equal =symbol= (box-type-of 'foo)))
  (is (equal =complex= (box-type-of #C (10 10))))
  (is (equal =integer= (box-type-of 1)))
  (is (equal =float= (box-type-of 1.0)))
  (is (equal =cons= (box-type-of (cons 1 2))))
  (is (equal =character= (box-type-of #\a)))
  (is (equal =hash-table= (box-type-of (make-hash-table))))
  (is (equal =package= (box-type-of (find-package :sheeple))))
  (is (equal =pathname= (box-type-of #P"compatibility.lisp")))
  (is (equal =readtable= (box-type-of *readtable*)))
  (is (equal =stream= (box-type-of *standard-output*)))
  (is (equal =number= (box-type-of 1/2)))
  (is (equal =string= (box-type-of "foo")))
  (is (equal =bit-vector= (box-type-of #*)))
  (is (equal =vector= (box-type-of (vector 1 2 3))))
  (is (equal =array= (box-type-of (make-array '(1 2)))))
  (is (equal =function= (box-type-of (lambda () 1)))))

;; (test clos-boxing)
;; TODO - implement CLOS autoboxing

;; TODO - Finish this compat stuff.
(test find-boxed-object)
(test box-object)
(test remove-boxed-object)
(test sheepify)
(test sheepify-list)
