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
  (is (equal (proto 'null) (box-type-of nil)))
  (is (equal (proto 'symbol) (box-type-of 'foo)))
  (is (equal (proto 'complex) (box-type-of #C (10 10))))
  (is (equal (proto 'integer) (box-type-of 1)))
  (is (equal (proto 'float) (box-type-of 1.0)))
  (is (equal (proto 'cons) (box-type-of (cons 1 2))))
  (is (equal (proto 'character) (box-type-of #\a)))
  (is (equal (proto 'hash-table) (box-type-of (make-hash-table))))
  (is (equal (proto 'package) (box-type-of (find-package :sheeple))))
  (is (equal (proto 'pathname) (box-type-of #P"compatibility.lisp")))
  (is (equal (proto 'readtable) (box-type-of *readtable*)))
  (is (equal (proto 'stream) (box-type-of *standard-output*)))
  (is (equal (proto 'number) (box-type-of 1/2)))
  (is (equal (proto 'string) (box-type-of "foo")))
  (is (equal (proto 'bit-vector) (box-type-of #*)))
  (is (equal (proto 'vector) (box-type-of (vector 1 2 3))))
  (is (equal (proto 'array) (box-type-of (make-array '(1 2)))))
  (is (equal (proto 'function) (box-type-of (lambda () 1)))))

;; (test clos-boxing)
;; TODO - implement CLOS autoboxing

;; TODO - Finish this compat stuff.
(test find-boxed-object)
(test box-object)
(test remove-boxed-object)
(test sheepify)
(test sheepify-list)