;; Copyright 2008 Josh Marchan

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

;; tests/buzzwords.lisp
;;
;; Unit tests for src/buzzwords.lisp
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :sheeple-tests)

(def-suite buzzword-tests :in sheeple)

(in-suite buzzword-tests)

(test buzzword-definition
  "Checks basic buzzword definition, confirms that FIND-BUZZWORD 
errors when a buzzword doesn't exist."
  (defbuzzword test-buzz "This is a test")
  (is (buzzword-p (find-buzzword 'test-buzz)))
  (signals no-such-buzzword (find-buzzword 'another-buzzword))
  (undefbuzzword test-buzz)
  (defun test-buzz () (print "durr hurr"))
  (signals clobbering-function-definition (defbuzzword test-buzz "OHNOES"))
  (fmakunbound 'test-buzz))

(test buzzword-undefinition
  "Usage of the undefbuzzword macro, confirmation of removal of all messages and
relevant role objects from the system."
  (defbuzzword test-buzz)
  (undefbuzzword test-buzz)
  (signals no-such-buzzword (find-buzzword 'test-buzz))
  (signals undefined-function (test-buzz))
  (is (not (sheeple::participant-p =dolly= 'test-buzz)))
  (defmessage another-buzzer (foo) foo)
  (defmessage another-buzzer ((foo =string=)) (declare (ignore foo)) "String returned!")
  (undefmessage another-buzzer ((foo =string=)))
  (is (equal "hei-ho!" (another-buzzer "hei-ho!")))
  (undefmessage another-buzzer (foo))
  (signals sheeple::no-most-specific-message (another-buzzer =dolly=)) ; this package bs pisses me off
  (is (not (sheeple::participant-p =dolly= 'test-buzz)))
  (is (not (sheeple::participant-p =string= 'test-buzz)))
  (undefbuzzword another-buzzer)
  (signals no-such-buzzword (find-buzzword 'another-buzzer))
  (signals undefined-function (another-buzzer "WHAT ARE YOU GONNA DO, BLEED ON ME?!")))

(test message-undefinition
  "Tests undefmessage macro."
  (defbuzzword test-message)
  (defmessage foo (foo) foo)
  (is (equal =dolly= (foo =dolly=)))
  (undefmessage foo (foo))
  (signals sheeple::no-most-specific-message (foo =dolly=))
  (defmessage foo ((string =string=) (another =string=)) (concatenate 'string string another))
  (is (equal "fullstring" (foo "full" "string")))
  (undefmessage foo ((x =string=) (y =string=)))
  (signals sheeple::no-most-specific-message (foo "full" "string"))
  (undefbuzzword test-message))

(test basic-message-definition
  "Checks that messages are defined properly, and added to their respective objects.
also, checks that a style-warning is signaled if there is no buzzword defined."
  (undefbuzzword test-message)
  (signals style-warning (defmessage test-message (foo) (print foo)))
  (defbuzzword test-message)
  (defmessage test-message (foo) (print foo))
  (is (buzzword-p (find-buzzword 'test-message)))
  (is (message-p (car (buzzword-messages (find-buzzword 'test-message)))))
  (undefbuzzword test-message))

(test multimessage-definition
  "Checks that multimessages are defined correctly, with roles added
to their respective participants, with correct role-indexes, etc."
  (defmessage test-message (foo bar) bar foo)
  (let ((sheep1 (clone () () (:nickname "sheep1")))
	(sheep2 (clone () () (:nickname "sheep2"))))
    (defmessage test-message ((foo sheep1) (bar sheep2)) foo bar)
    (is (equal t (sheeple::participant-p sheep1 'test-message)))
    (is (equal t (sheeple::participant-p sheep2 'test-message)))
    (let ((sheep1-roles (sheep-direct-roles sheep1))
	  (sheep2-roles (sheep-direct-roles sheep2)))
      (is (equal 0 (role-position (car sheep1-roles))))
      (is (equal 1 (role-position (car sheep2-roles))))))
  (undefbuzzword test-message))

(test message-redefinition
  "Confirms correct redefinition of messages"
  (defbuzzword synergize)
  (defmessage synergize ((x =number=) (y =number=)) (+ x y))
  (is (= 10 (synergize 6 4)))
  (defmessage synergize ((y =number=) (x =number=)) (* x y))
  (is (= 24 (synergize 6 4)))
  (undefbuzzword synergize))

(test basic-message-dispatch
  "Checks that basic single-dispatch messages work."
  (let ((test-sheep (clone () () (:nickname "testie")))
	(another-sheep (clone () () (:nickname "rejected-failure"))))
    (defmessage test-message ((sheep test-sheep)) (sheep-nickname sheep))
    (is (equal "testie" (test-message test-sheep)))
    (signals sheeple::no-most-specific-message (test-message another-sheep)))
  (undefbuzzword test-message))

(test multimessage-dispatch
  "Checks correct multimethod dispatching."
  (defmessage foo ((foo =number=))
    foo)
  (defmessage foo ((foo =number=) (bar =number=))
    (+ foo bar))
  (defmessage foo (foo bar)
    foo bar)
  (is (= 5 (foo 2 3)))
  (is (= 5 (foo 5)))
  (is (equal "bar" (foo "foo" "bar"))))


