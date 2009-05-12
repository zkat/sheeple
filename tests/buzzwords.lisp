;; This file is part of Sheeple

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
  (defbuzzword buzzword-definition-test (x) (:documentation "This is a test"))
  (is (buzzword-p (find-buzzword 'buzzword-definition-test)))
  (signals no-such-buzzword (find-buzzword 'another-buzzword-test))
  (defun clobbering-definition-test () (print "durr hurr"))
  (signals sheeple::clobbering-function-definition (defbuzzword clobbering-definition-test (foo)
						     (:documentation "OHNOES"))))

(test message-undefinition
  "Tests undefmessage macro."
  (defbuzzword foo (x y))
  (defmessage foo (foo bar) foo)
  (is (equal =dolly= (foo =dolly= =dolly=)))
  (undefmessage foo (foo bar))
  (signals sheeple::no-applicable-messages (foo =dolly= =dolly=))
  (defmessage foo ((string =string=) (another =string=)) (concatenate 'string string another))
  (is (equal "fullstring" (foo "full" "string")))
  (undefmessage foo ((x =string=) (y =string=)))
  (signals sheeple::no-applicable-messages (foo "full" "string")))

(test basic-message-definition
  "Checks that messages are defined properly, and added to their respective objects.
also, checks that a style-warning is signaled if there is no buzzword defined."
  (signals style-warning (defmessage totally-new-test-message (foo) (print foo)))
  (defbuzzword totally-new-test-message (x))
  (defmessage totally-new-test-message (foo) (print foo))
  (is (buzzword-p (find-buzzword 'totally-new-test-message)))
  (is (member 'totally-new-test-message (available-messages =dolly=) :key (lambda (v) (elt v 0))))
  (is (message-p (car (buzzword-messages (find-buzzword 'totally-new-test-message))))))

(test multimessage-definition
  "Checks that multimessages are defined correctly, with roles added
to their respective participants, with correct role-indexes, etc."
  (defmessage multi-message-test (foo bar) bar foo)
  (let ((sheep1 (clone () () (:nickname "sheep1")))
	(sheep2 (clone () () (:nickname "sheep2"))))
    (defmessage multi-message-test ((foo sheep1) (bar sheep2)) foo bar)
    (is (eql t (participant-p sheep1 'multi-message-test)))
    (is (eql t (participant-p sheep2 'multi-message-test)))
    (is (member 'multi-message-test (available-messages sheep1) :key (lambda (v) (elt v 0))))
    (is (member 'multi-message-test (available-messages sheep2) :key (lambda (v) (elt v 0))))
    (let ((sheep1-roles (sheep-direct-roles sheep1))
	  (sheep2-roles (sheep-direct-roles sheep2)))
      (is (= 0 (role-position (car sheep1-roles))))
      (is (= 1 (role-position (car sheep2-roles)))))))

(test message-redefinition
  "Confirms correct redefinition of messages"
  (defbuzzword synergize (x y))
  (defmessage synergize ((x =number=) (y =number=)) (+ x y))
  (is (= 10 (synergize 6 4)))
  (defmessage synergize ((y =number=) (x =number=)) (* x y))
  (is (= 24 (synergize 6 4))))

(test basic-message-dispatch
  "Checks that basic single-dispatch messages work."
  (let ((test-sheep (clone () () (:nickname "testie")))
	(another-sheep (clone () () (:nickname "rejected-failure"))))
    (defmessage basic-test-message ((sheep test-sheep)) (sheep-nickname sheep))
    (is (equal "testie" (basic-test-message test-sheep)))
    (signals sheeple::no-applicable-messages (basic-test-message another-sheep))))

(test before-messages
  "Checks proper dispatch of :before messages."
  (let ((test-sheep (clone () ((var "value")))))
    (defmessage get-var ((sheep test-sheep))
      (property-value sheep 'var))
    (is (equal "value" (get-var test-sheep)))
    (defmessage get-var :before ((sheep test-sheep))
		(setf (property-value sheep 'var) "new-value"))
    (is (equal "value" (property-value test-sheep 'var)))
    (is (equal "new-value" (get-var test-sheep)))
    (setf (property-value test-sheep 'var) "different-value")
    (is (equal "different-value" (property-value test-sheep 'var)))
    (is (equal "new-value" (get-var test-sheep)))))

(test after-messages
  "Checks proper dispatch of :after messages."
  (let ((test-sheep (clone () ((var "value")))))
    (defmessage get-var ((sheep test-sheep))
      (property-value sheep 'var))
    (is (equal "value" (get-var test-sheep)))
    (defmessage get-var :after ((sheep test-sheep))
		(setf (property-value sheep 'var) "new-value"))
    (is (equal "value" (get-var test-sheep)))
    (is (equal "new-value" (property-value test-sheep 'var)))
    (setf (property-value test-sheep 'var) "different-value")
    (is (equal "different-value" (get-var test-sheep)))
    (is (equal "new-value" (property-value test-sheep 'var)))))

(test around-messages
  "Checks proper dispatch of :around messages."
  (let ((test-sheep (clone () ((var "value")))))
    (defmessage get-var ((sheep test-sheep))
      (property-value sheep 'var))
    (is (equal "value" (get-var test-sheep)))
    (defmessage get-var :around ((sheep test-sheep))
		(concatenate 'string "a " (call-next-message)))
    (is (equal "a value" (get-var test-sheep)))
    (is (equal "value" (property-value test-sheep 'var)))
    (setf (property-value test-sheep 'var) "different-value")
    (is (equal "different-value" (property-value test-sheep 'var)))
    (is (equal "a different-value"  (get-var test-sheep)))
    (is (equal "different-value" (property-value test-sheep 'var)))))

(test call-next-message
  "Tests proper dispatch of next-message on a call to (call-next-message)"
  (let ((test-sheep (clone () ((var "value")))))
    (defmessage get-var (something) (property-value something 'var))
    (is (equal "value" (get-var test-sheep)))
    (defmessage get-var ((sheep test-sheep)) (call-next-message))
    (is (equal "value" (get-var test-sheep)))))

(test next-message-p
  "Checks that next-message-p returns T or NIL when appropriate."
  (let ((test-sheep (clone () ((var "value")))))
    (defmessage get-var (something) (property-value something 'var))
    (is (equal "value" (get-var test-sheep)))
    (defmessage get-var ((sheep test-sheep)) (declare (ignore sheep)) (next-message-p))
    (is (equal t (get-var test-sheep)))))

(test multimessage-dispatch
  ;; TODO
  "Checks correct multimethod dispatching."
  (defmessage foo ((foo =number=) (bar =number=))
    (+ foo bar))
  (defmessage foo (foo bar)
    foo bar)
  (is (= 5 (foo 2 3)))
  (is (equal "bar" (foo "foo" "bar")))
  (let* ((sheep1 (clone () () (:nickname "sheep1")))
	 (sheep2 (clone (sheep1) () (:nickname "sheep2")))
	 (sheep3 (clone (sheep1) () (:nickname "sheep3")))
	 (sheep4 (clone (sheep2) () (:nickname "sheep4")))
	 (sheep5 (clone (sheep4 sheep3) () (:nickname "sheep5"))))
    (defmessage foo ((foo sheep1) (bar sheep1)) 
      (declare (ignore foo bar))
      "sheep1 x2")
    (defmessage foo ((foo sheep2) (bar sheep2))
      (declare (ignore foo bar))
      "sheep2 x2")
    (is (equal "sheep1 x2" (foo sheep1 sheep1)))
    (is (equal "sheep2 x2" (foo sheep2 sheep2)))
    (is (equal "sheep1 x2" (foo sheep3 sheep3)))
    (is (equal "sheep1 x2" (foo sheep3 sheep4)))
    (is (equal "sheep1 x2" (foo sheep5 sheep1)))
    (is (equal "sheep2 x2" (foo sheep5 sheep2)))
    (defmessage foo ((foo sheep1) (bar sheep2))
      (declare (ignore foo bar))
      "sheep1,2")
    (defmessage foo ((foo sheep2) (bar sheep1))
      (declare (ignore foo bar))
      "sheep2,1")
    (is (equal "sheep1,2" (foo sheep1 sheep2)))
    (is (equal "sheep2,1" (foo sheep2 sheep1)))
    ;; I don't even know how to test the advanced dispatch stuff...
    ))

;;; TODO
;; (test full-method-combination
;;   "Checks that various method combinations work properly."
;; )
