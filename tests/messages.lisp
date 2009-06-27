;; This file is part of Sheeple

;; tests/messages.lisp
;;
;; Unit tests for src/messages.lisp
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :sheeple)

(def-suite message-tests :in sheeple)
(in-suite message-tests)

(test message-definition
  "Checks basic message definition, confirms that FIND-MESSAGE 
errors when a message doesn't exist."
  (defmessage message-definition-test (x) (:documentation "This is a test"))
  (is (message-p (find-message 'message-definition-test)))
  (signals no-such-message (find-message 'another-message-test))
  (defun clobbering-definition-test () (print "durr hurr"))
  (signals sheeple::clobbering-function-definition (defmessage clobbering-definition-test (foo)
						     (:documentation "OHNOES"))))

(test reply-undefinition
  "Tests undefreply macro."
  (defmessage foo (x y))
  (defreply foo (foo bar) foo)
  (is (equal =dolly= (foo =dolly= =dolly=)))
  (undefreply foo (foo bar))
  (signals sheeple::no-applicable-replies (foo =dolly= =dolly=))
  (defreply foo ((string =string=) (another =string=)) (concatenate 'string string another))
  (is (equal "fullstring" (foo "full" "string")))
  (undefreply foo ((x =string=) (y =string=)))
  (signals sheeple::no-applicable-replies (foo "full" "string")))

(test basic-reply-definition
  "Checks that replies are defined properly, and added to their respective objects.
also, checks that a style-warning is signaled if there is no message defined."
  (signals style-warning (defreply totally-new-test-reply (foo) (print foo)))
  (defmessage totally-new-test-reply (x))
  (defreply totally-new-test-reply (foo) (print foo))
  (is (message-p (find-message 'totally-new-test-reply)))
  (is (member 'totally-new-test-reply (available-replies =dolly=) :key (lambda (v) (elt v 0))))
  (is (reply-p (car (message-replies (find-message 'totally-new-test-reply))))))

(test multireply-definition
  "Checks that multireplies are defined correctly, with roles added
to their respective participants, with correct role-indexes, etc."
  (defreply multi-reply-test (foo bar) bar foo)
  (let ((sheep1 (clone () () (:nickname "sheep1")))
	(sheep2 (clone () () (:nickname "sheep2"))))
    (defreply multi-reply-test ((foo sheep1) (bar sheep2)) foo bar)
    (is (eql t (participant-p sheep1 'multi-reply-test)))
    (is (eql t (participant-p sheep2 'multi-reply-test)))
    (is (member 'multi-reply-test (available-replies sheep1) :key (lambda (v) (elt v 0))))
    (is (member 'multi-reply-test (available-replies sheep2) :key (lambda (v) (elt v 0))))
    (let ((sheep1-roles (sheep-direct-roles sheep1))
	  (sheep2-roles (sheep-direct-roles sheep2)))
      (is (= 0 (role-position (car sheep1-roles))))
      (is (= 1 (role-position (car sheep2-roles)))))))

(test reply-redefinition
  "Confirms correct redefinition of replies"
  (defmessage synergize (x y))
  (defreply synergize ((x =number=) (y =number=)) (+ x y))
  (is (= 10 (synergize 6 4)))
  (defreply synergize ((y =number=) (x =number=)) (* x y))
  (is (= 24 (synergize 6 4))))

(test basic-reply-dispatch
  "Checks that basic single-dispatch replies work."
  (let ((test-sheep (clone () () (:nickname "testie")))
	(another-sheep (clone () () (:nickname "rejected-failure"))))
    (defreply basic-test-reply ((sheep test-sheep)) (sheep-nickname sheep))
    (is (equal "testie" (basic-test-reply test-sheep)))
    (signals sheeple::no-applicable-replies (basic-test-reply another-sheep))))

(test before-replies
  "Checks proper dispatch of :before replies."
  (let ((test-sheep (clone () ((var "value")))))
    (defreply get-var ((sheep test-sheep))
      (property-value sheep 'var))
    (is (equal "value" (get-var test-sheep)))
    (defreply get-var :before ((sheep test-sheep))
		(setf (property-value sheep 'var) "new-value"))
    (is (equal "value" (property-value test-sheep 'var)))
    (is (equal "new-value" (get-var test-sheep)))
    (setf (property-value test-sheep 'var) "different-value")
    (is (equal "different-value" (property-value test-sheep 'var)))
    (is (equal "new-value" (get-var test-sheep)))))

(test after-replies
  "Checks proper dispatch of :after replies."
  (let ((test-sheep (clone () ((var "value")))))
    (defreply get-var ((sheep test-sheep))
      (property-value sheep 'var))
    (is (equal "value" (get-var test-sheep)))
    (defreply get-var :after ((sheep test-sheep))
		(setf (property-value sheep 'var) "new-value"))
    (is (equal "value" (get-var test-sheep)))
    (is (equal "new-value" (property-value test-sheep 'var)))
    (setf (property-value test-sheep 'var) "different-value")
    (is (equal "different-value" (get-var test-sheep)))
    (is (equal "new-value" (property-value test-sheep 'var)))))

(test around-replies
  "Checks proper dispatch of :around replies."
  (let ((test-sheep (clone () ((var "value")))))
    (defreply get-var ((sheep test-sheep))
      (property-value sheep 'var))
    (is (equal "value" (get-var test-sheep)))
    (defreply get-var :around ((sheep test-sheep))
		(concatenate 'string "a " (call-next-reply)))
    (is (equal "a value" (get-var test-sheep)))
    (is (equal "value" (property-value test-sheep 'var)))
    (setf (property-value test-sheep 'var) "different-value")
    (is (equal "different-value" (property-value test-sheep 'var)))
    (is (equal "a different-value"  (get-var test-sheep)))
    (is (equal "different-value" (property-value test-sheep 'var)))))

#+nil(test general-reply-combination
  "Checks combinations of :before, :after, and :around reply dispatching."
  (let ((test-sheep (clone () ((var "value")))))
    (defreply get-var ((sheep test-sheep))
      )))

(test call-next-reply
  "Tests proper dispatch of next-reply on a call to (call-next-reply)"
  (let ((test-sheep (clone () ((var "value")))))
    (defreply get-var (something) (property-value something 'var))
    (is (equal "value" (get-var test-sheep)))
    (defreply get-var ((sheep test-sheep)) (call-next-reply))
    (is (equal "value" (get-var test-sheep)))))

(test next-reply-p
  "Checks that next-reply-p returns T or NIL when appropriate."
  (let ((test-sheep (clone () ((var "value")))))
    (defreply get-var (something) (property-value something 'var))
    (is (equal "value" (get-var test-sheep)))
    (defreply get-var ((sheep test-sheep)) (declare (ignore sheep)) (next-reply-p))
    (is (equal t (get-var test-sheep)))))

(test multireply-dispatch
  ;; TODO
  "Checks correct multimethod dispatching."
  (defreply foo ((foo =number=) (bar =number=))
    (+ foo bar))
  (defreply foo (foo bar)
    foo bar)
  (is (= 5 (foo 2 3)))
  (is (equal "bar" (foo "foo" "bar")))
  (let* ((sheep1 (clone () () (:nickname "sheep1")))
	 (sheep2 (clone (sheep1) () (:nickname "sheep2")))
	 (sheep3 (clone (sheep1) () (:nickname "sheep3")))
	 (sheep4 (clone (sheep2) () (:nickname "sheep4")))
	 (sheep5 (clone (sheep4 sheep3) () (:nickname "sheep5"))))
    (defreply foo ((foo sheep1) (bar sheep1)) 
      (declare (ignore foo bar))
      "sheep1 x2")
    (defreply foo ((foo sheep2) (bar sheep2))
      (declare (ignore foo bar))
      "sheep2 x2")
    (is (equal "sheep1 x2" (foo sheep1 sheep1)))
    (is (equal "sheep2 x2" (foo sheep2 sheep2)))
    (is (equal "sheep1 x2" (foo sheep3 sheep3)))
    (is (equal "sheep1 x2" (foo sheep3 sheep4)))
    (is (equal "sheep1 x2" (foo sheep5 sheep1)))
    (is (equal "sheep2 x2" (foo sheep5 sheep2)))
    (defreply foo ((foo sheep1) (bar sheep2))
      (declare (ignore foo bar))
      "sheep1,2")
    (defreply foo ((foo sheep2) (bar sheep1))
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
