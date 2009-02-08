;; This is a demonstration of Sheeple usage.

;; Sheeple can be used, when necessary, in a style identical to class-based OOP.
;; The main purpose of this demo is not to demonstrate the advantages of using Sheeple
;; over CLOS, but simply its ability to perform tasks in a similar way to CLOS.
(in-package :sheeple-user)

(defvar =person= 
  (clone ()
	 ((name 
	   "NoName"
	   :manipulator name)
	  (phone
	   "NoPhone"
	   :manipulator phone))))

(defvar *charles*
  (clone (=person=)
	 ((name "Charles"))))

(defvar *jenny*
  (clone (=person=)
	 ((name "Jenny")
	  (phone "543-543-5533"))))

(defbuzzword greet "Greets a person")
(defmessage greet ((person =person=))
  (format t "Hello, ~a" (name person)))
(defmessage greet ((person *jenny*))
  (format t "Hullo, ~a!!!" (name person)))

;; SHEEPLE-USER> (name =person=)
;; "NoName"
;; SHEEPLE-USER> (name *charles*)
;; "Charles"
;; SHEEPLE-USER> (greet *charles*)
;; Hello, Charles
;; NIL
;; SHEEPLE-USER> (greet *jenny*)
;; Hullo, Jenny!!!
;; NIL
;; SHEEPLE-USER> (phone *charles*)
;; "NoPhone"
;; SHEEPLE-USER> (phone *jenny*)
;; "543-543-5533"
;; SHEEPLE-USER> (setf (phone *charles*) "555-555-5555")
;; "555-555-5555"
;; SHEEPLE-USER> (phone =person=)
;; "NoPhone"

;; Here's a translation of the bank account code from PCL, into Sheeple
;;
;; Things to note:
;; * No advanced method combination
;; * Currently no :initform equivalent (planned)
;; * No need for class allocation (can simply keep data in prototype)
;; * with-properties and with-manipulators macros have not been written yet.
;; * No distinction between classes and objects. The =foo= scheme is only stylistic
;;   sugar denoting that the object is used more as a prototype.
;; * Message definition is on actual objects, not on classes or 'named prototypes'
;; * The hierarchy list for sheep works identically to CLOS's class precedence list (same sorting)

(defvar *max-acc-num* 0)
(defparameter *minimum-balance* 500)

(defvar =bank-account=
  (clone ()
	 ((customer-name
	   "NoName"
	   :manipulator customer-name)
	  (balance
	   0
	   :manipulator balance)
	  (account-number 
	   (incf *max-acc-num*)    ; :initform-like behavior can be achieved by defining messages.
	   :reader account-number) ; I'm working on including auto-generation of the message with
	  (account-type            ; a :cloneform option. Being a regular message, it can be changed
	   :bronze                 ; or removed completely later.
	   :manipulator type))))

(defvar =checking-account=
  (clone (=bank-account=)
	 ()))

(defvar =savings-account=
  (clone (=bank-account=)
	 ((interest-rate
	   0
	   :manipulator interest))))

(defvar =proxy-account=
  (clone (=bank-account=) ()))

(defvar =money-market-account=
  (clone (=checking-account= =savings-account=)
	 ()))

(defvar *account-of-bank-president*
  (clone (=bank-account=)
	 ((customer-name
	   "The Prez Man")
	  (balance
	   9001)
	  (account-number
	   (incf *max-acc-num*)))))

(defvar *account*
  (clone (=bank-acount=)
	 ((customer-name
	   "Jane Doe")
	  (balance
	   1000)
	  (account-number
	   (incf *max-acc-num*))))) ; note the annoying repetition. 
                                    ; Again, a :cloneform option will solve it.

(defbuzzword withdraw "Withdraw the specified amount from the account.
Signal an error if the current balance is less than AMOUNT.")
(defmessage withdraw ((account =bank-account=) amount)
  (when (< (balance account) amount)
    (error "Account overdrawn."))
  (decf (balance account) amount))

(defmessage withdraw :before ((account =checking-account=) amount)
	    (let ((overdraft (- amount (balance-account))))
	      (when (plusp overdraft)
		(withdraw (overdraft-account account) overdraft)
		(incf (balance account) overdraft))))

(defmessage withdraw ((account =proxy-account=) amount)
  (withdraw (proxied-account proxy) amount))

(defmessage withdraw :before ((account *account-of-bank-president*) amount)
  (let ((overdraft (- amount (balance account))))
    (when (plusp overdraft)
      (incf (balance account) (embezzle *bank* overdraft)))))

;; This is written in PCL using the with-slots macro.
(defmessage asses-low-balance-penalty ((account =bank-account=))
  (when (< (balance account) *minimum-balance*)
    (decf (balance account) (* (balance account) .01))))