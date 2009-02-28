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
	  (phone "543-867-5309"))))

(defbuzzword greet (person)
  (:documentation "Greets a person"))
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
;; * No need for class allocation (can simply keep data in prototype)
;; * with-properties and with-manipulators macros have not been written yet.
;; * No distinction between classes and objects. The =foo= scheme is only stylistic,
;;   denoting that the object is used as a prototype.
;; * Message definition is on actual objects, not on classes or 'named prototypes'.
;; * The hierarchy list for sheep works identically to CLOS's class precedence list (same sorting)
;; * Since Sheeple's messages can dispatch on arg lists of variable length, there is no
;;   readily available information about what arguments a buzzword expects.

(defvar *max-acc-num* 0)
(defparameter *minimum-balance* 500)

(defvar =bank-account=
  (clone ()
	 ((customer-name
	   "NoName"
	   :manipulator customer-name
	   :cloneform (error "Must supply a customer name."))
	  (balance
	   0
	   :manipulator balance)
	  (account-number 
	   (incf *max-acc-num*)    
	   :reader account-number
	   :cloneform (incf *max-acc-num*))
	  (account-type            
	   :bronze                 
	   :manipulator account-type))))

;; SHEEPLE-USER> (property-value =bank-account= 'customer-name) => "NoName"
;; SHEEPLE-USER> (property-value =bank-account= 'balance) => 0

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
	   9001))))

(defvar *account*
  (clone (=bank-account=)
	 ((customer-name
	   "Jane Doe")
	  (balance
	   1000))))

(defbuzzword withdraw (account amount)
  (:documentation "Withdraw the specified amount from the account.
Signal an error if the current balance is less than AMOUNT."))
(defmessage withdraw ((account =bank-account=) amount)
  (when (< (balance account) amount)
    (error "Account overdrawn."))
  (decf (balance account) amount))

(defmessage withdraw :before ((account =checking-account=) amount)
  (let ((overdraft (- amount (balance account))))
    (when (plusp overdraft)
      (withdraw (overdraft-account account) overdraft)
      (incf (balance account) overdraft))))

(defmessage withdraw ((account =proxy-account=) amount)
  (withdraw (proxied-account proxy) amount))

(defmessage withdraw :before ((account *account-of-bank-president*) amount)
  ;; Note a big difference here between this and its CLOS equivalent:
  ;; It's easy here to simply start cloning *account-of-bank-president* and make
  ;; several different accounts, based on that. This method will apply to all of them.
  ;; The EQL-specializer version simply "bottoms out", and a change like this to the code
  ;; would involve redefinition of this method, reinitialization of the instance, and a
  ;; change to the class hierarchy.
  (let ((overdraft (- amount (balance account))))
    (when (plusp overdraft)
      (incf (balance account) (embezzle *bank* overdraft)))))

(defmessage assess-low-balance-penalty ((account =bank-account=))
  (with-properties ((bal balance)) account
    (when (< bal *minimum-balance*)
      (decf bal (* bal .01)))))

(defmessage merge-accounts ((acc1 =bank-account=) (acc2 =bank-account=))
  (with-manipulators ((balance1 balance)) acc1
    (with-manipulators ((balance2 balance)) acc2
      (incf balance1 balance2)
      (setf balance2 0))))

