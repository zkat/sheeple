;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; indent-tabs-mode: nil -*-

;; Here's a translation of the bank account code from PCL, into Sheeple
;;
;; Things to note:
;; * No advanced method combination
;; * No need for class allocation (can simply keep data in prototype)
;; * No distinction between classes and objects. The #@foo scheme is only for convenience
;;   denoting that the object is used as a prototype, stored in a different namespace.
;; * No equivalent of :INITFORM. All properties start off 'initialized' with whatever value the
;;   nearest parent set. Getting :INITFORM-like behavior requires writing replies for the
;;   INITIALIZE-SHEEP message.
;; * Message definition is on actual objects, not on classes. #@foo fetches an actual object.
;; * The hierarchy list for sheep works identically to CLOS's class precedence list (same sorting)
(in-package :sheeple-user)

(defvar *max-acc-num* 0)
(defparameter *minimum-balance* 500)
;; The defproto macro is really just a wrapper around sheep creation that handles redefinition in a
;; convenient way that fits more nicely with Lisp-style development. DEFPROTO should be used
;; for prototypes that are meant to be part of the text code, while clone should be used for
;; anonymous objects.
;; There is no difference between the actual objects that defproto and clone create,
;; named prototypes are simply stored in a different area.
(defproto bank-account ()
  ((customer-name "NoName")
   (balance 0)
   (account-number (incf *max-acc-num*)
                   :reader account-number)
   (account-type :bronze)))

(defreply initialize-sheep :after ((account #@bank-account) &key)
  (setf (property-value account 'account-number) (incf *max-acc-num*)))

;; SHEEPLE-USER> (property-value #@bank-account 'customer-name) => "NoName"
;; SHEEPLE-USER> (account-number #@bank-account) => 0

(defproto #@checking-account (#@bank-account) ())

(defproto #@savings-account (#@bank-account)
  ((interest-rate 0)))

(defproto #@proxy-account (#@bank-account) ())

(defproto @#money-market-account (#@checking-account #@savings-account) ())

;; defclone provides similar facilities to defproto, but without automatic accessor generation,
;; and the objects remain anonymous.
(defvar *account-of-bank-president*
  (defclone (#@bank-account)
      ((customer-name
        "The Prez Man")
       (balance
        9001))))

(defvar *janes-account*
  (defclone (#@bank-account)
      ((customer-name
        "Jane Doe")
       (balance
        1000))))

;; CLONE is the most minimalist of the 3 creation methods: it only accepts a list of prototypes.
;; The two advantages of using CLONE are that A. it's a function, you can apply it. B. It's simple.
(defvar *lots-of-accounts*
  (loop for i upto 1000
     collect (clone #@bank-account)))

;; you can add or remove direct properties from an object at any time, too
(add-property (car *lots-of-accounts*) 'unique "I'm a unique butterfly!") ;auto-generates accessors.
(remove-property (car *lots-of-accounts*) 'unique) ;poof. Accessors stay, though!

;; You can also inspect objects and their internals at the REPL...
;;
;; SHEEPLE-USER> (property-summary *janes-account*)
;; Sheep: #<Sheep #x1506134E>
;; Properties:

;;    Name:     CUSTOMER-NAME
;;    Value:    "Jane Doe"
;;    Readers:  (CUSTOMER-NAME)
;;    Writers:  ((SETF CUSTOMER-NAME))
;;    Owner:    #<Sheep #x1506134E>

;;    Name:     BALANCE
;;    Value:    1000
;;    Readers:  (BALANCE)
;;    Writers:  ((SETF BALANCE))
;;    Owner:    #<Sheep #x1506134E>

;;    Name:     CUSTOMER-NAME
;;    Value:    "NoName"
;;    Readers:  (CUSTOMER-NAME)
;;    Writers:  ((SETF CUSTOMER-NAME))
;;    Owner:    #<Sheep #x1506134E>

;;    Name:     ACCOUNT-TYPE
;;    Value:    :BRONZE
;;    Readers:  (ACCOUNT-TYPE)
;;    Writers:  ((SETF ACCOUNT-TYPE))
;;    Owner:    #<Sheep AKA: BANK-ACCOUNT #x15044BBE>

;;    Name:     ACCOUNT-NUMBER
;;    Value:    2
;;    Readers:  NIL ;; This only shows *direct* readers.
;;    Writers:  NIL
;;    Owner:    #<Sheep #x1506134E>

;;    Name:     BALANCE
;;    Value:    0
;;    Readers:  (BALANCE)
;;    Writers:  ((SETF BALANCE))
;;    Owner:    #<Sheep #x1506134E>

;; NIL
;; SHEEPLE-USER>

(defmessage withdraw (account amount)
  (:documentation "Withdraw the specified amount from the account.
Signal an error if the current balance is less than AMOUNT."))
(defreply withdraw ((account #@bank-account) amount)
  (when (< (balance account) amount)
    (error "Account overdrawn."))
  (decf (balance account) amount))

(defreply withdraw :before ((account #@checking-account) amount)
  (let ((overdraft (- amount (balance account))))
    (when (plusp overdraft)
      (withdraw (overdraft-account account) overdraft)
      (incf (balance account) overdraft))))

(defreply withdraw ((proxy #@proxy-account) amount)
  (withdraw (proxied-account proxy) amount))

(defreply withdraw :before ((account *account-of-bank-president*) amount)
  (let ((overdraft (- amount (balance account))))
    (when (plusp overdraft)
      (incf (balance account) (embezzle *bank* overdraft)))))

(defreply assess-low-balance-penalty ((account #@bank-account))
  (with-properties ((bal balance)) account
    (when (< bal *minimum-balance*)
      (decf bal (* bal .01)))))

(defreply merge-accounts ((acc1 #@bank-account) (acc2 @#bank-account))
  (with-manipulators ((balance1 balance)) acc1
    (with-manipulators ((balance2 balance)) acc2
      (incf balance1 balance2)
      (setf balance2 0))))
