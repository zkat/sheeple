;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; indent-tabs-mode: nil -*-

;; Here's a translation of the bank account code from PCL, into Sheeple
;;
;; Things to note:
;; * No advanced method combination
;; * No need for class allocation (can simply keep data in prototype)
;; * No real distinction between classes and objects.
;; * No equivalent of :INITFORM. All properties start off 'initialized' with whatever value the
;;   nearest parent set. Getting :INITFORM-like behavior requires writing replies for the
;;   INIT-SHEEP message.
;; * Message definition is on actual objects, not on classes.
;; * The hierarchy list for sheep works identically to CLOS's class precedence list (same sorting)
(in-package :sheeple-user)

(defvar *max-acc-num* 0)
(defparameter *minimum-balance* 500)
;; The defproto macro is really just a wrapper around sheep creation that handles redefinition in a
;; convenient way that fits more nicely with Lisp-style development. DEFPROTO should be used
;; for prototypes that are meant to be part of the text code, while clone should be used for
;; anonymous objects.
;; There is no difference between the actual objects that DEFPROTO and SPAWN create,
;; DEFPROTO is purely convenience for defining code more concisely and precisely,
;; and for interactive development.
(defproto =bank-account= ()
  ((customer-name "NoName")
   (balance 0)
   (account-number 0 :reader 'account-number)
   (account-type :bronze)))

(defreply init-object :after ((account =bank-account=) &key)
  (setf (property-value account 'account-number) (incf *max-acc-num*)))

;; SHEEPLE-USER> (property-value =bank-account= 'customer-name) => "NoName"
;; SHEEPLE-USER> (account-number =bank-account=) => 0

(defproto =checking-account= =bank-account=)

(defproto =savings-account= =bank-account=
  ((interest-rate 0)))

(defproto =proxy-account= =bank-account=)

(defproto =money-market-account= (=checking-account= =savings-account=))

;; defobject provides similar facilities to defproto, but without automatic accessor generation,
;; and the objects remain anonymous.
(defvar *account-of-bank-president*
  (create =bank-account=
          'customer-name "The Prez Man"
          'balance       9001))

(defvar *janes-account*
  (create =bank-account=
          'customer-name "Jane Doe"
          'balance       1000))

(defvar *lots-of-accounts*
  (loop repeat 1000 collect (create =bank-account=)))

;; you can add or remove direct properties from an object at any time, too...

;; this auto-generates accessors:
(setf (property-value (car *lots-of-accounts*) 'unique :accessor t) "I'm a unique butterfly!")
(remove-property (car *lots-of-accounts*) 'unique) ;poof. Accessors stay, though!

;; You can also inspect objects and their internals at the REPL...
;;
;; SHEEPLE-USER> (describe *janes-account*)
;; Object: #<Object [=BANK-ACCOUNT=] #x30004140808D>
;; Parents: (#<Object =BANK-ACCOUNT= #x3000413AFF7D>)
;; Properties:
;; ACCOUNT-TYPE: :BRONZE (Delegated to: #<Object =BANK-ACCOUNT= #x3000413AFF7D>)
;; ACCOUNT-NUMBER: 6
;; BALANCE: 1000
;; CUSTOMER-NAME: "Jane Doe"
;; NICKNAME: =BANK-ACCOUNT= (Delegated to: #<Object =BANK-ACCOUNT= #x3000413AFF7D>)

;; the [=foo=] name means that the object is inheriting the :nickname property, whereas
;; the same without [] means the object's nickname is set locally.

(defmessage withdraw (account amount)
  (:documentation "Withdraw the specified amount from the account.
Signal an error if the current balance is less than AMOUNT.")
  (:reply ((account =bank-account=) amount)
    (when (< (balance account) amount)
      (error "Account overdrawn."))
    (decf (balance account) amount))
  (:reply :before ((account =checking-account=) amount)
    (let ((overdraft (- amount (balance account))))
      (when (plusp overdraft)
        (withdraw (overdraft-account account) overdraft)
        (incf (balance account) overdraft))))
  (:reply ((proxy =proxy-account=) amount)
    (withdraw (proxied-account proxy) amount))
  (:reply :before ((account *account-of-bank-president*) amount
                   &aux (overdraft (- amount (balance account))))
    (when (plusp overdraft)
      (incf (balance account) (embezzle *bank* overdraft)))))

(defreply assess-low-balance-penalty ((account =bank-account=))
  (with-properties ((bal balance)) account
    (when (< bal *minimum-balance*)
      (decf bal (* bal .01)))))

(defreply merge-accounts ((acc1 =bank-account=) (acc2 =bank-account=))
  ;; with-accessors works, of course :)
  (with-accessors ((balance1 balance)) acc1
    (with-accessors ((balance2 balance)) acc2
      (incf balance1 balance2)
      (setf balance2 0))))
