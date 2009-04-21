;; Here's a translation of the bank account code from PCL, into Sheeple
;;
;; Things to note:
;; * No advanced method combination
;; * No need for class allocation (can simply keep data in prototype)
;; * No distinction between classes and objects. The =foo= scheme is only stylistic,
;;   denoting that the object is used as a prototype.
;; * Message definition is on actual objects, not on classes or 'named prototypes'.
;; * The hierarchy list for sheep works identically to CLOS's class precedence list (same sorting)
(in-package :sheeple-user)

(defvar *max-acc-num* 0)
(defparameter *minimum-balance* 500)

;; The defsheep macro is really just a wrapper around clone that handles redefinition in a
;; convenient way that fits more nicely with CLOS-style development. Defsheep should be used
;; for prototypes that are meant to be part of the text code, while clone should be used for
;; anonymous objects.
;; There is no difference between the actual objects that defsheep and clone create, or
;; the variables that they are held in (defsheep uses defparameter).
(defsheep =bank-account= ()
  ((customer-name
    "NoName"
    :manipulator customer-name
    ;; You could do :cloneform (error "Gimme a name") here,
    ;; but that would force you to provide a name for anything
    ;; that clones this -- including other prototypes.
    ;; Since we want to remain flexible, we prefer defaulting to 
    ;; a generic value ("NoName")
    )
   (balance
    0
    :manipulator balance)
   (account-number 
    (incf *max-acc-num*)    
    :reader account-number
    :cloneform (incf *max-acc-num*))
   (account-type            
    :bronze                 
    :manipulator account-type)))

;; SHEEPLE-USER> (property-value =bank-account= 'customer-name) => "NoName"
;; SHEEPLE-USER> (property-value =bank-account= 'balance) => 0

(defsheep =checking-account= (=bank-account=) ())

(defsheep =savings-account= (=bank-account=)
  ((interest-rate
    0
    :manipulator interest)))

(defsheep =proxy-account= (=bank-account=) ())

(defsheep =money-market-account= (=checking-account= =savings-account=) ())

(defvar *account-of-bank-president*
  (clone (=bank-account=)
	 ((customer-name
	   "The Prez Man")
	  (balance
	   9001))))

(defvar *janes-account*
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

(defmessage withdraw ((proxy =proxy-account=) amount)
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
