;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; indent-tabs-mode: nil -*-

;;;; This file is part of Sheeple

;;;; properties.lisp
;;;;
;;;; Property access, inspection, and management stuff, for the most part.
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :sheeple)

;;;
;;; Property spec object
;;;
(defparameter the-std-property-form '(defproto =standard-property= ()
                                      ((property-name 'std-property))))

(defvar =standard-property= (gensym "=STANDARD-PROPERTY="))

;;;
;;; API
;;;

;; (defmethod old-add-property ((sheep standard-sheep) property-name value
;;                              &key readers writers (make-accessor-p t)
;;                              (property-metaclass 'standard-property))
;;   "Allocates VALUE as one of SHEEP's direct-properties."
;;   (when (has-direct-property-p sheep property-name)
;;     (warn "~A already has a direct property named ~A. Overwriting." sheep property-name))

;;   (add-property-using-property-metaobject sheep value
;;                                           (make-instance property-metaclass
;;                                                          :name property-name)
;;                                           :readers readers :writers writers)
;;   sheep)

;; (defun add-property (sheep property-name value
;;                      &rest all-keys
;;                      &key readers writers 
;;                      make-accessor-p (property-prototype =standard-property=)
;;                      &allow-other-keys)
;;   "Adds a property named PROPERTY-NAME to SHEEP, initialized with VALUE."
;;   (assert (symbolp property-name))
;;   (when (has-direct-property-p sheep property-name)
;;     (cerror "Add property anyway."
;;             "~A already has a direct property named ~A."
;;             sheep property-name))
;;   (when make-accessor-p
;;     (pushnew property-name readers)
;;     (pushnew `(setf ,property-name) writers))
;;   (let ((property-metaobject (defclone (property-prototype) ((name property-name)))))
;;     (if (std-sheep-p sheep)
;;         (apply #'std-add-property sheep property-metaobject value all-keys)
;;         (apply #'add-property-using-metasheep
;;                (sheep-metasheep sheep) sheep
;;                property-name value all-keys))))

;; (defun add-property-metaobject-to-sheep (sheep property)
;;   (let ((pname (property-name property)))
;;     (setf (gethash pname (sheep-property-metaobject-table sheep))
;;           property)))

;; (defmethod add-property-using-property-metaobject ((sheep standard-sheep) value
;;                                                    (property standard-property)
;;                                                    &key readers writers)
;;   (let ((pname (property-name property)))
;;     (add-property-metaobject-to-sheep sheep property)
;;     (when readers
;;       (mapc (lambda (reader) (add-reader-to-sheep reader pname sheep))
;;             readers))
;;     (when writers
;;       (mapc (lambda (writer) (add-writer-to-sheep writer pname sheep))
;;             writers))
;;     (setf (property-value sheep pname) value))
;;   sheep)



;; (defun std-add-property (sheep property-name value
;;                          &key readers writers
;;                          make-accessor-p (property-prototype =standard-property=))

;;   )

;; (defun remove-property (sheep property)
;;   "If PROPERTY-NAME is a direct property of SHEEP, this function removes it. If PROPERTY-NAME
;; is inherited from one of SHEEP's parents, or if PROPERTY-NAME does not exist in SHEEP's hierarchy
;; list, an error is signaled. This function returns SHEEP after property removal."
;;   ;; todo
;;   )

;; (defun remove-all-direct-properties (sheep)
;;   "Wipes out all direct properties and their values from SHEEP."
;;   ;; todo
;;   )
(defun %get-property-cons (sheep property-name)
  (find prop (%sheep-direct-properties sh)
        :test #'eq :key (fn (property-name (car _)))))

(defun has-direct-property-p (sheep property-name)
  "Returns T if SHEEP has a property called PROPERTY-NAME as a direct property.
NIL otherwise."
  (when (%get-property-cons sheep property-name) t))

(defun has-property-p (sheep property-name)
  "Returns T if calling PROPERTY-VALUE on SHEEP using the same property-name
would yield a value (i.e. not signal an unbound-property condition)."
  (some (fn (has-direct-property-p _ property-name))
        (sheep-hierarchy-list sheep)))

;;; Value
(defun %add-property-cons (sheep property-metaobject value)
  ;; for now, we'll treat %sheep-direct-properties as a list.
  (push (cons property-metaobject value) (%sheep-direct-properties sheep)))
(defun %remove-property-cons (sheep property-name)
  (deletef (%sheep-direct-properties)
           (%get-property-cons sheep property-name)))

(defun %direct-property-value (sheep property-name)
  (cdr (%get-property-cons sheep property-name)))
(defun (setf %direct-property-value) (new-value sheep property-name)
  (setf (cdr (%get-property-cons sh prop)) new-value))

(defun direct-property-value (sheep property-name)
  "Returns the property-value set locally in SHEEP for PROPERTY-NAME.
If the value is non-local (is delegated or does not exist in the hierarchy list),
a condition of type UNBOUND-DIRECT-PROPERTY condition is signalled."
  ;;  (declare (inline %sheep-direct-properties %get-property-cons)) ;commented until after testing
  (awhen (%sheep-direct-properties sheep)
    (cdr (or (%get-property-cons sheep property-name)
             (error 'unbound-direct-property
                    :sheep sheep :property-name (property-name property))))))

(defun property-value (sheep property-name)
  "Returns a property-value that is not necessarily local to SHEEP."
  (property-value-with-hierarchy-list sheep property-name))

(defun property-value-with-hierarchy-list (sheep property-name)
  (let ((hl (sheep-hierarchy-list sheep)))
    (or (loop for sheep in hl
           do (let ((hasp (has-direct-property-p sheep property-name)))
                (when hasp
                  (return-from property-value-with-hierarchy-list
                    (direct-property-value sheep property-name)))))
        (error 'unbound-property :sheep sheep :property-name property-name))))

(defun (setf property-value) (new-value sheep property-name)
  "Sets NEW-VALUE as the value of a direct-property belonging to SHEEP, named
PROPERTY-NAME. If the property does not already exist anywhere in the hierarchy list, an error
is signaled."
  ;; If the property exists locally already, simply set the new value as the CDR of the property
  ;; cons. If it doesn't, we first need to check if there's an ancestor that has that property
  ;; available. If there is, we create a new property cons object with the CAR being a pointer
  ;; to that ancestor's property's metaobject. The CDR should be set to the local value.
  ;; If there *isn't* an ancestor that already has the property, a continuable error should be
  ;; signaled that gives the user the option of creating the property locally with default
  ;; settings.
  (cond ((has-direct-property-p sheep property-name)
         (setf (%direct-property-value sheep property-name new-value)))
        ((has-property-p sheep property-name)
         (let ((owner-prop-mo (car (%get-property-cons (property-owner sheep property-name)
                                                       property-name))))
           (setf )
           ))))


;; ;;; Reflection API
;; (defgeneric property-owner (sheep property-name &optional errorp)
;;   (:documentation "Returns the sheep object with a direct-property called PROPERTY-NAME from which
;; SHEEP inherits its value. If ERRORP is T, an error is signaled if the property is unbound. Otherwise,
;; NIL is returned."))

;; (defgeneric available-properties (sheep)
;;   (:documentation "Returns a list of property objects describing all properties available to
;; SHEEP, including inherited ones."))

;; (defgeneric direct-property-metaobject (sheep property-name)
;;   (:documentation "Returns the direct local metaobject for a property named PROPERTY-NAME."))

;; (defgeneric property-summary (sheep &optional stream)
;;   (:documentation "Provides a pretty-printed representation of SHEEP's available properties."))

;; (defgeneric direct-property-summary (sheep &optional stream)
;;   (:documentation "Provides a pretty-printed representation of SHEEP's direct properties."))

;;;
;;; Methods
;;;

;;; Existential



;; ;; TODO - remove-property should look at the property metaobject and remove any replies for
;; ;;        accessors that it points to. This will have to wait until reply-undefinition works
;; ;;        again, though. -syko
;; (defmethod remove-property ((sheep standard-sheep) (property-name symbol))
;;   (if property-name
;;       (remove-property sheep (gethash property-name (sheep-property-metaobject-table sheep)))
;;       (error "Cannot remove property: ~A is not a direct property of ~A" property-name sheep)))
;; (defmethod remove-property ((sheep standard-sheep) (property standard-property))
;;   (let ((name (property-name property)))
;;     (if (has-direct-property-p sheep name)
;;         (progn (remhash name (sheep-property-value-table sheep))
;;                (remhash name (sheep-property-metaobject-table sheep))
;;                sheep)
;;         (error "Cannot remove property: ~A is not a direct property of ~A" name sheep))))

;; (defmethod remove-all-direct-properties ((sheep standard-sheep))
;;   (clrhash (sheep-property-value-table sheep))
;;   (clrhash (sheep-property-metaobject-table sheep))
;;   sheep)

;; (defmethod has-direct-property-p ((sheep standard-sheep) property-name)
;;   (nth-value 1 (gethash property-name (sheep-property-metaobject-table sheep))))

;; (defmethod has-property-p ((sheep standard-sheep) property-name)
;;   "Returns T if a property with PROPERTY-NAME is available to SHEEP."
;;   (if (member-if (lambda (x)
;;                    (has-direct-property-p x property-name))
;;                  (sheep-hierarchy-list sheep))
;;       t nil))

;; ;;; values
;; (defmethod direct-property-value ((sheep standard-sheep) (property-name symbol))
;;   (let ((property (gethash property-name (sheep-property-metaobject-table sheep))))
;;     (if property
;;         (direct-property-value sheep property)
;;         (error 'unbound-property :sheep sheep :property-name property-name))))
;; (defmethod direct-property-value ((sheep standard-sheep) (property standard-property))
;;   (multiple-value-bind (value hasp)
;;       (gethash (property-name property) (sheep-property-value-table sheep))
;;     (if hasp
;;         value
;;         (error 'unbound-direct-property :sheep sheep
;;                :property-name (property-name property)))))

;; (defmethod property-value ((sheep standard-sheep) property-name)
;;   (property-value-with-hierarchy-list sheep property-name))
;; (defun property-value-with-hierarchy-list (sheep property-name)
;;   (let ((hl (sheep-hierarchy-list sheep)))
;;     (or (loop for sheep in hl
;;            do (let ((hasp (has-direct-property-p sheep property-name)))
;;                 (when hasp
;;                   (return-from property-value-with-hierarchy-list
;;                     (direct-property-value sheep property-name)))))
;;         (error 'unbound-property :sheep sheep :property-name property-name))))

;; ;; What the following code SHOULD do:
;; ;; 1. If there is no ancestor with a property added with that name, error.
;; ;; 2. If there is no property set locally, but an ancestor has one,
;; ;;    use the ancestor's property's class to make a new instance of the
;; ;;    property-spec locally.
;; ;; 3. set the actual property-value locally
;; ;;
;; ;; The general idea is that all children of a certain object will inherit the property's
;; ;; class when creating it locally, although they can always override the property type
;; ;; by using add-property. There should be a way to override this in the MOP, though.
;; (defmethod (setf property-value) (new-value (sheep standard-sheep) (property-name symbol))
;;   (if (has-property-p sheep property-name)
;;       (let ((property (direct-property-metaobject (property-owner sheep property-name)
;;                                             property-name)))
;;         (setf (property-value sheep property) new-value))
;;       (error "Property ~A does not exist for sheep ~A." property-name sheep)))
;; (defmethod (setf property-value) :before (new-value (sheep standard-sheep)
;;                                                     (property standard-property))
;;   (declare (ignorable new-value))
;;   (unless (direct-property-metaobject sheep (property-name property))
;;     ;; TODO - Something like add-property here instead of this crap. It
;;     ;;        should be possible to override the exact behavior exhibited here..
;;     ;;        Maybe..
;;     (setf (gethash (property-name property)
;;                    (sheep-property-metaobject-table sheep))
;;           (make-instance (class-of property)
;;                          :name (property-name property)))))
;; (defmethod (setf property-value) (new-value (sheep standard-sheep) (property standard-property))
;;   (let ((property-table (sheep-property-value-table sheep)))
;;     (setf (gethash (property-name property) property-table) new-value)))

;; ;; Reflection
;; (defun sheep-direct-properties (sheep)
;;   "Returns a set of direct property definition metaobjects."
;;   (loop for pname being the hash-keys of (sheep-property-value-table sheep)
;;      collect (direct-property-metaobject sheep pname)))

;; (defmethod direct-property-metaobject ((sheep standard-sheep) property-name)
;;   (unless (has-direct-property-p sheep property-name)
;;     (error 'unbound-direct-property :sheep sheep :property-name property-name))
;;   (nth-value 0 (gethash property-name (sheep-property-metaobject-table sheep))))

;; (defmethod property-owner ((sheep standard-sheep) property-name &optional (errorp t))
;;   (let ((owner
;;          (loop for obj in (sheep-hierarchy-list sheep)
;;             when (has-direct-property-p obj property-name)
;;             return obj)))
;;     (or owner
;;         (if errorp
;;             (error 'unbound-property :sheep sheep :property-name property-name)
;;             nil))))

;; (defmethod available-properties ((sheep standard-sheep))
;;   (let* ((direct-properties (sheep-direct-properties sheep))
;;          (avail-property-names (mapcar (lambda (p)
;;                                          (property-name p))
;;                                        (remove-duplicates
;;                                         (flatten
;;                                          (append direct-properties
;;                                                  (mapcar #'available-properties
;;                                                          (sheep-parents sheep))))
;;                                         :key #'property-name))))
;;     (mapcar (lambda (pname)
;;               (direct-property-metaobject (property-owner sheep pname nil) pname))
;;             avail-property-names)))

;; (defmethod property-summary ((sheep standard-sheep) &optional (stream *standard-output*))
;;   (let ((all-properties (available-properties sheep)))
;;     (format stream
;;             "~&Sheep: ~A~%Properties:~% ~{~{~&~3TName: ~13T~A~%~3TValue: ~13T~S~%~
;;              ~3TReaders: ~13T~A~%~3TWriters: ~13T~A~%~
;;              ~3TOwner: ~13T~A~%~%~}~}"
;;             sheep (loop for property in all-properties
;;                      collect (list (property-name property)
;;                                    (property-value sheep (property-name property))
;;                                    (property-readers property)
;;                                    (property-writers property)
;;                                    (property-owner sheep (property-name property)))))))

;; (defmethod direct-property-summary ((sheep standard-sheep) &optional (stream *standard-output*))
;;   (format stream
;;           "~&Sheep: ~A~%~
;;            Direct Properties: ~%~%~
;;            ~{~{~&~3TName: ~A~%~3TValue: ~S~%~
;;            ~3TReaders: ~A~%~3TWriters: ~A~%~%~}~}"
;;           sheep (loop for property in (sheep-direct-properties sheep)
;;                    collect (list (property-name property)
;;                                  (direct-property-value sheep property)
;;                                  (property-readers property)
;;                                  (property-writers property)))))
