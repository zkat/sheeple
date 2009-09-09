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

;; TODO - I'll add this stuff again once I go back to the propmop.
;;        In the meantime, #+/-sheeple will pepper the file. It's nice
;;        to be able to just get rid of the alternate implementation as
;;        soon as I go back to using metaobjects instead of just symbol names.
#+sheeple3.1
(defparameter the-std-property-form
 '(defproto =standard-property= ()
    ((name 'std-property))))
#+sheeple3.1
(defun property-name (property)
  (property-value property 'name))
#+sheeple3.1
(defun (setf property-name) (new-value property)
  (setf (property-value property 'name) new-value))

#+sheeple3.1
(define-bound-variable =standard-property=)

;;;
;;; Internals
;;;
(defvar *property-vector-initial-size* 5
  "The initial size for a sheep's property vector.")

(defvar *property-vector-grow-ratio* 5
  "The ratio by which the property vector is expanded when full.")

;; this macrolet is for quite a bit of convenience for the following functions..
(symbol-macrolet ((%properties (%sheep-direct-properties sheep)))

  (defun %create-property-vector (sheep)
    "Sets SHEEP's property vector to a (simple-vector `*property-vector-initial-size*')."
    (setf %properties (make-vector *property-vector-initial-size*)))

  (defun %property-vector-full-p (sheep)
    "SHEEP's property vector is full when all elements are non-NIL, assuming it has one."
    (aand %properties (notany 'null it)))

  (defun %enlarge-property-vector (sheep)
    "This function takes care of enlarging a sheep's property vector -- usually called when
the vector needs to make space for new property conses. The amount the vector is enlarged by
depends on the value of *property-vector-grow-ratio*. The property vector is not guaranteed to
be EQ when 'enlarged', but it's guaranteed to keep all the previous property conses."
    ;; This implementation conses up a completely new simple-vector.
    ;; Using extendable vectors forces us to use AREF (a time price), and
    ;; it also carries some extra space overhead. The case of many properties being added
    ;; en-masse to a sheep object isn't expected to be very common, so this is probably
    ;; a nice approach.
    (let* ((old-vector %properties)
           (new-vector (make-vector (* *property-vector-grow-ratio* (length old-vector)))))
      (setf %properties (replace new-vector old-vector)))
    sheep)

  (defun %get-property-cons (sheep property)
    (find property %properties :test 'eq
          :key #-sheeple3.1 'car
          #+sheeple3.1 (compose 'property-name 'car)))

  (defun %add-property-cons (sheep property value)
    "This function puts PROPERTY and VALUE into a cons cell and adds the cell to
SHEEP's property-vector if the property is unique. If it's not unique a generic error
is signaled."
    ;; Since we start off all sheep objects with their property-vector slot set to NIL,
    ;; we have to check for that case and cons up a fresh property-vector. If the vector
    ;; turns out to be full, we also have to enlarge it.
    (let ((properties %properties))
      (unless properties
        (%create-property-vector sheep)
        (setf properties %properties))
      (if (%get-property-cons sheep
                              #+sheeple3.1(property-name property)
                              #-sheeple3.1 property)
          (error "Property already exists!")
          (progn
            (when (%property-vector-full-p sheep)
              (%enlarge-property-vector sheep)
              (setf properties %properties))
            (dotimes (i (length properties))
              (unless (svref properties i)
                (return (setf (svref properties i) (cons property value))))))))
    sheep)

  (defun %remove-property-cons (sheep property)
    "Removes the actual property-cons representing PROPERTY."
    (awhen (position property %properties
                     :test 'eq
                     :key #-sheeple3.1 'car
                     #+sheeple3.1 (compose 'property-name 'car))
      (setf (svref %properties it) nil))
    sheep)

  ) ; end symbol-macrolet

(defun %direct-property-value (sheep property-name)
  (cdr (%get-property-cons sheep property-name)))
(defun (setf %direct-property-value) (new-value sheep property-name)
  (setf (cdr (%get-property-cons sheep property-name)) new-value))

#+sheeple3.1
(defun %direct-property-metaobject (sheep property-name)
  (car (%get-property-cons sheep property-name)))
#+sheeple3.1
(defun (setf %direct-property-metaobject) (new-value sheep pname)
  (setf (car (%get-property-cons sheep pname)) new-value))

;;;
;;; Existential
;;;
(defun has-direct-property-p (sheep property-name)
  "Returns T if SHEEP has a property called PROPERTY-NAME as a direct property.
NIL otherwise."
  (when (%get-property-cons sheep property-name) t))

(defun has-property-p (sheep property-name)
  "Returns T if calling PROPERTY-VALUE on SHEEP using the same property-name
would yield a value (i.e. not signal an unbound-property condition)."
  (some (rcurry 'has-direct-property-p property-name)
        (sheep-hierarchy-list sheep)))

;; TODO - The way adding/removing properties is done needs to be thought out better.
;;        Right now, doing (defsheep (something) ((var "value"))) would create that
;;        property locally, which could become problematic, specially wrt to how much
;;        space I was hoping to save by adding fresh local property metasheeple only
;;        when they're non-existent in the hierarchy-list, or when add-property is used.
;;
;;        A good part of this issue will probably be resolved when there's a clean,
;;        straightforward API for adding/removing properties. The current interface
;;        is suboptimal, needless to say. -sykopomp
(defun add-property (sheep property-name value &key reader writer accessor generate-accessor)
  "Adds a property named PROPERTY-NAME to SHEEP, initialized with VALUE."
  ;; TODO: do shit with the kwargs
  (assert (symbolp property-name))
  (when (has-direct-property-p sheep property-name)
    (cerror "Remove existing property." "~A already has a direct property named ~A."
            sheep property-name)
    (remove-property sheep property-name))
  (%add-property-cons sheep
                      #+sheeple3.1
                      (if (has-property-p sheep property-name)
                          (%direct-property-metaobject (property-owner sheep property-name)
                                                       property-name)
                          (defsheep (=standard-property=) ((name property-name))))
                      #-sheeple3.1 property-name
                      value))

;; TODO - remove-property should look at the property metaobject and remove any replies for
;;        accessors that it points to. This will have to wait until reply-undefinition works
;;        again, though. -syko
;; Addendum - this may not count anymore. I'm not sure if we'll still have the metaobjects
;;            keep track of this info
(defun remove-property (sheep property-name)
  "Removes SHEEP's direct property named PROPERTY-NAME. Signals an error if there is no such
direct property. Returns SHEEP."
  (if (has-direct-property-p sheep property-name)
      (prog1 sheep (%remove-property-cons sheep property-name))
      (error "Cannot remove property: ~A is not a direct property of ~A" property-name sheep)))

(defun remove-all-direct-properties (sheep)
  "Wipes out all direct properties and their values from SHEEP."
  (setf (%sheep-direct-properties sheep) nil))

;;; Value
(defun direct-property-value (sheep property-name)
  "Returns the property-value set locally in SHEEP for PROPERTY-NAME.
If the value is non-local (is delegated or does not exist in the hierarchy list),
a condition of type UNBOUND-DIRECT-PROPERTY condition is signalled."
  ;;  (declare (inline %sheep-direct-properties %get-property-cons)) ;commented until after testing
  (awhen (%sheep-direct-properties sheep)
    (cdr (or (%get-property-cons sheep property-name)
             (error 'unbound-property
                    :sheep sheep :property-name property-name)))))

(defun property-value (sheep property-name)
  "Returns a property-value that is not necessarily local to SHEEP."
  (property-value-with-hierarchy-list sheep property-name))

(declaim (inline property-value-with-hierarchy-list))
(defun property-value-with-hierarchy-list (sheep property-name)
  (aif (find-if (rcurry 'has-direct-property-p property-name) (sheep-hierarchy-list sheep))
       (direct-property-value it property-name)
       (error 'unbound-property :sheep sheep :property-name property-name)))

(defun (setf property-value) (new-value sheep property-name)
  "Sets NEW-VALUE as the value of a direct-property belonging to SHEEP, named
PROPERTY-NAME. If the property does not already exist anywhere in the hierarchy list, an error
is signaled."
  ;; TODO - the changes to add-property may have changed the way this should work. -sykopomp
  (cond ((has-direct-property-p sheep property-name)
         (setf (%direct-property-value sheep property-name) new-value))
        ((has-property-p sheep property-name)
         ;; We place a restriction on the user that a property metaobject
         ;; itself cannot be side-effected. That restriction allows us,
         ;; in the common case of a property already existing in the hierarchy,
         ;; to reuse the property metaobject by just adding a pointer to it locally.
         (let ((owner-prop-mo #+sheeple3.1 (car (%get-property-cons
                                                 (property-owner sheep property-name)
                                                 property-name))
                              #-sheeple3.1 property-name))
           (%add-property-cons sheep owner-prop-mo new-value)))
        (t (cerror "Add the property locally" 'unbound-property
                   :sheep sheep
                   :property-name property-name)
           (add-property sheep property-name new-value))))

;;; Reflection API
(defun property-owner (sheep property-name &optional errorp)
  "Returns the sheep object with a direct-property called PROPERTY-NAME from which SHEEP inherits
its value. If ERRORP is T, an error is signaled if the property is unbound. Otherwise, NIL is
returned."
  (or (find-if (rcurry 'has-direct-property-p property-name) (sheep-hierarchy-list sheep))
      (when errorp (error 'unbound-property :sheep sheep :property-name property-name))))

#+sheeple3.1
(defun property-metaobject-p (obj)
  (and (sheepp obj)
       (find =standard-property= (sheep-hierarchy-list obj))))

#+sheeple3.1
(defun direct-property-metaobject (sheep property-name &optional errorp)
  "Returns the direct local metaobject for a property named PROPERTY-NAME."
  (or (%direct-property-metaobject sheep property-name)
      (when errorp (error 'unbound-property :sheep sheep :property-name property-name))))

(defun sheep-direct-properties (sheep)
  #-sheeple3.1 "Returns a set of direct property definition metaobjects."
  #+sheeple3.1 "Returns a set of direct property definition metaobjects."
  (map 'list 'car (%sheep-direct-properties sheep)))

(defun available-properties (sheep)
  "Returns a list of property objects describing all properties available to SHEEP, including
inherited ones."
  #+sheeple3.1
  (mapcar (fun (direct-property-metaobject (property-owner sheep _ nil) _))
          (mapcar 'property-name
                  (delete-duplicates (append (sheep-direct-properties sheep)
                                             (mapcan #'available-properties
                                                     (sheep-parents sheep)))
                                     :key #'property-name)))
  #-sheeple3.1
  (delete-duplicates (append (sheep-direct-properties sheep)
                             (mapcan 'available-properties (sheep-parents sheep)))))

(defun property-summary (sheep &optional (stream *standard-output*))
  "Provides a pretty-printed representation of SHEEP's available properties."
  (format stream
          "~&Sheep: ~A~%Properties:~% ~{~{~&~3TName: ~13T~A~%~3TValue: ~13T~S~%~
           ~3TOwner: ~13T~A~%~%~}~}"
          sheep (mapcar (fun (let ((pname #+sheeple3.1(property-name _)
                                          #-sheeple3.1 _))
                               (list pname
                                     (property-value sheep pname)
                                     (property-owner sheep pname))))
                        (available-properties sheep))))

(defun direct-property-summary (sheep &optional stream)
  "Provides a pretty-printed representation of SHEEP's direct properties."
  (format stream
          "~&Sheep: ~A~%~
           Direct Properties: ~%~%~
           ~{~{~&~3TName: ~A~%~3TValue: ~S~%~%~}~}"
          sheep (mapcar (fun (let ((pname #+sheeple3.1 (property-name_)
                                          #-sheeple3.1 _))
                               (list pname
                                     (direct-property-value sheep pname))))
                        (sheep-direct-properties sheep))))

