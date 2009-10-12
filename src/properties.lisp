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
  "The initial size for a object's property vector.")

(defvar *property-vector-grow-ratio* 5
  "The ratio by which the property vector is expanded when full.")

;; this macrolet is for quite a bit of convenience for the following functions..
(symbol-macrolet ((%properties (%object-properties object)))

  (defun %create-property-vector (object)
    "Sets OBJECT's property vector to a (simple-vector `*property-vector-initial-size*')."
    (setf %properties (make-vector *property-vector-initial-size*)))

  (defun %property-vector-full-p (object)
    "OBJECT's property vector is full when all elements are non-NIL, assuming it has one."
    (aand %properties (notany 'null it)))

  (defun %enlarge-property-vector (object)
    "This function takes care of enlarging a object's property vector -- usually called when
the vector needs to make space for new property conses. The amount the vector is enlarged by
depends on the value of *property-vector-grow-ratio*. The property vector is not guaranteed to
be EQ when 'enlarged', but it's guaranteed to keep all the previous property conses."
    ;; This implementation conses up a completely new simple-vector.
    ;; Using extendable vectors forces us to use AREF (a time price), and
    ;; it also carries some extra space overhead. The case of many properties being added
    ;; en-masse to a object object isn't expected to be very common, so this is probably
    ;; a nice approach.
    (let* ((old-vector %properties)
           (new-vector (make-vector (* *property-vector-grow-ratio* (length old-vector)))))
      (setf %properties (replace new-vector old-vector)))
    object)

  (defun %get-property-cons (object property)
    (find property %properties :test 'eq
          :key #-sheeple3.1 'car
          #+sheeple3.1 (compose 'property-name 'car)))

  (defun %add-property-cons (object property value)
    "This function puts PROPERTY and VALUE into a cons cell and adds the cell to
OBJECT's property-vector if the property is unique. If it's not unique a generic error
is signaled."
    ;; Since we start off all object objects with their property-vector slot set to NIL,
    ;; we have to check for that case and cons up a fresh property-vector. If the vector
    ;; turns out to be full, we also have to enlarge it.
    (let ((properties %properties))
      (unless properties
        (%create-property-vector object)
        (setf properties %properties))
      (if (%get-property-cons object
                              #+sheeple3.1(property-name property)
                              #-sheeple3.1 property)
          (error "Property already exists!")
          (progn
            (when (%property-vector-full-p object)
              (%enlarge-property-vector object)
              (setf properties %properties))
            (dotimes (i (length properties))
              (unless (svref properties i)
                (return (setf (svref properties i) (cons property value))))))))
    object)

  (defun %remove-property-cons (object property)
    "Removes the actual property-cons representing PROPERTY."
    (awhen (position property %properties
                     :test 'eq
                     :key #-sheeple3.1 'car
                     #+sheeple3.1 (compose 'property-name 'car))
      (setf (svref %properties it) nil))
    object)

  ) ; end symbol-macrolet

(defun %direct-property-value (object property-name)
  (cdr (%get-property-cons object property-name)))
(defun (setf %direct-property-value) (new-value object property-name)
  (setf (cdr (%get-property-cons object property-name)) new-value))

#+sheeple3.1
(defun %direct-property-metaobject (object property-name)
  (car (%get-property-cons object property-name)))
#+sheeple3.1
(defun (setf %direct-property-metaobject) (new-value object pname)
  (setf (car (%get-property-cons object pname)) new-value))

;;;
;;; Existential
;;;
(defun has-direct-property-p (object property-name)
  "Returns T if OBJECT has a property called PROPERTY-NAME as a direct property.
NIL otherwise."
  (when (%get-property-cons object property-name) t))

(defun has-property-p (object property-name)
  "Returns T if calling PROPERTY-VALUE on OBJECT using the same property-name
would yield a value (i.e. not signal an unbound-property condition)."
  (some (rcurry 'has-direct-property-p property-name)
        (object-hierarchy-list object)))

;; TODO - The way adding/removing properties is done needs to be thought out better.
;;        Right now, doing (defobject (something) ((var "value"))) would create that
;;        property locally, which could become problematic, specially wrt to how much
;;        space I was hoping to save by adding fresh local property metaobjects only
;;        when they're non-existent in the hierarchy-list, or when add-property is used.
;;
;;        A good part of this issue will probably be resolved when there's a clean,
;;        straightforward API for adding/removing properties. The current interface
;;        is suboptimal, needless to say. -zkat
(defun add-property (object property-name value 
                     &key (reader nil readerp)
                     (writer nil writerp)
                     accessor)
  "Adds a property named PROPERTY-NAME to OBJECT, initialized with VALUE."
  (prog1 object
    (assert (symbolp property-name))
    (when (has-direct-property-p object property-name)
      (cerror "Remove existing property." "~A already has a direct property named ~A."
              object property-name)
      (remove-property object property-name))
    (%add-property-cons object
                        #+sheeple3.1
                        (if (has-property-p object property-name)
                            (%direct-property-metaobject (property-owner object property-name)
                                                         property-name)
                            (defobject (=standard-property=) ((name property-name))))
                        #-sheeple3.1 property-name
                        value)
    (when reader (add-reader-to-object reader property-name object))
    (when writer (add-writer-to-object writer property-name object))
    (when accessor
      (let ((accessor-name (if (eq t accessor) property-name accessor)))
        (unless (and readerp (null reader))
          (add-reader-to-object accessor-name property-name object))
        (unless (and writerp (null writer))
          (add-writer-to-object `(setf ,accessor-name) property-name object))))))

;; TODO - remove-property should look at the property metaobject and remove any replies for
;;        accessors that it points to. This will have to wait until reply-undefinition works
;;        again, though. -syko
;; Addendum - this may not count anymore. I'm not sure if we'll still have the metaobjects
;;            keep track of this info
(defun remove-property (object property-name)
  "Removes OBJECT's direct property named PROPERTY-NAME. Signals an error if there is no such
direct property. Returns OBJECT."
  (if (has-direct-property-p object property-name)
      (prog1 object (%remove-property-cons object property-name))
      (error "Cannot remove property: ~A is not a direct property of ~A" property-name object)))

(defun remove-all-direct-properties (object)
  "Wipes out all direct properties and their values from OBJECT."
  (setf (%object-properties object) nil)
  object)

;;; Value
(defun direct-property-value (object property-name)
  "Returns the property-value set locally in OBJECT for PROPERTY-NAME.
If the value is non-local (is delegated or does not exist in the hierarchy list),
a condition of type UNBOUND-DIRECT-PROPERTY condition is signalled."
  ;;  (declare (inline %object-properties %get-property-cons)) ;commented until after testing
  (if (%object-properties object)
      (aif (%get-property-cons object property-name)
           (cdr it)
           (error 'unbound-property
                  :object object :property-name property-name))
      (error 'unbound-property
             :object object :property-name property-name)))

(defun property-value (object property-name)
  "Returns a property-value that is not necessarily local to OBJECT."
  (property-value-with-hierarchy-list object property-name))

(declaim (inline property-value-with-hierarchy-list))
(defun property-value-with-hierarchy-list (object property-name)
  (aif (find-if (rcurry 'has-direct-property-p property-name) (object-hierarchy-list object))
       (direct-property-value it property-name)
       (error 'unbound-property :object object :property-name property-name)))

(defun (setf property-value) (new-value object property-name)
  "Sets NEW-VALUE as the value of a direct-property belonging to OBJECT, named
PROPERTY-NAME. If the property does not already exist anywhere in the hierarchy list, an error
is signaled."
  ;; TODO - the changes to add-property may have changed the way this should work. -zkat
  (cond ((has-direct-property-p object property-name)
         (setf (%direct-property-value object property-name) new-value))
        ((has-property-p object property-name)
         ;; We place a restriction on the user that a property metaobject
         ;; itself cannot be side-effected. That restriction allows us,
         ;; in the common case of a property already existing in the hierarchy,
         ;; to reuse the property metaobject by just adding a pointer to it locally.
         (let ((owner-prop-mo #+sheeple3.1 (car (%get-property-cons
                                                 (property-owner object property-name)
                                                 property-name))
                              #-sheeple3.1 property-name))
           (%add-property-cons object owner-prop-mo new-value)))
        (t (cerror "Add the property locally" 'unbound-property
                   :object object
                   :property-name property-name)
           (add-property object property-name new-value)))
  new-value)

;;; Reflection API
(defun property-owner (object property-name &optional errorp)
  "Returns the object object with a direct-property called PROPERTY-NAME from which OBJECT inherits
its value. If ERRORP is T, an error is signaled if the property is unbound. Otherwise, NIL is
returned."
  (or (find-if (rcurry 'has-direct-property-p property-name) (object-hierarchy-list object))
      (when errorp (error 'unbound-property :object object :property-name property-name))))

#+sheeple3.1
(defun property-metaobject-p (obj)
  (and (objectp obj)
       (find =standard-property= (object-hierarchy-list obj))))

#+sheeple3.1
(defun direct-property-metaobject (object property-name &optional errorp)
  "Returns the direct local metaobject for a property named PROPERTY-NAME."
  (or (%direct-property-metaobject object property-name)
      (when errorp (error 'unbound-property :object object :property-name property-name))))

(defun object-direct-properties (object)
  #-sheeple3.1 "Returns a set of direct property definition metaobjects."
  #+sheeple3.1 "Returns a set of direct property definition metaobjects."
  (awhen (%object-properties object)
    (loop for prop across it when prop collect (car prop))))

(defun available-properties (object)
  "Returns a list of property objects describing all properties available to OBJECT, including
inherited ones."
  #+sheeple3.1
  (mapcar (fun (direct-property-metaobject (property-owner object _ nil) _))
          (mapcar 'property-name
                  (delete-duplicates (append (object-direct-properties object)
                                             (mapcan #'available-properties
                                                     (object-parents object)))
                                     :key #'property-name)))
  #-sheeple3.1
  (delete-duplicates (append (object-direct-properties object)
                             (mapcan 'available-properties (object-parents object)))))

(defun property-summary (object &optional (stream *standard-output*))
  "Provides a pretty-printed representation of OBJECT's available properties."
  (format stream
          "~&Object: ~A~%Properties:~% ~{~{~&~3TName: ~13T~A~%~3TValue: ~13T~S~%~
           ~3TOwner: ~13T~A~%~%~}~}"
          object (mapcar (fun (let ((pname #+sheeple3.1(property-name _)
                                          #-sheeple3.1 _))
                               (list pname
                                     (property-value object pname)
                                     (property-owner object pname))))
                        (available-properties object))))

(defun direct-property-summary (object &optional stream)
  "Provides a pretty-printed representation of OBJECT's direct properties."
  (format stream
          "~&Object: ~A~%~
           Direct Properties: ~%~%~
           ~{~{~&~3TName: ~A~%~3TValue: ~S~%~%~}~}"
          object (mapcar (fun (let ((pname #+sheeple3.1 (property-name_)
                                          #-sheeple3.1 _))
                               (list pname
                                     (direct-property-value object pname))))
                        (object-direct-properties object))))

;;; Convenience
(defmacro with-properties (properties object &body body)
  (let ((sh (gensym)))
    `(let ((,sh ,object))
       (symbol-macrolet ,(mapcar (lambda (property-entry)
                                   (let ((var-name
                                          (if (symbolp property-entry)
                                              property-entry
                                              (car property-entry)))
                                         (property-name
                                          (if (symbolp property-entry)
                                              property-entry
                                              (cadr property-entry))))
                                     `(,var-name
                                       (property-value ,sh ',property-name))))
                                 properties)
         ,@body))))