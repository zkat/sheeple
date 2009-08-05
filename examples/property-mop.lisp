;;;;
;;;; This is a quick hack to show the general idea of how writing a bit of mop code that allows
;;;; external storage of property-values would work.
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :sheeple-user)

(defvar *max-oid* 0)
(defclass persistent-sheep (standard-sheep) ((oid :initform (incf *max-oid*) :reader oid)))
(defclass persistent-property (standard-property) ())

(defmethod initialize-instance :after ((sheep persistent-sheep) &key)
  (format t "Allocating space for ~A in an external database. Its OID is: ~A.~%" sheep (oid sheep)))

(defmethod add-parent :around ((sheep standard-sheep) (sheep persistent-sheep))
  (if (or (eql sheep =dolly=)
          (eql (find-class 'persistent-sheep)
               (class-of sheep)))
      (call-next-method)
      (error "We don't support adding non-persistent sheep as parents!")))

(defmethod add-property :around ((sheep persistent-sheep) pname value &key transientp)
  (format t "Allocating existence of property in database, even if it's transient.~
             That way, we know whether the property is even supposed to exist.~%")
  (if transientp
      (call-next-method)
      (call-next-method sheep pname value :property-metaclass 'persistent-property)))

(defmethod add-property-using-property-metaobject ((sheep persistent-sheep) value
                                                   (property persistent-property) &key)
  (format t "Allocating property with name ~A in database.~%" (property-name property))
  (call-next-method))

(defmethod direct-property-value ((sheep persistent-sheep) (property persistent-property))
  (format t "Fetching property-value for ~A from database." (property-name property)))

(defmethod (setf property-value) (new-value (sheep persistent-sheep)
                                  (property persistent-property))
  (format t "Setting property value for ~A in database.~%" (property-name property)))


(defproto =persistent-obj= ()
  ((foo "bar")
   (baz "quux" :transientp t))
  (:metaclass 'persistent-sheep))

