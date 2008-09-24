(in-package :bknr.impex)

(defclass xml-class (indexed-class)
  ((element :initarg :element :initform nil :accessor xml-class-element)
   (unique-id-slot   :initarg :unique-id-slot :initform nil
		     :documentation "if set to a slot name, this
signals that the slot can be used as a unique id to refer to an
instance of the object in a n XML update operation")
   (unique-id-reader :initarg :unique-id-reader :initform nil
		     :documentation "if set to a function, this
signals that the function can be used as a unique index-reader when
used in XML update operations.")))

(defmethod xml-class-unique-id-slot ((class xml-class))
  (first (slot-value class 'unique-id-slot)))

(defmethod xml-class-unique-id-reader ((class xml-class))
  (eval (first (slot-value class 'unique-id-reader))))

(defmethod validate-superclass ((sub xml-class) (super indexed-class))
  t)

(defun princ-to-string-1 (object)
  (when object
    (princ-to-string object)))

(defclass xml-direct-slot-definition (bknr.indices::index-direct-slot-definition)
  ((attribute :initarg :attribute
              :initform nil
              :documentation "Name of attribute to use to impex the slot.")
   (element :initarg :element
            :initform nil
            :documentation "Name of the element to use to impex the slot.")
   (body :initarg :body
         :initform nil
         :documentation "Whether the value of the slot has to be stored in the body of the class element.")
   (containment :initarg :containment
                :initform nil
                :documentation "Containment specification for this slot, either nil, :* or :+")
   (parser :initarg :parser
           :initform #'identity
           :documentation "Function used to parse the slot value from the XML string.")
   (serializer :initarg :serializer
               :initform #'princ-to-string-1
               :documentation "Function used to serialize the slot back to XML.")

   (object-id-slot :initarg :object-id-slot
                   :initform nil
                   :documentation "If this slot is non-nil, the slot's
value is considered to be the unique object id of the object.  During
export, objects which have an object-id-slot will only be serialized
once.  Further occurances of the same object will be referenced
through the object-id-slot (either an element or an attribute)")

   (id-to-object :initarg :id-to-object
		 :initform nil
		 :documentation "Function used to get the value pointed to by the ID.")
   (object-to-id :initarg :object-to-id
		 :initform nil
		 :documentation "Function used to get the ID of the object stored in the slot.")

   (parent :initarg :parent
           :initform nil
           :documentation "Slot is a pointer to the parent object.")))

(defclass xml-effective-slot-definition (bknr.indices::index-effective-slot-definition)
  ((body :initform nil)
   (element :initform nil :reader xml-effective-slot-definition-element)
   (attribute :initform nil :reader xml-effective-slot-definition-attribute)

   (parser :initform nil :reader xml-effective-slot-definition-parser)
   (serializer :initform nil :reader xml-effective-slot-definition-serializer)

   (object-id-slot :initform nil :reader xml-effective-slot-definition-object-id-slot)
   
   (id-to-object :initform nil)
   (object-to-id :initform nil)
   (parent :initform nil)

   (containment :initform nil :reader xml-effective-slot-definition-containment)
   (required-p :initform nil :reader xml-effective-slot-definition-required-p)))

(defmethod print-object ((slot xml-effective-slot-definition) stream)
  (print-unreadable-object (slot stream :type t :identity t)
    (with-slots (attribute element body parent) slot
      (format stream "~A (~A~@[ ~S~])" (slot-definition-name slot)
	      (cond (attribute "ATTRIBUTE")
		    (element "ELEMENT")
		    (body "BODY")
		    (parent "PARENT")
		    (t "UNKNOWN"))
	      (or attribute element)))))

(defmethod xml-class-attribute-slots ((class xml-class))
  (remove-if #'(lambda (slot)
		 (or (not (typep slot 'xml-effective-slot-definition))
		     (not (slot-value slot 'attribute)))) (class-slots class)))

(defmethod xml-class-element-slots ((class xml-class))
  (remove-if #'(lambda (slot)
		 (or (not (typep slot 'xml-effective-slot-definition))
		     (not (slot-value slot 'element)))) (class-slots class)))

(defmethod xml-class-body-slot ((class xml-class))
  (let ((body-slots
	 (remove-if #'(lambda (slot)
			(or (not (typep slot 'xml-effective-slot-definition))
			    (not (slot-value slot 'body)))) (class-slots class))))
    (when (> (length body-slots) 1)
      (error "Class ~A has more than one body slot: ~A." class body-slots))
    (first body-slots)))

(defmethod xml-class-find-attribute-slot ((class xml-class) attribute)
  (find attribute (xml-class-attribute-slots class)
	:test #'string-equal
	:key #'(lambda (slot) (slot-value slot 'attribute))))

(defmethod xml-class-find-element-slot ((class xml-class) element)
  (find element (xml-class-element-slots class)
	:test #'string-equal
	:key #'(lambda (slot) (slot-value slot 'element))))

(defmethod xml-class-parent-slot ((class xml-class))
  (let ((parent-slots
	 (remove-if #'(lambda (slot)
			(or (not (typep slot 'xml-effective-slot-definition))
			    (not (slot-value slot 'parent))))
		    (class-slots class))))
    (when (> (length parent-slots) 1)
      (error "Class ~A has more than one parent slot: ~A." class parent-slots))
    (first parent-slots)))

(defmethod initialize-instance :after ((class xml-class) &key element &allow-other-keys)
  (setf (xml-class-element class) (or (first element) (string-downcase (class-name class))))
  (xml-class-finalize class))

(defmethod reinitialize-instance :after ((class xml-class) &key element &allow-other-keys)
  (setf (xml-class-element class) (or (first element) (string-downcase (class-name class))))
  (xml-class-finalize class))

(defmethod xml-class-finalize ((class xml-class))
  (unless (class-finalized-p class)
    (finalize-inheritance class))

  (class-slots class))

(defmethod direct-slot-definition-class ((class xml-class) &key parent attribute element body &allow-other-keys)
  (if (or attribute element body parent)
      'xml-direct-slot-definition
      (call-next-method)))

(defmethod effective-slot-definition-class ((class xml-class) &rest initargs)
  (declare (ignore initargs))
  'xml-effective-slot-definition)

(defmethod compute-effective-slot-definition :around ((class xml-class) name direct-slots)
  (let* ((xml-directs (remove-if-not #'(lambda (class) (typep class 'xml-direct-slot-definition))
				     direct-slots))
	 (xml-direct (first xml-directs)))

    ;; Commented out this check because I could not determine what it does and it warned me.
    #+(or)
    (when (> (length xml-directs) 1)
      (dolist (slot-def (class-slots (class-of (first xml-directs))))
        (unless (apply #'equal (mapcar #'(lambda (slot) (slot-value slot (slot-definition-name slot-def))) xml-directs))
          (warn "Possibly conflicting slot options for overloaded slot ~A." (slot-definition-name slot-def)))))

    (let ((normal-slot (call-next-method)))
      (when (and xml-direct
		 (typep normal-slot 'xml-effective-slot-definition))
	(with-slots (attribute element body parent) xml-direct
	  (when (> (length (remove nil (list parent element attribute body))) 1)
	    (error "Only one of ELEMENT, ATTRIBUTE, PARENT or BODY is possible for a slot definition."))
	  (unless (or body parent)
	    (unless (or element attribute)
	      (setf element (string-downcase name)))
	    (when element
	      (setf element (if (eq t element) (string-downcase name) element)))
	    (when attribute
	      (setf attribute (if (eq t attribute) (string-downcase name) attribute)))
	    (unless (or element attribute)
	      (error "Could not find element or attribute for slot ~A." name))))

        ;; copy direct-slot-definition slots to effective-slot-definition
	(dolist (slot '(parser serializer body id-to-object object-to-id
			parent attribute element containment))
	  (setf (slot-value normal-slot slot)
		(slot-value xml-direct slot))))

      (dolist (slot '(parser serializer object-id-slot object-to-id id-to-object) normal-slot)
	(let ((value (slot-value normal-slot slot)))
	  (when value
	    (setf (slot-value normal-slot slot)
		  (eval value)))))
	
      normal-slot)))

(defmethod xml-object-check-validity (object)
  (let ((class (class-of object)))
    (unless (typep class 'xml-class)
      (error "Object ~a is not of metaclass XML-CLASS." object))
    (dolist (slot (class-slots class))
      (when (typep slot 'xml-effective-slot-definition)
	(when (and (xml-effective-slot-definition-required-p slot)
		   (not (slot-boundp object (slot-definition-name slot))))
	  (error "Required slot ~A is not bound in ~a."
		 (slot-definition-name slot) object))
	(let ((containment (xml-effective-slot-definition-containment slot)))
	  (when (and containment
		     (eql containment :+)
		     (null (slot-value object (slot-definition-name slot))))
	    (error "Slot ~a with containment :+ has no value."
		   (slot-definition-name slot))))))))
