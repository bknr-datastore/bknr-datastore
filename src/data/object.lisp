;;; MOP based object subsystem for the BKNR datastore

(in-package :bknr.datastore)

(cl-interpol:enable-interpol-syntax)

(defclass store-object-subsystem ()
  ((next-object-id :initform 0
                   :accessor next-object-id
                   :documentation "Next object ID to assign to a new object")))

(defun store-object-subsystem ()
  (let ((subsystem (find-if (lambda (subsystem)
			      (typep subsystem 'store-object-subsystem))
			    (store-subsystems *store*))))
    (unless subsystem
      (error "Could not find a store-object-subsystem in the current store ~a." *store*))
    subsystem))

(defclass persistent-class (indexed-class)
  ((transient-slot-initargs :initform nil
			    :accessor persistent-class-transient-slot-initargs)))

(defmethod determine-transient-slot-initargs ((class persistent-class))
  (with-slots (transient-slot-initargs) class
    (setf transient-slot-initargs nil)
    (dolist (slot (class-slots class))
      (when (and (typep slot 'persistent-effective-slot-definition)
		 (persistent-effective-slot-definition-transient slot)
		 (slot-definition-initargs slot))
	(pushnew (car (slot-definition-initargs slot)) transient-slot-initargs)))))

(defmethod validate-superclass ((sub persistent-class) (super indexed-class))
  t)

(defvar *suppress-schema-warnings* nil)

(deftransaction update-instances-for-changed-class (class)
  (let ((instance-count (length (class-instances class))))
    (when (plusp instance-count)
      (unless *suppress-schema-warnings*
	(warn "updating ~A instances of ~A for class changes" instance-count class))
      (mapc #'reinitialize-instance (class-instances class)))))

(defmethod instance :after ((class persistent-class) &rest args)
  (declare (ignore args))
  (determine-transient-slot-initargs class))

(defmethod reinitialize-instance :after ((class persistent-class) &rest args)
  (declare (ignore args))
  (determine-transient-slot-initargs class)
  (when *store*
    (update-instances-for-changed-class (class-name class))
    (unless *suppress-schema-warnings*
      (warn "Class ~A has been changed. To ensure correct schema evolution, please snapshot your datastore."
	    (class-name class)))))

(defclass persistent-direct-slot-definition (index-direct-slot-definition)
  ((transient :initarg :transient :initform nil)
   (relaxed-object-reference :initarg :relaxed-object-reference :initform nil)))

(defclass persistent-effective-slot-definition (index-effective-slot-definition)
  ((transient :initarg :transient
	      :initform nil
	      :reader persistent-effective-slot-definition-transient)
   (relaxed-object-reference :initarg :relaxed-object-reference
			     :initform nil)))

(defmethod persistent-slot-p ((slot standard-effective-slot-definition))
  nil)

(defmethod persistent-slot-p ((slot persistent-effective-slot-definition))
  (not (slot-value slot 'transient)))

(defmethod relaxed-object-reference-slot-p ((slot standard-effective-slot-definition))
  nil)

(defmethod relaxed-object-reference-slot-p ((slot persistent-effective-slot-definition))
  "Slot is a relaxed object reference slot.  If the slot holds a
pointer to another persistent object and the referenced object is
deleted, slot reads will return nil."
  (slot-value slot 'relaxed-object-reference))

(defmethod (setf slot-value-using-class) :before (newval (class persistent-class) object slotd)
  (when (and (persistent-slot-p slotd)
	     (not (in-transaction-p)))
    (error "Attempt to set persistent slot ~A of ~A outside of a transaction"
	   (slot-definition-name slotd) object)))

(defmethod (setf slot-value-using-class) :after (newval (class persistent-class) object slotd)
  (when (in-anonymous-transaction-p)
    (push (make-instance 'transaction :timestamp (get-universal-time)
			 :function-symbol 'change-slot-values
			 :args (list object (slot-definition-name slotd) newval))
	  (anonymous-transaction-transactions *current-transaction*))))

(defmethod direct-slot-definition-class ((class persistent-class) &key &allow-other-keys)
  'persistent-direct-slot-definition)

(defmethod effective-slot-definition-class ((class persistent-class) &rest initargs)
  (declare (ignore initargs))
  'persistent-effective-slot-definition)

(defmethod compute-effective-slot-definition :around
    ((class persistent-class) name direct-slots)
  (let* ((persistent-directs (remove-if-not #'(lambda (class)
						(typep class 'persistent-direct-slot-definition))
					    direct-slots))
	 (transient (remove-duplicates (mapcar #'(lambda (slot)
						   (slot-value slot 'transient))
					       persistent-directs)))
	 (relaxed (reduce #'(lambda (&optional x y) (or x y))
			  (mapcar #'(lambda (slot)
				      (slot-value slot 'relaxed-object-reference))
				  persistent-directs))))
    (when (> (length transient) 1)
      (error "Can not create a slot that is both persistent and transient: ~a." name))
    (let ((normal-slot (call-next-method)))
      (when (typep normal-slot 'persistent-effective-slot-definition)
	(setf (slot-value normal-slot 'transient) (first transient)
	      (slot-value normal-slot 'relaxed-object-reference) relaxed))
      normal-slot)))

(defmethod class-persistent-slots ((class standard-class))
  (remove-if-not #'persistent-slot-p (class-slots class)))

(defclass store-object ()
  ((id :initarg :id :reader store-object-id
       :index-type unique-index
       :index-initargs (:test #'eql)
       :index-reader store-object-with-id :index-values all-store-objects
       :index-mapvalues map-store-objects))
  (:metaclass persistent-class)
  (:class-indices (all-class :index-type class-skip-index
			     :index-subclasses t
			     :index-initargs (:index-superclasses t)
			     :index-keys all-store-classes
			     :index-reader store-objects-with-class
			     :slots (id))))

(defun class-instances (class)
  (unless (find-class class)
    (error "class-instances called for nonexistent class ~A" class))
  (store-objects-with-class class))

#+allegro
(aclmop::finalize-inheritance (find-class 'store-object))

(defmethod initialize-instance :around
    ((object store-object) &key &allow-other-keys)
  (if (in-anonymous-transaction-p)
      (prog1
	  (call-next-method)
	(push (make-instance 'transaction
			     :function-symbol 'make-instance
			     :timestamp (get-universal-time)
			     :args (cons (class-name (class-of object))
					 (loop for slotd in (class-slots (class-of object))
					    for slot-name = (slot-definition-name slotd)
					    for slot-initarg = (first (slot-definition-initargs slotd))
					    when (and slot-initarg
						      (slot-boundp object slot-name))
					    appending (list slot-initarg (slot-value object slot-name)))))
	      (anonymous-transaction-transactions *current-transaction*)))
      (call-next-method)))

(defmethod initialize-instance :after ((object store-object) &key id &allow-other-keys)
  (let ((subsystem (store-object-subsystem)))
    (cond (id
	   ;; during restore, use the given ID
	   (when (>= id (next-object-id subsystem))
	     (setf (next-object-id subsystem) (1+ id))))
	  (t
	   ;; normal transaction: assign a new ID
	   (setf id (next-object-id subsystem))
	   (incf (next-object-id subsystem))
	   (setf (slot-value object 'id) id)))))

(defmethod print-object ((object store-object) stream)
  (print-unreadable-object (object stream :type t)
    (format stream "ID: ~D" (store-object-id object))))

(defmethod print-object :around ((object store-object) stream)
  (if (object-destroyed-p object)
      (print-unreadable-object (object stream :type t)
	(princ "DESTROYED" stream))
      (call-next-method)))

(defmethod change-class :before ((object store-object) class &rest args)
  (declare (ignore args))
  (when (not (in-transaction-p))
    (error "Can't change class of persistent object ~A using change-class outside of transaction, please use PERSISTENT-CHANGE-CLASS instead" object)))

(defun tx-persistent-change-class (object class-name &rest args)
  (warn "TX-PERSISTENT-CHANGE-CLASS does not maintain class indices, please snapshot and restore to recover indices")
  (apply #'change-class object (find-class class-name) args))

(defun persistent-change-class (object class &rest args)
  (execute (make-instance 'transaction :function-symbol 'tx-persistent-change-class
			  :timestamp (get-universal-time)
			  :args (append (list object (if (symbolp class) class (class-name class))) args))))

(defgeneric initialize-persistent-instance (store-object)
  (:documentation
   "Initializes the persistent aspects of a persistent object. This method is called
at the creationg of a persistent object, but not when the object is loaded from a
snapshot."))

(defgeneric initialize-transient-instance (store-object)
  (:documentation
   "Initializes the transient aspects of a persistent object. This method is called
whenever a persistent object is initialized, also when the object is loaded from
a snapshot."))

(defmethod initialize-persistent-instance ((object store-object)))
(defmethod initialize-transient-instance ((object store-object)))

(defmethod store-object-persistent-slots ((object store-object))
  (mapcar #'slot-definition-name (class-persistent-slots (class-of object))))

(defmethod store-object-relaxed-object-reference-p ((object store-object) slot-name)
  (let ((slot (find slot-name (class-slots (class-of object)) :key #'slot-definition-name)))
    (when slot
      (relaxed-object-reference-slot-p slot))))

(defmacro define-persistent-class (class (&rest superclasses) slots &rest class-options)
  (let ((superclasses (or superclasses '(store-object)))
	(metaclass (cadr (assoc :metaclass class-options))))
    (when (and metaclass
	       (not (validate-superclass (find-class metaclass)
					 (find-class 'persistent-class))))
      (error "Can not define a persistent class with metaclass ~A." metaclass))
    `(define-bknr-class ,class ,superclasses ,slots
      ,@(unless metaclass '((:metaclass persistent-class)))
      ,@class-options)))

(defmacro defpersistent-class (class (&rest superclasses) slots &rest class-options)
  (let ((superclasses (or superclasses '(store-object)))
	(metaclass (cadr (assoc :metaclass class-options))))
    (when (and metaclass
	       (not (validate-superclass (find-class metaclass)
					 (find-class 'persistent-class))))
      (error "Can not define a persistent class with metaclass ~A." metaclass))
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (defclass ,class ,superclasses ,slots
	 ,@(unless metaclass '(:metaclass persistent-class))
	 ,@class-options))))

#+nil
(define-persistent-class foo ()
  ((a :read)))
#+nil
(let ((foo (make-object 'foo :a 2)))
  (foo-a foo))

;;; test fuer multiple inheritance
#+nil
(progn
  (define-persistent-class bar ()
    ((b :read)))
  (define-persistent-class blorg (foo bar)
    ((c :read)))
  (make-object 'blorg :a 2 :b 3 :c 4))

;;; binary snapshot

(defvar *current-object-slot* nil)
(defvar *current-slot-relaxed-p* nil)

(defun encode-layout (id class slots stream)
  (%write-char #\L stream)
  (%encode-integer id stream)
  (%encode-symbol (class-name class) stream)
  (%encode-integer (length slots) stream)
  (dolist (slot slots)
    (%encode-symbol slot stream)))

(defun %encode-set-slots (slots object stream)
  (dolist (slot slots)
    (let ((*current-object-slot* (list object slot))
	  (*current-slot-relaxed-p* (store-object-relaxed-object-reference-p object slot)))
      (encode (if (slot-boundp object slot)
		  (slot-value object slot)
		  'unbound)
	      stream))))

(defun encode-create-object (class-layouts object stream)
  (let* ((class (class-of object))
         (layout (gethash class class-layouts)))
    (unless layout 
      (setf layout
            (cons (hash-table-count class-layouts)
                  ;; XXX layout muss konstant sein
                  (sort (remove 'id (store-object-persistent-slots object))
			#'string< :key #'symbol-name)))
      (encode-layout (car layout) class (cdr layout) stream)
      (setf (gethash class class-layouts) layout))
    (destructuring-bind (layout-id &rest slots) layout
      (declare (ignore slots))
      (%write-char #\O stream)
      (%encode-integer layout-id stream)
      (%encode-integer (store-object-id object) stream))))

(defun encode-set-slots (class-layouts object stream)
  (destructuring-bind (layout-id &rest slots)
      (gethash (class-of object) class-layouts)
    (%write-char #\S stream)
    (%encode-integer layout-id stream)
    (%encode-integer (store-object-id object) stream)
    (%encode-set-slots slots object stream)))

(defvar *class-rename-hash* (make-hash-table))

(defun find-class-with-interactive-renaming (class-name)
  (loop until (find-class class-name nil)
	do (progn
	     (format *query-io* "Class ~A not found, enter new class: " class-name)
	     (finish-output *query-io*)
	     (setq class-name (read *query-io*))))
  (setf (gethash class-name *class-rename-hash*) (find-class class-name)))

(defun find-slot-name-with-interactive-rename (class slot-name)
  (loop until (find slot-name (class-slots class) :key #'slot-definition-name)
	do (format *query-io* "Slot ~S not found in class ~S, enter new slot name: "
		   slot-name (class-name class))
	do (setq slot-name (read *query-io*))
	finally (return slot-name)))

(defvar *slot-name-map* nil)

(defun rename-slot (class slot-name)
  (or (caddr (find (list (class-name class) slot-name) *slot-name-map* :key #'(lambda (entry) (subseq entry 0 2)) :test #'equal))
      (find (symbol-name slot-name)
	    (mapcar #'slot-definition-name (class-slots class)) :key #'symbol-name :test #'equal)))

(defun find-slot-name-with-automatic-rename (class slot-name)
  (if (find slot-name (class-slots class) :key #'slot-definition-name)
      slot-name
      (restart-case
	  (let ((new-slot-name (rename-slot class slot-name)))
	    (cond
	      (new-slot-name
	       (warn "slot ~S not found in class ~S, automatically renamed to ~S"
		     slot-name (class-name class) new-slot-name)
	       new-slot-name)
	      (t
	       (error "can't find a slot in class ~A which matches the name ~A used in the store snapshot"
		      (class-name class) slot-name))))
	(ignore-slot ()
	  :report "Ignore slot, discarding values found in the snapshot file"
	  nil))))

(defun find-class-slots-with-interactive-renaming (class slot-names)
  #+(or)
  (format t "; verifying class layout for class ~A~%; slots:~{ ~S~}~%" (class-name class)
	  (mapcar #'slot-definition-name (class-slots class)))
  (loop for slot-name in slot-names
	collect (find-slot-name-with-automatic-rename class slot-name)))

(defun snapshot-read-layout (stream layouts)
  (let* ((id (%decode-integer stream))
         (class-name (%decode-symbol stream))
         (nslots (%decode-integer stream))
	 (class (find-class-with-interactive-renaming class-name))
         (slots (find-class-slots-with-interactive-renaming class (loop
								   repeat nslots
								   collect (%decode-symbol stream)))))
    (setf (gethash id layouts)
          (cons class slots))))

(defun %read-slots (stream object slots)
  (declare (optimize (speed 3)))
  (dolist (slot-name slots)
    (if slot-name			; NIL for slots which are not restored because of schema changes
	(restart-case
	    (let ((*current-object-slot* (list object slot-name))
		  (*current-slot-relaxed-p*
		   (store-object-relaxed-object-reference-p object slot-name)))
	      (let ((value (decode stream)))
		(when object
		  (let ((bknr.indices::*indices-remove-p* nil))
		    (if (eq value 'unbound)
			(slot-makunbound object slot-name)
			(setf (slot-value object slot-name) value))))))
	  (set-slot-nil ()
	    :report "Set slot to NIL."
	    (setf (slot-value object slot-name) nil))
	  (make-slot-unbound ()
	    :report "Make slot unbound."
	    (slot-makunbound object slot-name)))
	(decode stream))))		; read and ignore value

(defun snapshot-read-object (stream layouts)
  (declare (optimize (speed 3)))
  (with-simple-restart (skip-object "Skip the object.")
    (let ((layout-id (%decode-integer stream))
	  (object-id (%decode-integer stream)))
      #+nil (format t "id: ~A~%" object-id)
      (destructuring-bind (class &rest slots) (gethash layout-id layouts)
	(declare (ignore slots))
	#+nil (format t "; class: ~A~%" class)
	(let ((result (make-instance class :id object-id)))
	  result)))))

(defun snapshot-read-slots (stream layouts)
  (let* ((layout-id (%decode-integer stream))
	 (object-id (%decode-integer stream))
	 (object (store-object-with-id object-id)))
    (restart-case
	(progn
	  #+nil (format t "read-slots for object ~A, id ~A~%" object object-id)
	  (unless object
	    (error "READ-SLOTS form for unexistent object with ID ~A~%" object-id))
	  (%read-slots stream object (cdr (gethash layout-id layouts)))
	  #+nil
	  (when object 
	    (initialize-transient-instance object)))
	(skip-object-initialization ()
	  :report "Skip object initialization.")
	(delete-object ()
	  :report "Delete the object."
	  (delete-object object)))))

(defmethod encode-object ((object store-object) stream)
  (if (object-destroyed-p object)
      (let* ((*indexed-class-override* t)
	     (id (store-object-id object))
	     (container (first *current-object-slot*))
	     (slot (second *current-object-slot*)))

	;;; if we are not encoding slot values, something has gone
	;;; wrong with the indices
	(unless (and container slot)
	  (warn "Encoding destroyed object with ID ~A." id)
	  (%write-char #\o stream)
	  (%encode-integer id stream)
	  (return-from encode-object))

	(if *current-slot-relaxed-p*
	    ;;; the slot can contain references to deleted objects, just warn
	    (progn
	      (warn "Encoding reference to destroyed object with ID ~A from slot ~A of object ~A with ID ~A."
		    id slot (type-of container) (store-object-id container))
	      (%write-char #\o stream)
	      (%encode-integer id stream))
	    ;;; the slot can't contain references to deleted objects, throw an error
	    (error "Encoding reference to destroyed object with ID ~A from slot ~A of object ~A with ID ~A."
		   id slot (type-of container) (store-object-id container))))

      ;;; Go ahead and serialize the object reference
      (progn (%write-char #\o stream)
	     (%encode-integer (store-object-id object) stream))))

(defmethod decode-object ((tag (eql #\o)) stream)
  (%decode-store-object stream))

(defun %decode-store-object (stream)
  ;; This is actually called in two contexts, when a slot-value is to be filled with a reference to a store object
  ;; and when a list of store objects is read from the transaction log (%decode-list).  In the former case, references
  ;; two deleted objects are accepted when the slot pointing to the object is marked as being a "relaxed-object-reference",
  ;; in the latter case, no such information is available.  To ensure maximum restorability of transaction logs, object
  ;; references stored in lists are always considered to be relaxed references, which means that references to deleted
  ;; objects are restored as NIL.  Applications must be prepared to cope with NIL entries in such object lists (usually
  ;; lists in slots).
  (let* ((id (%decode-integer stream))
	 (object (or (store-object-with-id id)
		     (warn "internal inconsistency during restore: can't find store object ~A in loaded store" id)))
	 (container (first *current-object-slot*))
	 (slot-name (second *current-object-slot*)))
    (cond (object object)

	  ((or *current-slot-relaxed-p* (not container))
	   (if container
	       (warn "Reference to inexistent object with id ~A in relaxed slot ~A of object with class ~A with ID ~A."
		     id slot-name (type-of container) (store-object-id container))
	       (warn "Reference to inexistent object with id ~A from unnamed container, returning NIL." id))

	   ;; Possibly determine new "current object id"
	   (when (>= id (next-object-id (store-object-subsystem)))
	     (setf (next-object-id (store-object-subsystem)) (1+ id)))
	   nil)
	  
	  (t (error "Reference to inexistent object with id ~A from slot ~A of object ~A with ID ~A." id slot-name (type-of container)
		    (if container (store-object-id container) "unknown object"))))))

(defmethod snapshot-subsystem ((store store) (subsystem store-object-subsystem))
  (let ((snapshot (store-subsystem-snapshot-pathname store subsystem)))
    (with-open-file (s snapshot
		       :direction :output
		       :element-type '(unsigned-byte 8)
		       :if-does-not-exist :create
		       :if-exists :supersede)
      (let ((class-layouts (make-hash-table)))
        (with-transaction (:prepare-for-snapshot) 
          (map-store-objects #'prepare-for-snapshot))
	(map-store-objects (lambda (object) (when (subtypep (type-of object) 'store-object)
					      (encode-create-object class-layouts object s))))
	(map-store-objects (lambda (object) (when (subtypep (type-of object) 'store-object)
					      (encode-set-slots class-layouts object s))))
	t))))

(defmethod close-subsystem ((store store) (subsystem store-object-subsystem))
  (dolist (class-name (all-store-classes))
    (clear-class-indices (find-class class-name))))

(defmethod restore-subsystem ((store store) (subsystem store-object-subsystem) &key until)
  ; XXX check that until > snapshot time
  (declare (ignore until))
  (let ((snapshot (store-subsystem-snapshot-pathname store subsystem)))
    ;;; not all indices that should be cleared are cleared. maybe
    ;;; check on first instatiation of a class?
    (dolist (class-name (cons 'store-object (all-store-classes)))
      (clear-class-indices (find-class class-name)))
    (setf (next-object-id subsystem) 0)
    (when (probe-file snapshot)
      (format *trace-output* "loading snapshot file ~A~%" snapshot)
      (with-open-file (s snapshot
			 :element-type '(unsigned-byte 8)
			 :direction :input)
	(let ((class-layouts (make-hash-table))
	      (created-objects 0)
	      (read-slots 0)
	      (error t))
	  (unwind-protect
	       (progn
		 (with-simple-restart
		     (finalize-object-subsystem "Finalize the object subsystem.")
		   (loop
		    (when (and (plusp created-objects)
			       (zerop (mod created-objects 10000)))
		      #+nil(format t "Snapshot position ~A~%" (file-position s))
		      (format t "~A objects created.~%" created-objects)
		      (force-output))
		    (when (and (plusp read-slots)
			       (zerop (mod read-slots 10000)))
		      (format t "~A of ~A slots set.~%" read-slots created-objects)
		      (force-output))
		    (let ((char (%read-char s nil nil)))
		      (unless (member char '(#\O #\L #\S nil))
			(format t "unknown char ~A at offset ~A~%" char (file-position s)))
		      (ecase char
			((nil) (return))
			(#\O (snapshot-read-object s class-layouts) (incf created-objects))
			(#\L (snapshot-read-layout s class-layouts))
			(#\S (snapshot-read-slots s class-layouts) (incf read-slots))))))
		 (map-store-objects #'initialize-transient-instance)
		 (setf error nil))
	    (when error
	      (maphash #'(lambda (key val)
			   (declare (ignore key))
			   (let ((class-name (car val)))
			     (format t "clearing indices for class ~A~%" (class-name class-name))
			     (clear-class-indices class-name)))
		       class-layouts))))))))

(defun remove-transient-slot-initargs (class initializers)
  "Remove all initializers for transient slots"
  (loop for (keyword value) on initializers by #'cddr
       unless (find keyword (persistent-class-transient-slot-initargs class))
       collect keyword
       and
       collect value))

;;; create object transaction, should not be called from user code, as we have to give it
;;; a unique id in the initargs. After the object is created, the persistent and the
;;; transient instances are initialized
(defun tx-make-object (class-name &rest initargs)
  (let (obj
	(error t))
    (unwind-protect
	 (let ((restoring (eq (store-state *store*) :restore)))
	   (setf obj (apply #'make-instance class-name
			    (if restoring
				(remove-transient-slot-initargs (find-class class-name) initargs)
				initargs)))
	   (initialize-persistent-instance obj)
	   (initialize-transient-instance obj)
	   (setf error nil)
	   obj)
      (when (and error obj)
	(destroy-object obj)))))

(defun make-object (class-name &rest initargs)
  "Make a persistent object of class named CLASS-NAME. Calls MAKE-INSTANCE with INITARGS."
  (execute (make-instance 'transaction
			  :function-symbol 'tx-make-object
			  :args (append (list class-name
					      :id (next-object-id (store-object-subsystem)))
					initargs))))

(defun tx-delete-object (id)
  (destroy-object (store-object-with-id id)))

(defun delete-object (object)
  (if (in-transaction-p)
      (destroy-object object)
      (execute (make-instance 'transaction :function-symbol 'tx-delete-object
                              :timestamp (get-universal-time)
                              :args (list (store-object-id object))))))

(defun tx-delete-objects (&rest object-ids)
  (mapc #'(lambda (id) (destroy-object (store-object-with-id id))) object-ids))

(defun delete-objects (&rest objects)
  (if (in-transaction-p)
      (mapc #'destroy-object objects)
      (execute (make-instance 'transaction :function-symbol 'tx-delete-objects
                              :timestamp (get-universal-time)
                              :args (mapcar #'store-object-id objects)))))

(defgeneric cascade-delete-p (object referencing-object)
  (:documentation "return non-nil if the REFERENCING-OBJECT should be deleted when the OBJECT is deleted"))

(defmethod cascade-delete-p (object referencing-object)
  nil)

(defun partition-list (list predicate)
  "Return two list values, the first containing all elements from LIST
that satisfy PREDICATE, the second those that don't"
  (let (do dont)
    (dolist (element list)
      (if (funcall predicate element)
	  (push element do)
	  (push element dont)))
    (values do dont)))

(defun cascading-delete-object (object)
  "Delete the OBJECT and all objects that reference it and that are eligible to cascading deletes, as indicated by
the result of calling CASCADE-DELETE-P.  Generate error if there are references to the objects that are not eligible
to cascading deletes."
  (multiple-value-bind (cascading-delete-refs
			remaining-refs)
      (partition-list (find-refs object) #'cascade-delete-p)
    (when remaining-refs
      (error "Cannot delete object ~A because there are references to this object in the system, please consult a system administrator!"
	     object))
    (apply #'delete-objects object cascading-delete-refs)))

(deftransaction change-slot-values (object &rest slots-and-values)
  (when object
    (loop for (slot value) on slots-and-values by #'cddr
          do (setf (slot-value object slot) value))))

(defmethod prepare-for-snapshot (object)
  nil)
  

(defun find-store-object (id-or-name &key (class 'store-object) query-function key-slot-name)
  "Mock up implementation of find-store-object API as in the old datastore"
  (unless id-or-name
    (error "can't search a store object with null key"))
  (when (stringp id-or-name)
    (multiple-value-bind (value end) (parse-integer id-or-name :junk-allowed t)
      (when (and value
		 (eql end (length id-or-name)))
	(setq id-or-name value))))
  (let ((result (cond
		  ((numberp id-or-name)
		   (store-object-with-id id-or-name))
		  (t
		   (cond
		     (query-function
		      (funcall query-function id-or-name))
		     ((eq class 't)
		      (error "can't search for store object by name without class specified"))
		     (t
		      (let ((index (bknr.indices::class-slot-index (find-class class) key-slot-name)))
			(when index
			  (index-get index id-or-name)))))))))
    (unless (or (null result)
		(typep result class))
      (error "Object ~A is not of wanted type ~A." result class))
    result))

(deftransaction store-object-add-keywords (object slot keywords)
  (setf (slot-value object slot)
	(union (slot-value object slot)
	       keywords)))

(deftransaction store-object-remove-keywords (object slot keywords)
  (setf (slot-value object slot)
	(set-difference (slot-value object slot) keywords)))

(deftransaction store-object-set-keywords (object slot keywords)
  (setf (slot-value object slot) keywords))

(defmethod find-refs ((object store-object))
  "Find references to the given OBJECT in all store-objects, traversing both single valued and list valued slots."
  (remove-if-not
   (lambda (candidate)
     (find-if (lambda (slotd)
		(and (slot-boundp candidate (slot-definition-name slotd))
		     (let ((slot-value (slot-value candidate (slot-definition-name slotd))))
		       (or (eq object slot-value)
			   (and (listp slot-value)
				(find object slot-value))))))
	      (class-slots (class-of candidate))))
   (class-instances 'store-object)))

(pushnew :mop-store cl:*features*)
