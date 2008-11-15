(in-package :bknr.datastore)

(defparameter *ignore-slots* '(bknr.datastore::id bknr.indices::destroyed-p))

(defmacro with-json-ignore-slots ((&rest slots) &body body)
  `(let ((*ignore-slots* (append *ignore-slots* ,slots)))
     ,@body))

(defmethod json:encode ((object store-object) &optional (stream *standard-output*))
  (json:with-output (stream)
    (json:with-object ()
      (dolist (slotdef (closer-mop:class-slots (class-of object)))
        (when (and (slot-boundp object (closer-mop:slot-definition-name slotdef))
                   (not (find (closer-mop:slot-definition-name slotdef) *ignore-slots*)))
          (json:encode-object-element (closer-mop:slot-definition-name slotdef)
                                      (slot-value object (closer-mop:slot-definition-name slotdef))))))))
