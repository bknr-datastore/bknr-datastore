(in-package :bknr.datastore)

(defparameter *ignore-slots* '(bknr.datastore::id bknr.indices::destroyed-p))

(defmacro with-json-ignore-slots ((&rest slots) &body body)
  `(let ((*ignore-slots* (append *ignore-slots* ,slots)))
     ,@body))

(defmethod json:encode-json ((object store-object) stream)
  (let (printed)
    (princ #\{ stream)
    (dolist (slotdef (closer-mop:class-slots (class-of object)))
      (when (and (slot-boundp object (closer-mop:slot-definition-name slotdef))
                 (not (find (closer-mop:slot-definition-name slotdef) *ignore-slots*)))
        (if printed
            (princ #\, stream)
            (setf printed t))
        (json:encode-json (closer-mop:slot-definition-name slotdef) stream)
        (princ #\: stream)
        (json:encode-json (slot-value object (closer-mop:slot-definition-name slotdef)) stream)))
    (princ #\} stream)))
