(in-package :bknr.datastore)

(defclass convert-store-object-subsystem (store-object-subsystem)
  ())

(defvar *snapshot-store* nil)
(defvar *object-count* nil)

(defun snapshot-eval (exp)
  (declare (optimize (speed 3)))
  (if (consp exp)
      (case (car exp)
	(create-object
	 (incf *object-count*)
	 (let ((object (apply #'make-instance (second exp)
			      (loop for (slot value) on (nthcdr 2 exp) by #'cddr
				    collect slot
				    collect (snapshot-eval value)))))
	   object))
	(set-slots
	 (let ((obj #+nil(gethash (second exp) *snapshot-id-index*)
		    (store-object-with-id (second exp))))
	   (loop for (slot value) on (nthcdr 2 exp) by #'cddr
		 do (setf (slot-value obj slot) (snapshot-eval value)))
	   ))
	(store-object-with-id
	 (store-object-with-id (second exp))
	 #+nil(gethash (second exp) *snapshot-id-index*))
	(t (eval exp)))
      exp))
  
(defun snapshot-load (s)
  (declare (optimize (speed 3)))
  (let* ((*snapshot-store* *store*)
	 (*package* #.*package*)
	 (*object-count* 0))
    (loop for exp = (read s nil 'eof nil)
	  until (eql exp 'eof)
	  do (snapshot-eval exp)
	  when (= (mod *object-count* 10000) 0)
	  do (warn "; Restored ~A objects~%." *object-count*)
	  (format t "; classes = ~S~%" (all-store-classes)))))

(defmethod restore-subsystem ((store store)
			      (subsystem convert-store-object-subsystem)
			      &key until)
  (declare (ignore until))
  (dolist (class-name (all-store-classes))
    (clear-class-indices (find-class class-name)))
  
  #+nil
  (setf (store-id store) 0)
  #+nil
  (loop for hash being the hash-values of (store-indices store)
	do (clrhash hash))
  (with-open-file (s (merge-pathnames (make-pathname :name "snapshot")
				      (store-directory store))
		     :direction :input)
    (snapshot-load s)))

(defclass convert-store (mp-store)
  ()
  (:default-initargs :subsystems (list (make-instance 'convert-store-object-subsystem)
				       (make-instance 'blob-subsystem
						      :n-blobs-per-directory nil))))

(defmethod restore-store ((store convert-store) &key until)
  (declare (ignore until))
  (warn "restoring ~A" store)
  (with-store-state (:read-only store)
    (with-store-guard (store)
      (with-log-guard (store)
        (close-transaction-log-stream store)
        (let ((transaction-log (merge-pathnames (make-pathname :name "transaction-log")
						(store-directory  store)))
              (*current-session* :restore)
	      (*store* store))
	  (format t "store: ~A ~A~%" store (store-subsystems store))
	  (dolist (subsystem (store-subsystems store))
	    (restore-subsystem store subsystem))
          (when (probe-file transaction-log)
            (warn "loading transaction log ~A" transaction-log)	  
            (load transaction-log)))))))

