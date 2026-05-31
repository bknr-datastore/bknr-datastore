;;;; Tests for the replication hooks: commit-observer and persistent LSN.
(in-package :bknr.datastore)

(5am:in-suite :bknr.datastore)

;;; --- Fixtures ---

(defclass repl-test-object (store-object)
  ((value :initarg :value :accessor repl-test-object-value :initform nil))
  (:metaclass persistent-class))

(deftransaction repl-test-set (object value)
  (setf (slot-value object 'value) value))

(defun make-test-store-directory ()
  "A fresh, unique, empty directory for a test store."
  (ensure-directories-exist
   (merge-pathnames (make-pathname :directory (list :relative "bknr-repl-test"
                                                    (string (gensym "STORE"))))
                    (uiop:temporary-directory))))

(defmacro with-fresh-store ((store-var) &body body)
  "Run BODY with a fresh store bound to STORE-VAR, cleaning up the directory after."
  (let ((dir (gensym "DIR")))
    `(let ((,dir (make-test-store-directory)))
       (unwind-protect
            (let ((,store-var (make-instance 'store :directory ,dir)))
              (unwind-protect (progn ,@body)
                (when (boundp '*store*) (close-store))))
         (uiop:delete-directory-tree ,dir :validate t :if-does-not-exist :ignore)))))

(defun encode-to-bytes (object)
  "Encode OBJECT to an (unsigned-byte 8) vector, the way the commit path does internally."
  (let ((buffer (flex:make-in-memory-output-stream)))
    (encode object buffer)
    (flex:get-output-stream-sequence buffer)))

;;; --- Commit-observer ---

(5am:test commit-observer.receives
  "Observers are called for each committed transaction with a strictly increasing LSN."
  (with-fresh-store (store)
    (let ((events '()))
      (add-commit-observer
       (lambda (s txn bytes lsn)
         (declare (ignore s))
         (push (list (type-of txn) (length bytes) lsn) events))
       store)
      (let ((obj (make-instance 'repl-test-object :value 1)))   ; logged as one txn
        (with-transaction () (setf (repl-test-object-value obj) 2)))  ; one anonymous txn
      (setf events (nreverse events))
      (5am:is (>= (length events) 2))
      (5am:is (apply #'< (mapcar #'third events)))       ; LSN strictly increasing
      (5am:is (every #'plusp (mapcar #'second events)))))) ; non-empty bytes

(5am:test commit-observer.bytes-match-encoding
  "The bytes the observer receives are exactly the encoding of the transaction."
  (with-fresh-store (store)
    (let ((captured '()))
      (add-commit-observer
       (lambda (s txn bytes lsn)
         (declare (ignore s lsn))
         (push (cons txn bytes) captured))
       store)
      (make-instance 'repl-test-object :value 7)
      (5am:is (= 1 (length captured)))
      (destructuring-bind (txn . bytes) (first captured)
        (5am:is (equalp bytes (encode-to-bytes txn)))))))

(5am:test commit-observer.remove
  "After remove-commit-observer the observer is no longer called."
  (with-fresh-store (store)
    (let ((count 0))
      (flet ((obs (s txn bytes lsn)
               (declare (ignore s txn bytes lsn))
               (incf count)))
        (add-commit-observer #'obs store)
        (make-instance 'repl-test-object :value 1)
        (remove-commit-observer #'obs store)
        (make-instance 'repl-test-object :value 2)
        (5am:is (= 1 count))))))

;;; --- Persistent LSN ---

(5am:test lsn.survives-snapshot-and-restore
  "The counter is persisted at snapshot and read back on restore."
  (let ((dir (make-test-store-directory)) counter-before)
    (unwind-protect
         (progn
           (let ((store (make-instance 'store :directory dir)))
             (make-instance 'repl-test-object :value 1)
             (make-instance 'repl-test-object :value 2)
             (setf counter-before (store-transaction-counter store))
             (5am:is (>= counter-before 2))
             (snapshot-store store)
             (close-store))
           (let ((store2 (make-instance 'store :directory dir)))
             (5am:is (>= (store-transaction-counter store2) counter-before))
             (close-store)))
      (uiop:delete-directory-tree dir :validate t :if-does-not-exist :ignore))))

(5am:test lsn.consistent-after-replay
  "After snapshot + more commits + reopen the counter is reproduced identically."
  (let ((dir (make-test-store-directory)) counter-at-close)
    (unwind-protect
         (progn
           (let ((store (make-instance 'store :directory dir)))
             (make-instance 'repl-test-object :value 1)
             (snapshot-store store)                       ; baseline persisted
             (make-instance 'repl-test-object :value 2)   ; post-snapshot records in the new log
             (make-instance 'repl-test-object :value 3)
             (setf counter-at-close (store-transaction-counter store))
             (close-store))
           (let ((store2 (make-instance 'store :directory dir)))
             (5am:is (= counter-at-close (store-transaction-counter store2)))
             (close-store)))
      (uiop:delete-directory-tree dir :validate t :if-does-not-exist :ignore))))

(5am:test observer.does-not-corrupt-restore
  "With an observer registered the state is still restored correctly."
  (let ((dir (make-test-store-directory)) id)
    (unwind-protect
         (progn
           (let ((store (make-instance 'store :directory dir)))
             (add-commit-observer (lambda (&rest args) (declare (ignore args))) store)
             (setf id (store-object-id (make-instance 'repl-test-object :value 99)))
             (close-store))
           (let ((store2 (make-instance 'store :directory dir)))
             (declare (ignorable store2))
             (let ((obj (store-object-with-id id)))
               (5am:is (not (null obj)))
               (5am:is (= 99 (repl-test-object-value obj))))
             (close-store)))
      (uiop:delete-directory-tree dir :validate t :if-does-not-exist :ignore))))
