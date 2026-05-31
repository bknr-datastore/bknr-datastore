;;;; Transport-agnostic core of log-shipping replication for bknr.datastore.
;;;;
;;;; The primary side is a commit-observer (see BKNR.DATASTORE:ADD-COMMIT-OBSERVER)
;;;; that writes the raw encoded transaction bytes to a sink stream.  Because the
;;;; on-disk transaction format is self-delimiting, the resulting byte stream is
;;;; itself a valid transaction stream.
;;;;
;;;; The standby side reads that stream and applies each transaction with the
;;;; restore machinery, advancing the LSN and firing apply observers.  Apply
;;;; observers are the replica-side counterpart of commit observers: a primary
;;;; commits through the commit path, but a standby applies via EXECUTE-UNLOGGED,
;;;; which the commit path never touches -- so the standby needs its own hook.
;;;;
;;;; This file deliberately contains no networking: the same APPLY-REPLICATION-STREAM
;;;; drives an in-memory pipe, a file tail, or a socket.  The TCP transport in
;;;; network.lisp is a thin layer on top.

(in-package :bknr.datastore.replication)

;;;; ---------------------------------------------------------------------------
;;;; Primary side

(defun make-stream-replication-observer (output-stream)
  "Return a commit-observer that writes the raw encoded bytes of each committed
transaction to OUTPUT-STREAM.  Register it on a primary store with
BKNR.DATASTORE:ADD-COMMIT-OBSERVER.

The bytes written are the concatenation of self-delimiting transaction records,
exactly as they appear in the transaction log, so a standby can decode them one
at a time with APPLY-REPLICATION-STREAM."
  (lambda (store transaction bytes lsn)
    (declare (ignore store transaction lsn))
    (write-sequence bytes output-stream)))

;;;; ---------------------------------------------------------------------------
;;;; Standby side: apply observers (the replica-side hook)

(defvar *apply-observers* (make-hash-table :test 'eq)
  "Maps a replica STORE to the list of functions to run after each applied
transaction.")

(defun add-apply-observer (function &optional (store *store*))
  "Register FUNCTION to run after each transaction is applied to the replica
STORE.  FUNCTION is called with (store transaction lsn).  Returns FUNCTION.

Apply observers are the replica-side counterpart of bknr.datastore's commit
observers: on a primary, transactions go through the commit path; on a replica
they are applied via the restore machinery, which the commit path never touches.
Use these hooks to push server-sent events, maintain derived or materialized
views, feed external indexes (change-data-capture), or drive streaming
computation from the live object graph.

Keep observers cheap and non-blocking -- they run inline in the apply loop, so
slow work should be handed off to a queue."
  (pushnew function (gethash store *apply-observers*))
  function)

(defun remove-apply-observer (function &optional (store *store*))
  "Remove FUNCTION from the apply observers of replica STORE."
  (setf (gethash store *apply-observers*)
        (remove function (gethash store *apply-observers*)))
  function)

(defgeneric transaction-applied (store transaction lsn)
  (:documentation "Called after TRANSACTION has been applied to replica STORE,
with its log sequence number LSN.  The default method invokes each function
registered with ADD-APPLY-OBSERVER, in registration-independent order.")
  (:method ((store store) transaction lsn)
    (dolist (observer (gethash store *apply-observers*))
      (funcall observer store transaction lsn))))

(defun replica-applied-lsn (&optional (store *store*))
  "The LSN of the last transaction applied to STORE.  On a replica, comparing
this with the primary's LSN gives the replication lag."
  (store-transaction-counter store))

;;;; ---------------------------------------------------------------------------
;;;; The apply loop

(defun allowed-transaction-function-p (symbol)
  "Whether a transaction function named SYMBOL may be applied from a replication
stream.  Restricts apply to the DEFTRANSACTION naming convention (TX-*) plus
MAKE-INSTANCE (used to create store objects), so a hostile primary cannot drive
the replica to call arbitrary functions in the image."
  (and (symbolp symbol)
       (or (eq symbol 'cl:make-instance)
           (let ((name (symbol-name symbol)))
             (and (>= (length name) 3) (string= "TX-" name :end2 3))))))

(defun %drain-stream (in)
  "Read all remaining bytes of the in-memory input stream IN into a fresh octet
vector."
  (let ((out (flex:make-in-memory-output-stream))
        (buf (make-array 4096 :element-type '(unsigned-byte 8))))
    (loop for n = (read-sequence buf in)
          while (plusp n) do (write-sequence buf out :end n))
    (flex:get-output-stream-sequence out)))

(defun %check-transaction-allowed (transaction)
  "Signal an error if TRANSACTION names a function not on the apply allowlist.
An anonymous transaction group carries no function-symbol but holds a buffer of
sub-transactions; we drain it, recursively validate every sub-transaction, then
restore the buffer so EXECUTE-UNLOGGED can still replay it -- otherwise a hostile
primary could smuggle a disallowed call inside a group."
  (cond
    ((typep transaction 'bknr.datastore::anonymous-transaction)
     (let ((bytes (%drain-stream (bknr.datastore::anonymous-transaction-log-buffer transaction))))
       (let ((in (flex:make-in-memory-input-stream bytes)))
         (handler-case (loop (%check-transaction-allowed (bknr.datastore::decode in)))
           (end-of-file ())))
       (setf (bknr.datastore::anonymous-transaction-log-buffer transaction)
             (flex:make-in-memory-input-stream bytes))))
    (t
     (let ((fsym (and (typep transaction 'bknr.datastore::transaction)
                      (ignore-errors (transaction-function-symbol transaction)))))
       (when (and fsym (not (allowed-transaction-function-p fsym)))
         (error "refusing to apply disallowed transaction function ~S" fsym))))))

(defun apply-replication-stream (store stream)
  "Read self-delimiting transaction records from STREAM and apply each to STORE,
advancing STORE's LSN and firing apply observers in order.  STORE is placed in
restore state for the duration.  Returns the number of transactions applied.

STREAM is consumed until end of file; for a live socket this blocks, streaming
updates as they arrive.  This is the transport-agnostic core of replication --
the same function drives an in-memory pipe, a file tail, or a network socket.

The store's code must define every transaction function referenced in the
stream, otherwise applying signals UNDEFINED-TRANSACTION."
  (let ((bknr.datastore:*store* store)
        (applied 0)
        (old-state (store-state store)))
    (setf (store-state store) :restore)
    (unwind-protect
         (handler-case
             (loop
               (let ((transaction (bknr.datastore::decode stream)))
                 (%check-transaction-allowed transaction)
                 (bknr.datastore::execute-unlogged transaction)
                 (incf (store-transaction-counter store))
                 (incf applied)
                 (transaction-applied store transaction
                                      (store-transaction-counter store))))
           (end-of-file () applied))
      (setf (store-state store) old-state))))
