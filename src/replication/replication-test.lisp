(in-package :cl-user)

(defpackage :bknr.datastore.replication.test
  (:use :cl :bknr.datastore :bknr.datastore.replication))

(in-package :bknr.datastore.replication.test)

(5am:def-suite :bknr.datastore.replication)
(5am:in-suite :bknr.datastore.replication)

;;; --- Fixtures ---

(defclass replicated-counter (store-object)
  ((value :initarg :value :accessor counter-value :initform 0))
  (:metaclass persistent-class))

(deftransaction set-counter (object value)
  (setf (slot-value object 'value) value))

(defun temp-store-directory (tag)
  (ensure-directories-exist
   (merge-pathnames (make-pathname :directory (list :relative "bknr-repl-mod"
                                                    (string (gensym tag))))
                    (uiop:temporary-directory))))

;;; --- Tests ---

(5am:test replication.basic-apply
  "Transactions captured on a primary are reproduced on a fresh standby, the
apply observers fire in LSN order, and the replica LSN matches the work done."
  (let ((pdir (temp-store-directory "P"))
        (sdir (temp-store-directory "S"))
        (wire (flex:make-in-memory-output-stream))
        (ids nil))
    (unwind-protect
         (progn
           ;; --- primary: capture the replication stream into WIRE ---
           (let ((primary (make-instance 'store :directory pdir :make-default nil)))
             (let ((bknr.datastore:*store* primary))
               (add-commit-observer (make-stream-replication-observer wire) primary)
               (let ((a (make-instance 'replicated-counter :value 10))
                     (b (make-instance 'replicated-counter :value 20)))
                 (set-counter a 11)
                 (setf ids (list (store-object-id a) (store-object-id b))))))
           ;; --- standby: apply the captured stream onto a fresh store ---
           (let ((events nil)
                 (standby (make-instance 'store :directory sdir :make-default nil)))
             (flet ((observe (s txn lsn)
                      (declare (ignore s txn))
                      (push lsn events)))
               (add-apply-observer #'observe standby)
               (let* ((bytes (flex:get-output-stream-sequence wire))
                      (in (flex:make-in-memory-input-stream bytes))
                      (n (apply-replication-stream standby in)))
                 (setf events (nreverse events))
                 (5am:is (= 3 n))                       ; 2 make-instance + 1 set-counter
                 (5am:is (= n (length events)))         ; one apply-observer call per txn
                 (5am:is (apply #'< events))            ; LSN strictly increasing
                 (5am:is (= n (replica-applied-lsn standby)))
                 ;; state replicated with identical ids and values
                 (let ((bknr.datastore:*store* standby))
                   (destructuring-bind (id-a id-b) ids
                     (let ((a (store-object-with-id id-a))
                           (b (store-object-with-id id-b)))
                       (5am:is (not (null a)))
                       (5am:is (not (null b)))
                       (5am:is (= 11 (counter-value a)))
                       (5am:is (= 20 (counter-value b))))))))))
      (uiop:delete-directory-tree pdir :validate t :if-does-not-exist :ignore)
      (uiop:delete-directory-tree sdir :validate t :if-does-not-exist :ignore))))

(5am:test replication.delete-applies
  "A delete transaction replicates: the deleted object is gone on the standby
while the kept object survives."
  (let ((pdir (temp-store-directory "PD"))
        (sdir (temp-store-directory "SD"))
        (wire (flex:make-in-memory-output-stream))
        (kept-id nil) (gone-id nil))
    (unwind-protect
         (progn
           ;; primary: create two objects, delete one, capturing the stream
           (let ((primary (make-instance 'store :directory pdir :make-default nil)))
             (let ((bknr.datastore:*store* primary))
               (add-commit-observer (make-stream-replication-observer wire) primary)
               (let ((a (make-instance 'replicated-counter :value 1))
                     (b (make-instance 'replicated-counter :value 2)))
                 (setf kept-id (store-object-id a)
                       gone-id (store-object-id b))
                 (delete-object b))))
           ;; standby: apply the stream onto a fresh store
           (let ((standby (make-instance 'store :directory sdir :make-default nil)))
             (let ((in (flex:make-in-memory-input-stream
                        (flex:get-output-stream-sequence wire))))
               (apply-replication-stream standby in))
             (let ((bknr.datastore:*store* standby))
               (5am:is (not (null (store-object-with-id kept-id))))   ; survivor present
               (5am:is (null (store-object-with-id gone-id))))))      ; deleted object gone
      (uiop:delete-directory-tree pdir :validate t :if-does-not-exist :ignore)
      (uiop:delete-directory-tree sdir :validate t :if-does-not-exist :ignore))))

;;; --- Network-layer unit tests (single store / pure helpers) ---

(5am:test framing.roundtrip
  "Wire framing helpers round-trip integers (incl. >32 bit) and UTF-8 strings."
  (let ((out (flex:make-in-memory-output-stream)))
    (bknr.replication::%wu 1 4 out)
    (bknr.replication::%wu 9999999999 8 out)        ; needs 8 bytes
    (bknr.replication::%wstr "héllo-æ" out)
    (let ((in (flex:make-in-memory-input-stream (flex:get-output-stream-sequence out))))
      (5am:is (= 1 (bknr.replication::%ru 4 in)))
      (5am:is (= 9999999999 (bknr.replication::%ru 8 in)))
      (5am:is (string= "héllo-æ" (bknr.replication::%rstr in))))))

(5am:test log-tail.skips-records
  "%send-log-tail emits exactly the records after TO-SKIP, framed as [u64 len][bytes]."
  (let ((dir (temp-store-directory "LD")))
    (unwind-protect
         (let ((store (make-instance 'store :directory dir :make-default nil)))
           (let ((bknr.datastore:*store* store))
             (dotimes (i 3) (make-instance 'replicated-counter :value i))   ; LSNs 1,2,3
             (flet ((tail-count (to-skip)
                      (let ((out (flex:make-in-memory-output-stream))
                            (end (bknr.replication::%log-length store)))
                        (bknr.replication::%send-log-tail store out to-skip end)
                        (let* ((in (flex:make-in-memory-input-stream
                                    (flex:get-output-stream-sequence out)))
                               (len (bknr.replication::%ru 8 in))
                               (data (bknr.replication::%rn len in))
                               (din (flex:make-in-memory-input-stream data)) (n 0))
                          (handler-case (loop (bknr.datastore::decode din) (incf n))
                            (end-of-file ()))
                          n))))
               (5am:is (= 3 (tail-count 0)))    ; all
               (5am:is (= 2 (tail-count 1)))    ; after #1
               (5am:is (= 0 (tail-count 3)))))) ; caught up
      (uiop:delete-directory-tree dir :validate t :if-does-not-exist :ignore))))

(5am:test backpressure.drops-slow-client
  "A client whose send queue exceeds the cap is dropped (marked dead), never
blocking the enqueue."
  (let ((c (make-instance 'bknr.replication::replica-client :socket nil :stream nil)))
    (bknr.replication::client-enqueue c (make-array 10 :element-type '(unsigned-byte 8)) 100)
    (5am:is (bknr.replication::client-alive-p c))           ; under cap
    (bknr.replication::client-enqueue c (make-array 200 :element-type '(unsigned-byte 8)) 100)
    (5am:is (not (bknr.replication::client-alive-p c)))))   ; over cap -> dropped

(5am:test log-baseline.snapshot
  "The current-log baseline is 0 before a snapshot and equals the LSN at snapshot
time afterwards (so a replica behind it must re-bootstrap, not resume)."
  (let ((dir (temp-store-directory "LB")))
    (unwind-protect
         (let ((store (make-instance 'store :directory dir :make-default nil)))
           (let ((bknr.datastore:*store* store))
             (5am:is (= 0 (bknr.replication::%current-log-baseline store)))
             (make-instance 'replicated-counter :value 1)
             (make-instance 'replicated-counter :value 2)
             (bknr.datastore::snapshot-store store)
             (5am:is (= 2 (bknr.replication::%current-log-baseline store)))))
      (uiop:delete-directory-tree dir :validate t :if-does-not-exist :ignore))))

(5am:test epoch.stable-and-distinct
  "The primary epoch is stable for a given store directory (persisted in the
root, surviving reopen) and differs for a different store."
  (let ((dir1 (temp-store-directory "EP1"))
        (dir2 (temp-store-directory "EP2")))
    (unwind-protect
         (let ((e1 (bknr.replication::%store-epoch
                    (make-instance 'store :directory dir1 :make-default nil))))
           ;; same dir, reopened -> same epoch
           (5am:is (string= e1 (bknr.replication::%store-epoch
                                (make-instance 'store :directory dir1 :make-default nil))))
           ;; different dir -> different epoch
           (5am:is (not (string= e1 (bknr.replication::%store-epoch
                                     (make-instance 'store :directory dir2 :make-default nil))))))
      (uiop:delete-directory-tree dir1 :validate t :if-does-not-exist :ignore)
      (uiop:delete-directory-tree dir2 :validate t :if-does-not-exist :ignore))))

(5am:test sync-mode.decision
  "Resume vs bootstrap decision: resume only on the same epoch with the LSN still
within the current log; bootstrap on epoch mismatch or when behind a snapshot."
  (5am:is (eq :bootstrap (bknr.replication::%sync-mode "old" 5 "new" 0)))  ; epoch changed
  (5am:is (eq :bootstrap (bknr.replication::%sync-mode ""    0 "e"   0)))  ; fresh replica
  (5am:is (eq :resume    (bknr.replication::%sync-mode "e"   5 "e"   3)))  ; lsn > baseline
  (5am:is (eq :resume    (bknr.replication::%sync-mode "e"   3 "e"   3)))  ; lsn = baseline
  (5am:is (eq :bootstrap (bknr.replication::%sync-mode "e"   2 "e"   3)))) ; behind a snapshot

(5am:test schema-skew.undefined-transaction-signals
  "Applying a transaction whose function is not loaded signals an error (clean
failure), rather than silently corrupting the replica."
  (let ((dir (temp-store-directory "SK")))
    (unwind-protect
         (let ((store (make-instance 'store :directory dir :make-default nil)))
           (let* ((bknr.datastore:*store* store)
                  (txn (make-instance 'bknr.datastore::transaction
                                      :function-symbol 'no-such-tx-function-xyz
                                      :timestamp 0 :args nil))
                  (bytes (let ((b (flex:make-in-memory-output-stream)))
                           (bknr.datastore::encode txn b)
                           (flex:get-output-stream-sequence b))))
             (5am:signals error
               (apply-replication-stream store (flex:make-in-memory-input-stream bytes)))))
      (uiop:delete-directory-tree dir :validate t :if-does-not-exist :ignore))))

(5am:test path-traversal.rejected
  "Replicated file names are reduced to a safe basename; traversal/absolute names
are rejected so a hostile primary cannot write outside the replica directory."
  (5am:is (string= "transaction-log"
                   (bknr.replication::%safe-replicated-name "transaction-log")))
  (5am:is (string= "store-object-subsystem-snapshot"
                   (bknr.replication::%safe-replicated-name "store-object-subsystem-snapshot")))
  (5am:signals error (bknr.replication::%safe-replicated-name "../../etc/passwd"))
  (5am:signals error (bknr.replication::%safe-replicated-name "/etc/passwd"))
  (5am:signals error (bknr.replication::%safe-replicated-name "sub/dir/x")))

;;; --- Authentication ---

(defun %sec (s) (and s (trivial-utf-8:string-to-utf-8-bytes s)))

(5am:test auth.hmac-and-ct-equal
  "HMAC is deterministic and key-dependent; constant-time compare is correct."
  (let ((k (%sec "key")) (m (%sec "msg")))
    (5am:is (bknr.replication::%ct-equal (bknr.replication::%hmac k m)
                                         (bknr.replication::%hmac k m)))
    (5am:is (not (bknr.replication::%ct-equal (bknr.replication::%hmac k m)
                                              (bknr.replication::%hmac (%sec "other") m))))
    (5am:is (bknr.replication::%ct-equal #(1 2 3) #(1 2 3)))
    (5am:is (not (bknr.replication::%ct-equal #(1 2 3) #(1 2 4))))
    (5am:is (not (bknr.replication::%ct-equal #(1 2) #(1 2 3))))))

(defun %run-handshake (server-secret client-secret)
  "Run the real mutual handshake over a localhost socket pair; return
(values server-outcome client-outcome), each :ok or the signaled condition."
  (let* ((listener (usocket:socket-listen "127.0.0.1" 0 :reuse-address t
                                                        :element-type '(unsigned-byte 8)))
         (port (usocket:get-local-port listener))
         sres cres)
    (unwind-protect
         (let* ((cs (usocket:socket-connect "127.0.0.1" port :element-type '(unsigned-byte 8)))
                (ss (usocket:socket-accept listener :element-type '(unsigned-byte 8))))
           (unwind-protect
                (let ((th (bordeaux-threads:make-thread
                           (lambda ()
                             (setf sres (handler-case
                                            (progn (bknr.replication::server-authenticate
                                                    (usocket:socket-stream ss) server-secret)
                                                   :ok)
                                          (error (e) e)))))))
                  (setf cres (handler-case
                                 (progn (bknr.replication::client-authenticate
                                         (usocket:socket-stream cs) client-secret)
                                        :ok)
                               (error (e) e)))
                  (bordeaux-threads:join-thread th)
                  (values sres cres))
             (ignore-errors (usocket:socket-close cs))
             (ignore-errors (usocket:socket-close ss))))
      (ignore-errors (usocket:socket-close listener)))))

(5am:test auth.matching-secret-succeeds
  (multiple-value-bind (s c) (%run-handshake (%sec "shared") (%sec "shared"))
    (5am:is (eq :ok s))
    (5am:is (eq :ok c))))

(5am:test auth.no-secret-succeeds
  "With no secret on either side, the handshake passes (local/demo mode)."
  (multiple-value-bind (s c) (%run-handshake nil nil)
    (5am:is (eq :ok s))
    (5am:is (eq :ok c))))

(5am:test auth.server-rejects-unauthenticated-client
  "A secret-configured server rejects a client with no secret."
  (multiple-value-bind (s c) (%run-handshake (%sec "shared") nil)
    (declare (ignore c))
    (5am:is (typep s 'bknr.replication:replication-auth-error))))

(5am:test auth.client-rejects-unauthenticated-server
  "A secret-configured client refuses a server with no secret (anti-downgrade)."
  (multiple-value-bind (s c) (%run-handshake nil (%sec "shared"))
    (declare (ignore s))
    (5am:is (typep c 'bknr.replication:replication-auth-error))))

(5am:test auth.mismatched-secret-rejected
  "Different secrets fail on both sides."
  (multiple-value-bind (s c) (%run-handshake (%sec "a") (%sec "b"))
    (5am:is (typep s 'bknr.replication:replication-auth-error))
    (5am:is (typep c 'bknr.replication:replication-auth-error))))

(5am:test secret.resolution
  "Explicit secret -> octets; empty -> NIL; a configured-but-missing secret file
errors rather than silently downgrading."
  (5am:is (null (bknr.replication::resolve-secret "")))
  (5am:is (equalp (trivial-utf-8:string-to-utf-8-bytes "x")
                  (bknr.replication::resolve-secret "x")))
  (let ((missing (merge-pathnames "no-such-secret-file"
                                  (uiop:temporary-directory))))
    (sb-posix:setenv "BKNR_REPL_SECRET_FILE" (namestring missing) 1)
    (unwind-protect
         (5am:signals error (bknr.replication::resolve-secret nil))
      (sb-posix:unsetenv "BKNR_REPL_SECRET_FILE"))))

;;; --- Allowlist recursion + decoder caps ---

(5am:test allowlist.anonymous-subtx-checked
  "A disallowed function smuggled inside an anonymous transaction group is rejected."
  (let ((sub (make-instance 'bknr.datastore::transaction
                            :function-symbol 'evil-fn-not-allowed :timestamp 0 :args nil))
        (buf (flex:make-in-memory-output-stream)))
    (bknr.datastore::encode sub buf)
    (let ((anon (make-instance 'bknr.datastore::anonymous-transaction
                               :label "x"
                               :log-buffer (flex:make-in-memory-input-stream
                                            (flex:get-output-stream-sequence buf)))))
      (5am:signals error (bknr.datastore.replication::%check-transaction-allowed anon)))))

(5am:test allowlist.anonymous-allowed-replayable
  "An anonymous group of allowed sub-transactions passes the check, and its buffer
is restored afterwards so it stays replayable (drain/restore is non-destructive)."
  (let ((sub (make-instance 'bknr.datastore::transaction
                            :function-symbol 'cl:make-instance :timestamp 0
                            :args (list 'replicated-counter :id 0 :value 1)))
        (buf (flex:make-in-memory-output-stream)))
    (bknr.datastore::encode sub buf)
    (let ((anon (make-instance 'bknr.datastore::anonymous-transaction
                               :label "x"
                               :log-buffer (flex:make-in-memory-input-stream
                                            (flex:get-output-stream-sequence buf)))))
      (5am:finishes (bknr.datastore.replication::%check-transaction-allowed anon))
      ;; buffer restored: the sub-transaction is still decodable
      (let ((again (bknr.datastore::decode
                    (bknr.datastore::anonymous-transaction-log-buffer anon))))
        (5am:is (eq 'make-instance (transaction-function-symbol again)))))))

(5am:test decode.length-cap
  "An oversized decoded length is rejected before allocation."
  (let ((bknr.datastore::*max-decoded-length* 100))
    (5am:is (= 50 (bknr.datastore::%check-decoded-length 50)))
    (5am:signals error (bknr.datastore::%check-decoded-length 101))))
