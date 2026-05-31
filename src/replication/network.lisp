;;;; Network transport for bknr.datastore replication.
;;;;
;;;; A replica connects and sends a HELLO (the primary epoch it last synced from
;;;; and its last applied LSN).  The primary replies with its epoch and a mode:
;;;;
;;;;   * BOOTSTRAP -- send the full base state (the current/ directory: snapshot
;;;;     + log + lsn baseline + random-state) so a fresh replica can RESTORE to
;;;;     the exact current state.  Chosen when the replica is fresh, points at a
;;;;     different primary epoch, or has fallen behind the current log (the older
;;;;     log was rotated away by a snapshot).
;;;;
;;;;   * RESUME -- send only the log delta after the replica's LSN.  Chosen when
;;;;     the replica is on the same epoch and its LSN is still within the current
;;;;     log.  Keeps the replica's in-memory state and LSN in lockstep.
;;;;
;;;; After the base/delta, the primary streams live transactions via a commit
;;;; observer.  RUN-REPLICA drives the connect -> bootstrap/resume -> apply loop
;;;; with reconnect + exponential backoff; policy (reconnect?, backoff, hooks) is
;;;; the caller's.
;;;;
;;;; The base/delta is transferred off the commit lock by per-replica writer
;;;; threads with bounded queues (non-blocking bootstrap + backpressure).  Peers
;;;; mutually authenticate with a shared secret (auth.lisp); confidentiality is
;;;; optional via the :make-stream transport seam (the bknr.datastore.replication
;;;; /tls add-on wraps the connection in TLS).  Known limitation: blob-subsystem
;;;; files are not transferred.

(in-package :bknr.datastore.replication)

;;;; ---------------------------------------------------------------------------
;;;; Wire framing lives in wire.lisp; this file adds the name sanitizer.

(defun %safe-replicated-name (raw)
  "Reduce a file name received over the wire to a bare basename, rejecting any
directory component, absolute path, or NUL -- preventing path traversal when the
replica writes received base files to disk."
  (let ((pn (ignore-errors (pathname raw))))
    (when (or (null pn) (null (pathname-name pn)) (pathname-directory pn)
              (member raw '("." "..") :test #'equal)
              (find #\/ raw) (find #\\ raw) (find #\Nul raw))
      (error "illegal replicated file name: ~S" raw))
    (file-namestring pn)))

;;;; ---------------------------------------------------------------------------
;;;; Primary-side identity, baseline, and delta

(defun %store-epoch (store)
  "A stable identity for this primary's data instance.  Persisted in the store
ROOT (so it survives snapshots, which rotate current/), and regenerated whenever
the store directory is recreated.  Used by a replica to detect that it is now
talking to a different/restarted primary and must re-bootstrap."
  (let ((path (merge-pathnames "replication-epoch"
                               (bknr.datastore::store-directory store))))
    (if (probe-file path)
        (with-open-file (f path) (read-line f))
        (let ((epoch (format nil "~36R-~36R"
                             (get-universal-time)
                             (random (expt 2 48) (make-random-state t)))))
          (with-open-file (f path :direction :output
                                  :if-exists :supersede :if-does-not-exist :create)
            (write-line epoch f))
          epoch))))

(defun %current-log-baseline (store)
  "LSN before the first record in the current transaction log (0 if no snapshot
has rotated the log).  Equals the persisted transaction-id written at snapshot."
  (let ((path (bknr.datastore::store-transaction-id-pathname store)))
    (if (probe-file path)
        (with-open-file (f path) (with-standard-io-syntax (read f)))
        0)))

(defun %base-files (store)
  "Regular files in the store's current/ directory that constitute its state."
  (remove-if-not #'pathname-name
                 (directory (merge-pathnames (make-pathname :name :wild :type :wild)
                                             (bknr.datastore::store-current-directory store)))))

(defun %log-pathname (store)
  (bknr.datastore::store-transaction-log-pathname store))

(defun %log-length (store)
  "Current byte length of the transaction log (0 if it does not exist yet).
Captured under the log guard to mark the exact point a base/delta ends and live
fan-out begins."
  (let ((path (%log-pathname store)))
    (if (probe-file path)
        (with-open-file (s path :element-type '(unsigned-byte 8)) (file-length s))
        0)))

(defun %send-log-tail (store stream to-skip end)
  "Write a resume delta to STREAM as [u64 len][bytes]: the log bytes after
skipping TO-SKIP records (records the replica already has), up to byte END.
TO-SKIP and END are captured cheaply under the commit lock; the actual skip I/O
runs here in the writer thread, off the lock.  END bounds the read so concurrent
appends are not included."
  (let ((path (%log-pathname store)))
    (if (and (probe-file path) (plusp end))
        (with-open-file (s path :element-type '(unsigned-byte 8))
          (handler-case (dotimes (i to-skip) (bknr.datastore::decode s))
            (end-of-file ()))
          (let* ((start (min end (file-position s)))
                 (len (- end start))
                 (buf (make-array len :element-type '(unsigned-byte 8))))
            (when (plusp len) (read-sequence buf s :end len))
            (%wu len 8 stream)
            (write-sequence buf stream :end len)))
        (%wu 0 8 stream))))

(defun %send-base-prefix (store stream log-len)
  "Send the base for a bootstrap as a uniform sequence of file entries
[u32 name-len][name][u64 data-len][data]..., terminated by a zero name-len:
every current/ file EXCEPT the log in full (snapshots are immutable), then the
log as a bounded prefix [0, LOG-LEN).  None of this needs the commit lock, so the
primary keeps accepting writes during the transfer."
  (let ((logname (file-namestring (%log-pathname store))))
    (dolist (path (%base-files store))
      (unless (equal (file-namestring path) logname)
        (let ((bytes (alexandria:read-file-into-byte-vector path)))
          (%wstr (file-namestring path) stream)
          (%wu (length bytes) 8 stream)
          (write-sequence bytes stream))))
    ;; the log as a file entry, but only the bounded prefix [0, log-len)
    (%wstr logname stream)
    (%wu log-len 8 stream)
    (when (plusp log-len)
      (with-open-file (s (%log-pathname store) :element-type '(unsigned-byte 8))
        (let ((buf (make-array log-len :element-type '(unsigned-byte 8))))
          (read-sequence buf s)
          (write-sequence buf stream))))
    (%wu 0 4 stream)))                            ; terminator

(defun %sync-mode (replica-epoch replica-lsn primary-epoch baseline)
  "Decide how to (re)sync a replica: :RESUME when it is on the same primary epoch
and its LSN is still within the current log (>= the log baseline), otherwise
:BOOTSTRAP -- i.e. when the replica is fresh, points at a different/restarted
primary, or has fallen behind a snapshot that rotated the needed log away."
  (cond ((not (equal replica-epoch primary-epoch)) :bootstrap)
        ((>= replica-lsn baseline) :resume)
        (t :bootstrap)))

;;;; ---------------------------------------------------------------------------
;;;; Primary server

(defparameter *handshake-timeout* 10
  "Seconds to wait for a connecting peer's HELLO before dropping it, so a peer
that connects and stalls cannot pin a handler thread.")

(defparameter *default-max-clients* 256
  "Maximum number of concurrently connected replicas; further connections are
refused, bounding thread/fd use under a connection flood.")

(defparameter *default-max-queue-bytes* (* 64 1024 1024)
  "Per-replica send-queue cap.  A replica whose queue exceeds this (a slow or
stuck consumer) is dropped rather than allowed to block the primary's commits.")

(defclass replication-server ()
  ((store :initarg :store :reader server-store)
   (epoch :initarg :epoch :reader server-epoch)
   (secret :initarg :secret :initform nil :reader server-secret)
   (make-stream :initarg :make-stream :reader server-make-stream
                :documentation "Function (socket) -> bidirectional binary stream;
the pluggable transport seam.  Defaults to the plain socket stream; the TLS
add-on supplies a function that wraps it in an SSL server stream.")
   (listen-socket :initarg :listen-socket :accessor server-listen-socket)
   (clients :initform nil :accessor server-clients
            :documentation "List of live REPLICA-CLIENTs.")
   (clients-lock :initform (bordeaux-threads:make-lock "repl-clients") :reader server-clients-lock)
   (in-flight :initform 0 :accessor server-in-flight
              :documentation "Count of connections currently in the (pre-auth) handshake,
capped at MAX-CLIENTS so an unauthenticated connection flood can't exhaust threads.")
   (max-clients :initarg :max-clients :reader server-max-clients)
   (max-queue-bytes :initarg :max-queue-bytes :reader server-max-queue-bytes)
   (accept-thread :initform nil :accessor server-accept-thread)
   (observer :initform nil :accessor server-observer)))

(defun %plain-stream (socket)
  "Default transport: the socket's own bidirectional binary stream (no TLS)."
  (usocket:socket-stream socket))

;;; Each replica has a bounded send queue drained by its own writer thread.  The
;;; commit path only ENQUEUES (never writes to a socket), so a slow replica can
;;; never block a commit; and the writer thread sends the (large) base outside
;;; the commit lock, so bootstrapping a replica never freezes writes.

(defclass replica-client ()
  ((socket :initarg :socket :reader client-socket)
   (stream :initarg :stream :reader client-stream)
   (initial :initarg :initial :accessor client-initial
            :documentation "Thunk: write the reply header + base/delta (run by the
writer thread, off the commit lock).")
   (queue :initform nil :accessor client-queue)        ; live bytes, LIFO; reversed on drain
   (queued-bytes :initform 0 :accessor client-queued-bytes)
   (lock :initform (bordeaux-threads:make-lock "repl-client") :reader client-lock)
   (cvar :initform (bordeaux-threads:make-condition-variable) :reader client-cvar)
   (alive :initform t :accessor client-alive-p)))

(defun client-kill (client)
  (bordeaux-threads:with-lock-held ((client-lock client))
    (setf (client-alive-p client) nil)
    (bordeaux-threads:condition-notify (client-cvar client)))
  ;; Close the socket outside the lock so a writer parked in blocking I/O
  ;; (not in condition-wait) errors out and the thread exits.
  (ignore-errors (usocket:socket-close (client-socket client))))

(defun client-enqueue (client bytes max-bytes)
  "Append BYTES to CLIENT's send queue.  If the queue would exceed MAX-BYTES the
client is a slow consumer and is dropped.  Never blocks."
  (bordeaux-threads:with-lock-held ((client-lock client))
    (when (client-alive-p client)
      (cond
        ((> (+ (client-queued-bytes client) (length bytes)) max-bytes)
         (setf (client-alive-p client) nil)
         ;; Close now so a writer parked in write-sequence (full TCP buffer --
         ;; exactly the slow consumer we're dropping) errors out immediately
         ;; rather than lingering until TCP eventually fails.
         (ignore-errors (usocket:socket-close (client-socket client))))
        (t
         (push bytes (client-queue client))
         (incf (client-queued-bytes client) (length bytes))))
      (bordeaux-threads:condition-notify (client-cvar client)))))

(defun client-writer (client)
  "Writer thread: send the initial payload (reply + base/delta) off the commit
lock, then drain the live queue until the client dies or errors."
  (unwind-protect
       (handler-case
           (let ((stream (client-stream client)))
             (funcall (client-initial client))           ; reply + base/delta
             (force-output stream)
             (loop
               (let ((batch nil))
                 (bordeaux-threads:with-lock-held ((client-lock client))
                   (loop while (and (client-alive-p client) (null (client-queue client)))
                         do (bordeaux-threads:condition-wait (client-cvar client) (client-lock client)))
                   (when (and (not (client-alive-p client)) (null (client-queue client)))
                     (return))
                   (setf batch (nreverse (client-queue client))
                         (client-queue client) nil
                         (client-queued-bytes client) 0))
                 (dolist (b batch) (write-sequence b stream))
                 (force-output stream))))
         (error () nil))
    (setf (client-alive-p client) nil)
    (ignore-errors (usocket:socket-close (client-socket client)))))

(defun %with-log-guard (store thunk)
  (funcall (bknr.datastore::store-log-guard store) thunk))

(defun %make-fanout-observer (server)
  "Commit-observer: enqueue each committed transaction's bytes to every live
replica (non-blocking), and reap dead ones.  Runs inside the commit log guard,
so registration (which captures the base/live boundary) is serialized with it."
  (lambda (store transaction bytes lsn)
    (declare (ignore store transaction lsn))
    (let (live)
      (bordeaux-threads:with-lock-held ((server-clients-lock server))
        (dolist (c (server-clients server))
          (client-enqueue c bytes (server-max-queue-bytes server))
          (when (client-alive-p c) (push c live)))
        (setf (server-clients server) (nreverse live))))))

(defun %register-client (server client replica-epoch replica-lsn)
  "Under the commit log guard: decide bootstrap vs resume, capture the current log
length (the exact base -> live boundary), build the client's initial-payload
thunk, and add it to the fan-out list -- so every commit from here on is queued
for this client and the base/delta covers exactly up to here.  Only cheap reads
(file length, arithmetic) happen under the lock; all transfer I/O is deferred to
the writer thread via the thunk."
  (%with-log-guard (server-store server)
    (lambda ()
      (let* ((store (server-store server))
             (epoch (server-epoch server))
             (baseline (%current-log-baseline store))
             (mode (%sync-mode replica-epoch replica-lsn epoch baseline))
             (log-len (%log-length store))
             (stream (client-stream client)))
        (flet ((check-not-rotated ()
                 ;; The base/delta is read off-lock; if a snapshot rotated the log
                 ;; since registration the captured offsets are stale -> abort so
                 ;; the replica reconnects and re-bootstraps cleanly.
                 (unless (= (%current-log-baseline store) baseline)
                   (error "log rotated during transfer; replica will reconnect"))))
          (setf (client-initial client)
                (ecase mode
                  (:bootstrap
                   (lambda ()
                     (%wstr epoch stream) (write-byte 0 stream)
                     (check-not-rotated)
                     (%send-base-prefix store stream log-len)
                     (check-not-rotated)))
                  (:resume
                   (let ((to-skip (max 0 (- replica-lsn baseline))))
                     (lambda ()
                       (%wstr epoch stream) (write-byte 1 stream)
                       (check-not-rotated)
                       (%send-log-tail store stream to-skip log-len)
                       (check-not-rotated)))))))
        (bordeaux-threads:with-lock-held ((server-clients-lock server))
          (when (>= (length (server-clients server)) (server-max-clients server))
            (error "replica connection limit (~D) reached" (server-max-clients server)))
          (push client (server-clients server)))))))

(defun %serve-client (server socket)
  "Handle one replica connection in its own thread: read HELLO, register under
the log guard, then spawn the writer thread to send base/delta + live.  Refuses
the connection if too many handshakes are already in flight (anti-flood)."
  (unless (bordeaux-threads:with-lock-held ((server-clients-lock server))
            (when (< (server-in-flight server) (server-max-clients server))
              (incf (server-in-flight server))))
    (ignore-errors (usocket:socket-close socket))
    (return-from %serve-client))
  (unwind-protect
       (handler-case
           (let* (;; Bound EVERY handshake read (incl. the TLS handshake done by
                  ;; make-stream, and the auth exchange): a socket receive timeout
                  ;; set BEFORE any read makes a peer that connects then stalls
                  ;; error out instead of pinning this thread.  The server never
                  ;; reads after the handshake (the writer thread only writes), so
                  ;; it can stay set.
                  (_to (ignore-errors
                         (setf (usocket:socket-option socket :receive-timeout) *handshake-timeout*)))
                  (_ (unless (usocket:wait-for-input socket :timeout *handshake-timeout*
                                                            :ready-only t)
                       (error "handshake timeout")))
                  (stream (funcall (server-make-stream server) socket)) ; TLS handshake here (bounded)
                  (_auth (server-authenticate stream (server-secret server))) ; before any data
                  (version (%ru 4 stream))                 ; HELLO
                  (replica-epoch (%rstr stream))
                  (replica-lsn (%ru 8 stream))
                  (client (make-instance 'replica-client :socket socket :stream stream)))
             (declare (ignore version _ _auth _to))
             (%register-client server client replica-epoch replica-lsn)
             (bordeaux-threads:make-thread (lambda () (client-writer client))
                                           :name "replication-writer"))
         (error (e)
           (format *error-output* "; replication client handshake error: ~A~%" e)
           (ignore-errors (usocket:socket-close socket))))
    (bordeaux-threads:with-lock-held ((server-clients-lock server))
      (decf (server-in-flight server)))))

(defun %run-accept-loop (server)
  (loop
    (handler-case
        (let ((socket (usocket:socket-accept (server-listen-socket server)
                                             :element-type '(unsigned-byte 8))))
          (bordeaux-threads:make-thread (lambda () (%serve-client server socket))
                                        :name "replication-handler"))
      (usocket:socket-error () (return))
      (error (e) (format *error-output* "; replication accept error: ~A~%" e)))))

(defun start-replication-server (store &key (host "127.0.0.1") (port 9100)
                                            secret (make-stream #'%plain-stream)
                                            (max-clients *default-max-clients*)
                                            (max-queue-bytes *default-max-queue-bytes*))
  "Start accepting standby connections for STORE on HOST:PORT and return a
REPLICATION-SERVER.  Bootstrap/resume and live fan-out run on per-replica writer
threads with bounded queues, so the primary is never blocked by a transfer or a
slow replica.  SECRET (string/octets, or via BKNR_REPL_SECRET[_FILE]) gates which
replicas may connect; with no secret, any client is served (warned).  For TLS,
pass :MAKE-STREAM (bknr.datastore.replication.tls:make-tls-server-stream-fn ...)."
  (let* ((secret (resolve-secret secret))
         (listen (usocket:socket-listen host port
                                        :reuse-address t
                                        :element-type '(unsigned-byte 8)))
         (server (make-instance 'replication-server
                                :store store :epoch (%store-epoch store) :secret secret
                                :make-stream make-stream
                                :listen-socket listen :max-clients max-clients
                                :max-queue-bytes max-queue-bytes)))
    (unless secret
      (warn "replication server on ~A:~A has no shared secret -- any client can pull the store"
            host port))
    (when (find-if (lambda (s) (typep s 'bknr.datastore:blob-subsystem))
                   (bknr.datastore::store-subsystems store))
      (warn "replication does not transfer blob files; replicas of this store will be ~
             missing blob data (replicate blob-root out of band, e.g. via rsync)"))
    (setf (server-observer server) (%make-fanout-observer server))
    (add-commit-observer (server-observer server) store)
    (setf (server-accept-thread server)
          (bordeaux-threads:make-thread (lambda () (%run-accept-loop server))
                                        :name "replication-accept"))
    server))

(defun replica-count (server)
  "Number of replicas currently connected to SERVER."
  (bordeaux-threads:with-lock-held ((server-clients-lock server))
    (length (server-clients server))))

(defun stop-replication-server (server)
  "Stop SERVER: remove the commit observer, kill clients, close the listener, and
tear down the accept thread."
  (when (server-observer server)
    (remove-commit-observer (server-observer server) (server-store server)))
  (ignore-errors (usocket:socket-close (server-listen-socket server)))
  (bordeaux-threads:with-lock-held ((server-clients-lock server))
    (dolist (c (server-clients server)) (client-kill c))
    (setf (server-clients server) nil))
  (let ((thread (server-accept-thread server)))
    (when (and thread (bordeaux-threads:thread-alive-p thread))
      (ignore-errors (bordeaux-threads:destroy-thread thread))))
  server)

;;;; ---------------------------------------------------------------------------
;;;; Replica side

(defstruct replica-state
  dir                  ; store directory
  store                ; the open replica store, or NIL before the first bootstrap
  epoch                ; primary epoch we are synced to, or NIL
  (lsn 0))             ; last applied LSN

(defun %replica-bootstrap (state stream primary-epoch)
  "Receive the full base into a fresh store and restore it."
  (let ((dir (replica-state-dir state)))
    (when (replica-state-store state)
      (close-store)                     ; clears the global *store* before re-opening
      (setf (replica-state-store state) nil))
    (uiop:delete-directory-tree dir :validate t :if-does-not-exist :ignore)
    (let ((cur (ensure-directories-exist
                (merge-pathnames (make-pathname :directory '(:relative "current")) dir))))
      (loop for nlen = (%ru 4 stream)
            until (zerop nlen)
            do (let* ((name (%safe-replicated-name
                             (trivial-utf-8:utf-8-bytes-to-string (%rn nlen stream))))
                      (dlen (%ru 8 stream))
                      (data (%rn dlen stream)))
                 ;; The random-state file is read with *read-eval* (SBCL serializes
                 ;; random-state with #.), so a hostile primary could ship a #.
                 ;; form for RCE on the replica.  Discard it and let the replica
                 ;; generate its own (transactions that consume randomness are not
                 ;; bit-reproduced -- an accepted determinism trade-off for safety).
                 (unless (string= name "random-state")
                   (with-open-file (out (merge-pathnames name cur)
                                        :element-type '(unsigned-byte 8)
                                        :direction :output
                                        :if-exists :supersede :if-does-not-exist :create)
                     (write-sequence data out))))))
    ;; Restore brings the store to the primary's current state WITHOUT firing
    ;; apply observers -- bootstrap is a silent sync, not a stream of live events.
    (let ((store (make-instance 'bknr.datastore:mp-store :directory dir)))
      (setf (replica-state-store state) store
            (replica-state-epoch state) primary-epoch
            (replica-state-lsn state) (store-transaction-counter store)))))

(defun %replica-resume (state stream)
  "Apply the log delta onto the already-open replica store."
  (let* ((len (%ru 8 stream))
         (delta (%rn len stream))
         (store (replica-state-store state)))
    (apply-replication-stream store (flex:make-in-memory-input-stream delta))
    (setf (replica-state-lsn state) (store-transaction-counter store))))

(defun replica-session (state host port &key on-apply on-sync secret
                                             (make-stream #'%plain-stream))
  "Run one connect -> handshake -> catch-up (bootstrap or resume) -> live-apply
session, updating STATE.

Catch-up is applied SILENTLY: it brings the local copy up to the primary's
current state without firing ON-APPLY, so a reconnecting replica does not replay
all the events it missed as if they were live.  ON-SYNC (state mode lsn) fires
once at the catch-up -> live boundary; that is where a consumer reads the full
current state (e.g. pushes the initial state to its clients).  ON-APPLY
(store transaction lsn) then fires only for genuinely live transactions.

Returns the mode on a clean disconnect; signals on connect/handshake/transport
error."
  (let ((socket (usocket:socket-connect host port :element-type '(unsigned-byte 8))))
    ;; NOTE: we deliberately do NOT set a receive timeout on the replica socket.
    ;; The live read must block indefinitely waiting for the next commit, and
    ;; SO_RCVTIMEO cannot be portably cleared once set (setting it to 0 makes the
    ;; live read non-blocking on this stack).  A hung primary during the replica's
    ;; own handshake is a self-DoS of a replica that chose its primary, not a
    ;; server-side amplification; bound it with a watchdog if needed.
    (unwind-protect
         (let ((stream (funcall make-stream socket))) ; TLS wraps here
           (client-authenticate stream secret)       ; mutual auth before any data
           (%wu +protocol-version+ 4 stream)         ; HELLO
           (%wstr (or (replica-state-epoch state) "") stream)
           (%wu (replica-state-lsn state) 8 stream)
           (force-output stream)
           (let ((primary-epoch (%rstr stream))      ; REPLY
                 (mode (if (zerop (read-byte stream)) :bootstrap :resume)))
             ;; catch-up (silent: no apply observer registered yet)
             (ecase mode
               (:bootstrap (%replica-bootstrap state stream primary-epoch))
               (:resume (%replica-resume state stream)))
             (when on-sync (funcall on-sync state mode (replica-state-lsn state)))
             ;; live: register ON-APPLY now, so only live transactions notify
             (let ((store (replica-state-store state)))
               (when on-apply (add-apply-observer on-apply store))
               (unwind-protect
                    (apply-replication-stream store stream)
                 (when on-apply (remove-apply-observer on-apply store))
                 (setf (replica-state-lsn state) (store-transaction-counter store))))
             mode))
      (ignore-errors (usocket:socket-close socket)))))

(defun %backoff (base jitter)
  (+ base (* base jitter (random 1.0 (make-random-state t)))))

(defun run-replica (dir host port
                    &key on-apply (reconnect t) secret (make-stream #'%plain-stream)
                         (backoff-initial 0.5) (backoff-max 30.0) (jitter 0.2)
                         (max-retries nil) (connect-tries 120)
                         on-sync on-disconnect on-error)
  "Replicate from a primary at HOST:PORT into the store directory DIR, applying
the live stream.  Mechanism only -- the caller chooses policy via the keyword
arguments and hooks:

  ON-SYNC       (state mode lsn)          -- when caught up (catch-up -> live)
  ON-APPLY      (store transaction lsn)   -- per LIVE transaction (not catch-up)
  ON-DISCONNECT (state)                   -- after a session ends cleanly
  ON-ERROR      (condition state)         -- on a session error

SECRET enables mutual auth (see CLIENT-AUTHENTICATE).  For TLS, pass :MAKE-STREAM
(bknr.datastore.replication.tls:make-tls-client-stream-fn ...).

The INITIAL connection is always retried (the primary may not be up yet) with
exponential backoff, up to CONNECT-TRIES consecutive refusals.  After a session
that actually connected, the replica reconnects iff RECONNECT, resuming from its
last LSN when possible and falling back to a full bootstrap when the primary
epoch changed or the needed log was rotated away.  Catch-up is applied silently;
ON-APPLY fires only after ON-SYNC.  Returns when it gives up, MAX-RETRIES is
exceeded, or (without RECONNECT) the primary disconnects."
  (let ((state (make-replica-state :dir dir))
        (secret (resolve-secret secret))
        (attempt 0) (refused 0)
        (backoff backoff-initial))
    (loop
      (let ((outcome
              (handler-case
                  (progn (replica-session state host port :on-apply on-apply :on-sync on-sync
                                                          :secret secret :make-stream make-stream)
                         :ok)
                (usocket:connection-refused-error () :refused)
                (replication-auth-error (e) (when on-error (funcall on-error e state)) :fatal)
                (error (e) (when on-error (funcall on-error e state)) :error))))
        (case outcome
          (:ok      (setf backoff backoff-initial refused 0)
                    (when on-disconnect (funcall on-disconnect state))
                    (unless reconnect (return)))
          (:refused (incf refused)
                    (when (>= refused connect-tries) (return)))   ; primary never came up
          (:fatal   (return))                                     ; bad secret won't self-heal
          (:error   (unless reconnect (return)))))
      (incf attempt)
      (when (and max-retries (>= attempt max-retries)) (return))
      (sleep (%backoff backoff jitter))
      (setf backoff (min backoff-max (* 2 backoff))))
    state))
