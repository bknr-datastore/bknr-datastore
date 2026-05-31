;;;; Mutual authentication for replication connections.
;;;;
;;;; A shared secret gates who may pull the database (server side) and which
;;;; primary a replica will trust (client side).  Authentication is a nonce-based
;;;; HMAC-SHA256 challenge-response, so the secret itself is never sent on the
;;;; wire.  Each side enforces the other's proof *iff it has a secret*:
;;;;
;;;;   * a server with a secret rejects a client that can't prove the secret;
;;;;   * a client with a secret refuses a server that can't prove the secret
;;;;     (so it won't be fed by an unauthenticated / spoofed primary);
;;;;   * if neither side has a secret, authentication is skipped (local demo).
;;;;
;;;; This authenticates the peers; it does NOT provide confidentiality -- use TLS
;;;; or a tunnel for that.  The secret is loaded from the BKNR_REPL_SECRET env
;;;; var or the file named by BKNR_REPL_SECRET_FILE (prefer the file, mode 0600;
;;;; an env var is visible in /proc and `ps`).

(in-package :bknr.datastore.replication)

(defparameter +nonce-bytes+ 32)
(defparameter +server-tag+ (trivial-utf-8:string-to-utf-8-bytes "bknr-repl-server"))
(defparameter +client-tag+ (trivial-utf-8:string-to-utf-8-bytes "bknr-repl-client"))

(defun resolve-secret (explicit)
  "Resolve the replication secret to an octet vector, or NIL if none is
configured.  EXPLICIT (string/octets) wins, else BKNR_REPL_SECRET, else the file
named by BKNR_REPL_SECRET_FILE."
  (let ((s (or explicit
               (uiop:getenv "BKNR_REPL_SECRET")
               (let ((f (uiop:getenv "BKNR_REPL_SECRET_FILE")))
                 ;; If a secret file is configured but missing/empty, error rather
                 ;; than silently falling back to no-auth (a downgrade footgun).
                 (when f
                   (unless (probe-file f)
                     (error "BKNR_REPL_SECRET_FILE points to a missing file: ~A" f))
                   (let ((c (string-trim '(#\Newline #\Return #\Space #\Tab)
                                         (uiop:read-file-string f))))
                     (when (zerop (length c))
                       (error "BKNR_REPL_SECRET_FILE is empty: ~A" f))
                     c))))))
    (etypecase s
      (null nil)
      (string (if (zerop (length s)) nil (trivial-utf-8:string-to-utf-8-bytes s)))
      ((vector (unsigned-byte 8)) (if (zerop (length s)) nil s)))))

(defun %nonce ()
  ;; The :os PRNG reads OS entropy directly (no seeding); a fresh instance per
  ;; call is cheap and avoids sharing PRNG state across concurrent handshakes.
  (ironclad:random-data +nonce-bytes+ (ironclad:make-prng :os)))

(defun %hmac (secret &rest octet-seqs)
  (let ((h (ironclad:make-hmac secret :sha256)))
    (dolist (s octet-seqs) (ironclad:update-hmac h s))
    (ironclad:hmac-digest h)))

(defun %ct-equal (a b)
  "Constant-time equality of two octet vectors of equal length."
  (and (= (length a) (length b))
       (zerop (let ((r 0))
                (dotimes (i (length a) r)
                  (setf r (logior r (logxor (aref a i) (aref b i)))))))))

(defparameter +max-proof-bytes+ 1024
  "Upper bound on an auth proof on the wire (HMAC-SHA256 is 32 bytes); keeps an
unauthenticated peer from triggering a large allocation during the handshake.")

(define-condition replication-auth-error (error)
  ((side :initarg :side :reader auth-error-side))
  (:report (lambda (c s) (format s "replication authentication failed (~A)"
                                 (auth-error-side c)))))

(defun server-authenticate (stream secret)
  "Server side of the handshake.  Reads the client nonce, replies with a nonce
and (if SECRET) a proof, then verifies the client's proof (if SECRET).  Signals
REPLICATION-AUTH-ERROR if a configured secret is not proven by the client."
  (let* ((client-nonce (%rn +nonce-bytes+ stream))
         (server-nonce (%nonce)))
    (write-sequence server-nonce stream)
    (%write-blob (if secret (%hmac secret +server-tag+ client-nonce server-nonce)
                     #())
                 stream)
    (force-output stream)
    (let ((client-proof (%read-blob stream +max-proof-bytes+)))
      (when (and secret
                 (not (%ct-equal client-proof
                                 (%hmac secret +client-tag+ server-nonce client-nonce))))
        (error 'replication-auth-error :side :server)))))

(defun client-authenticate (stream secret)
  "Replica side of the handshake.  Sends a nonce, verifies the server's proof
(if SECRET), and sends its own proof.  Signals REPLICATION-AUTH-ERROR if a
configured secret is not proven by the server."
  (let ((client-nonce (%nonce)))
    (write-sequence client-nonce stream)
    (force-output stream)
    (let* ((server-nonce (%rn +nonce-bytes+ stream))
           (server-proof (%read-blob stream +max-proof-bytes+))
           (server-ok (or (null secret)
                          (%ct-equal server-proof
                                     (%hmac secret +server-tag+ client-nonce server-nonce)))))
      ;; Always send our proof (it leaks nothing) so the server doesn't hang,
      ;; then abort if the server failed to authenticate to us.
      (%write-blob (if secret (%hmac secret +client-tag+ server-nonce client-nonce) #())
                   stream)
      (force-output stream)
      (unless server-ok (error 'replication-auth-error :side :client)))))
