;;;; Wire framing primitives shared by the auth handshake and the replication
;;;; transport.  Kept in its own file so both auth.lisp and network.lisp can
;;;; depend on it without a circular dependency.

(in-package :bknr.datastore.replication)

(defparameter +protocol-version+ 1)

(defparameter *max-frame-bytes* (* 8 1024 1024 1024)
  "Sanity ceiling on a single wire frame, to reject an absurd length from a
corrupt or hostile peer before allocating.  It is deliberately large because a
bootstrap base frame carries a whole snapshot/log file (often hundreds of MB).
The pre-auth handshake is bounded far more tightly -- proofs are read with the
small +MAX-PROOF-BYTES+ cap -- so this loose ceiling only applies after a peer
is authenticated.")

(defun %wu (n nbytes stream)
  "Write unsigned integer N as NBYTES big-endian bytes."
  (loop for i from (* 8 (1- nbytes)) downto 0 by 8
        do (write-byte (ldb (byte 8 i) n) stream)))

(defun %ru (nbytes stream)
  "Read an NBYTES big-endian unsigned integer."
  (let ((n 0))
    (dotimes (i nbytes n)
      (setf n (logior (ash n 8) (read-byte stream))))))

(defun %rn (n stream)
  "Read exactly N bytes into a fresh octet vector."
  (when (> n *max-frame-bytes*)
    (error "oversized replication frame: ~D bytes" n))
  (let ((buf (make-array n :element-type '(unsigned-byte 8))))
    (let ((got (read-sequence buf stream)))
      (unless (= got n) (error "short read: ~D of ~D bytes" got n)))
    buf))

(defun %write-blob (octets stream)
  "Write OCTETS as a u32 length prefix + bytes."
  (%wu (length octets) 4 stream)
  (write-sequence octets stream))

(defun %read-blob (stream &optional (max *max-frame-bytes*))
  "Read a u32-length-prefixed octet blob, rejecting anything larger than MAX so a
peer can't trigger a large allocation (the auth handshake passes a small MAX)."
  (let ((n (%ru 4 stream)))
    (when (> n max) (error "wire blob too large: ~D bytes" n))
    (%rn n stream)))

(defun %wstr (string stream)
  "Write STRING as a u32 length prefix + UTF-8 bytes."
  (%write-blob (trivial-utf-8:string-to-utf-8-bytes string) stream))

(defun %rstr (stream)
  (trivial-utf-8:utf-8-bytes-to-string (%read-blob stream)))
