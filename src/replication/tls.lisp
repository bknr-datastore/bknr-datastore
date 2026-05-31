;;;; Optional TLS transport for bknr.datastore replication.
;;;;
;;;; This is a separate system (bknr.datastore.replication.tls) so the base
;;;; replication module carries no cl+ssl / OpenSSL dependency.  It plugs into
;;;; the transport seam: START-REPLICATION-SERVER and RUN-REPLICA accept a
;;;; :MAKE-STREAM function (socket -> stream); the constructors here return one
;;;; that wraps the socket in a TLS stream.  TLS runs *under* the existing mutual
;;;; HMAC auth, adding confidentiality (the auth still authenticates the peers).
;;;;
;;;; Example:
;;;;   (start-replication-server store
;;;;     :make-stream (make-tls-server-stream-fn :certificate "cert.pem" :key "key.pem"))
;;;;   (run-replica dir host port
;;;;     :make-stream (make-tls-client-stream-fn :verify :required :hostname "primary"))

(in-package :cl-user)

(defpackage :bknr.datastore.replication.tls
  (:nicknames :bknr.replication.tls)
  (:use :cl)
  (:export #:make-tls-server-stream-fn
           #:make-tls-client-stream-fn))

(in-package :bknr.datastore.replication.tls)

(defun make-tls-server-stream-fn (&key certificate key password)
  "Return a MAKE-STREAM function for START-REPLICATION-SERVER that wraps each
accepted connection in a TLS server stream, presenting the PEM CERTIFICATE and
private KEY (PASSWORD if the key is encrypted)."
  (check-type certificate (or string pathname))
  (check-type key (or string pathname))
  (lambda (socket)
    (cl+ssl:make-ssl-server-stream (usocket:socket-stream socket)
                                   :certificate certificate
                                   :key key
                                   :password password)))

(defun make-tls-client-stream-fn (&key verify hostname)
  "Return a MAKE-STREAM function for RUN-REPLICA that wraps the connection in a
TLS client stream.  With VERIFY (e.g. :required) and HOSTNAME the primary's
certificate is verified; with VERIFY NIL the certificate is accepted unchecked
(only acceptable behind auth on a trusted network / for self-signed demos)."
  (lambda (socket)
    (cl+ssl:make-ssl-client-stream (usocket:socket-stream socket)
                                   :verify verify
                                   :hostname hostname)))
