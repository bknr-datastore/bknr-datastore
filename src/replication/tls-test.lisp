;;;; Unit test for the optional TLS transport (bknr.datastore.replication/tls).
;;;; Kept in its own system so the base test suite carries no cl+ssl dependency.

(in-package :cl-user)

(defpackage :bknr.datastore.replication.tls.test
  (:use :cl))

(in-package :bknr.datastore.replication.tls.test)

(5am:def-suite :bknr.datastore.replication.tls)
(5am:in-suite :bknr.datastore.replication.tls)

(defun %gen-self-signed (cert key)
  "Generate a throwaway self-signed cert/key with openssl.  Returns T on success."
  (zerop (nth-value 2
                    (uiop:run-program
                     (list "openssl" "req" "-x509" "-newkey" "rsa:2048"
                           "-keyout" (namestring key) "-out" (namestring cert)
                           "-days" "1" "-nodes" "-subj" "/CN=test")
                     :ignore-error-status t :output nil :error-output nil))))

(5am:test tls.stream-roundtrip
  "The TLS make-stream constructors produce working TLS streams that round-trip
data over a socket pair (server wraps in a thread, client in the foreground)."
  (let ((dir (ensure-directories-exist
              (merge-pathnames (make-pathname :directory (list :relative (string (gensym "bknr-tls"))))
                               (uiop:temporary-directory)))))
    (unwind-protect
         (let ((cert (merge-pathnames "cert.pem" dir))
               (key (merge-pathnames "key.pem" dir)))
           (if (not (and (uiop:getenv "PATH") (%gen-self-signed cert key)))
               (5am:skip "openssl not available")
               (let* ((listener (usocket:socket-listen "127.0.0.1" 0 :reuse-address t
                                                                     :element-type '(unsigned-byte 8)))
                      (port (usocket:get-local-port listener))
                      (server-fn (bknr.datastore.replication.tls:make-tls-server-stream-fn
                                  :certificate (namestring cert) :key (namestring key)))
                      (client-fn (bknr.datastore.replication.tls:make-tls-client-stream-fn))
                      (got nil))
                 (unwind-protect
                      (let ((cs (usocket:socket-connect "127.0.0.1" port
                                                        :element-type '(unsigned-byte 8))))
                        (let ((th (bordeaux-threads:make-thread
                                   (lambda ()
                                     (ignore-errors
                                       (let* ((ss (usocket:socket-accept
                                                   listener :element-type '(unsigned-byte 8)))
                                              (s (funcall server-fn ss)))
                                         (write-byte (1+ (read-byte s)) s)
                                         (force-output s)))))))
                          (let ((c (funcall client-fn cs)))
                            (write-byte 41 c) (force-output c)
                            (setf got (read-byte c)))
                          (bordeaux-threads:join-thread th)))
                   (ignore-errors (usocket:socket-close listener)))
                 (5am:is (eql 42 got)))))
      (uiop:delete-directory-tree dir :validate t :if-does-not-exist :ignore))))
