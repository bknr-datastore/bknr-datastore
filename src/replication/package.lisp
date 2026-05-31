(in-package :cl-user)

(defpackage :bknr.datastore.replication
  (:nicknames :bknr.replication)
  (:use :cl :bknr.datastore)
  (:export
   ;; primary side
   #:make-stream-replication-observer
   ;; standby side
   #:apply-replication-stream
   #:add-apply-observer
   #:remove-apply-observer
   #:transaction-applied
   #:replica-applied-lsn
   ;; network transport
   #:replication-server
   #:replica-count
   #:start-replication-server
   #:stop-replication-server
   #:run-replica
   #:replica-state
   #:replica-state-store
   #:replica-state-lsn
   #:replica-state-epoch
   ;; auth
   #:replication-auth-error
   #:auth-error-side))
