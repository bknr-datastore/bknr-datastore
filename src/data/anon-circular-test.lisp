(in-package :bknr.datastore)

(define-persistent-class parent ()
  ((children :update :initform nil)))

(define-persistent-class child ()
  ())

(defun test-circular (parent)
  (with-transaction (:circular)
    (push (make-object 'child) (parent-children parent))))

(defvar *p* (make-object 'parent))

(test-circular *p*)