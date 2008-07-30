(eval-when (:compile-toplevel :load-toplevel :execute)
  (or (find-package :bknr.datastore.tests)
      (defpackage :bknr.datastore.tests
	(:use :cl :bknr.datastore :bknr.indices :unit-test))))

(in-package :bknr.datastore.tests)

(defun delete-directory (pathname)
  (when (probe-file pathname)
    #+cmu
    (loop for file in (directory pathname)
	  when (pathname-name file)
	  do (delete-file file)
	  unless (pathname-name file)
	  do (delete-directory file))
    #+allegro
    (excl:delete-directory-and-files pathname)
    #+cmu
    (unix:unix-rmdir (namestring pathname))
    #+sbcl
    (loop for file in (directory 
		       (merge-pathnames
			(make-pathname 
			 :name    :wild
			 :type    :wild
			 :version :wild
			 )
			pathname)) 
	  when (pathname-name file) do (delete-file file)
	  unless (pathname-name file) do (delete-directory file))
    #+sbcl
    (sb-posix:rmdir (namestring pathname))
    #+openmcl
    (ccl::recursive-delete-directory pathname)))

(defvar *test-datastore-directory* #p"/tmp/test-datastore/")
(defvar *test-datastore* nil)

(define-test-class datastore-test-class)

(defmethod run-test :around ((test datastore-test-class) &optional (output *debug-io*))
  (declare (ignore output))
  (delete-directory *test-datastore-directory*)
  (let* ((*test-datastore* (make-instance 'mp-store :directory *test-datastore-directory*)))
    (call-next-method)
    (close-store)))

(defvar *tests* (make-hash-table))

(defmacro define-datastore-test (name &body body)
  `(setf (gethash ,name *tests*)
         (make-instance 'datastore-test-class
                        :unit :datastore
                        :name ,name
                        :body (lambda ()
                                ,@body))))

(define-datastore-test :store-setup
    (test-assert *test-datastore*))

(define-datastore-test :create-object
    (let ((obj (make-object 'store-object)))
      (test-assert obj)
      (test-equal (list obj) (all-store-objects))))

(define-datastore-test :create-multiple-objects
    (let ((o1 (make-object 'store-object))
	  (o2 (make-object 'store-object)))
      (test-assert o1)
      (test-assert o2)
      (test-equal (length (all-store-objects)) 2)
      (test-assert (subsetp (list o1 o2) (all-store-objects)))))

(define-datastore-test :delete-multiple-objects
    (let ((o1 (make-object 'store-object))
	  (o2 (make-object 'store-object)))
      (test-assert o1)
      (test-assert o2)
      (test-equal (length (all-store-objects)) 2)
      (test-assert (subsetp (list o1 o2) (all-store-objects)))
      (delete-object o1)
      (test-equal (all-store-objects) (list o2))
      (delete-object o2)
      (test-equal (all-store-objects) nil)))

(define-datastore-test :restore
    (make-object 'store-object)
  (restore)
  (test-equal (length (all-store-objects)) 1))

(define-datastore-test :snapshot-and-restore
    (make-object 'store-object)
  (snapshot)
  (restore)
  (test-equal (length (all-store-objects)) 1))

(define-datastore-test :restore-multiple-objects
    (dotimes (i 10) (make-object 'store-object))
  (restore)
  (test-equal (length (all-store-objects)) 10))

(define-datastore-test :snapshot-restore-multiple-objects
    (dotimes (i 10) (make-object 'store-object))
  (snapshot)
  (restore)
  (test-equal (length (all-store-objects)) 10))

(defconstant +stress-size+ 10000)

(define-datastore-test :stress-test
    (format t "Creating ~a objects~%" +stress-size+)
  (time (bknr.datastore::without-sync ()
          (dotimes (i +stress-size+)
            (make-object 'store-object))))
  (test-equal (length (all-store-objects)) +stress-size+)
  (format t "Delete ~A objects~%" +stress-size+)
  (time (bknr.datastore::without-sync ()
          (map-store-objects #'delete-object)))
  (test-equal (all-store-objects) nil))

(define-datastore-test :make-instance-in-anon-txn
  (with-transaction ()
    (make-instance 'store-object)))

(define-datastore-test :make-object-in-anon-txn
  (with-transaction ()
    (make-object 'store-object)))

(define-persistent-class parent ()
  ((child :update :initform nil)))

(define-persistent-class child ()
  ())

(define-datastore-test :serialize-circular-in-anon-txn
  (let ((parent (make-object 'parent)))
    (with-transaction (:circular)
      (setf (parent-child parent) (make-object 'child))))
  (restore)
  (test-equal (find-class 'child)
              (class-of (parent-child (first (class-instances 'parent))))))

(define-datastore-test :delete-object-in-anon-txn
  (let (object)
    (with-transaction (:make)
      (setf object (make-object 'child)))
    (with-transaction (:delete)
      (delete-object object))
    (restore)
    (test-assert (object-destroyed-p object))))