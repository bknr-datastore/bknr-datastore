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

(defmacro define-datastore-test (name &rest body)
  `(make-instance 'datastore-test-class
                  :unit :datastore
                  :name ,name
                  :body (lambda ()
                          ,@body)))

(define-datastore-test "Datastore setup"
    (test-assert *test-datastore*))

(define-datastore-test "Create object"
    (let ((obj (make-object 'store-object)))
      (test-assert obj)
      (test-equal (list obj) (all-store-objects))))

(define-datastore-test "Create multiple objects"
    (let ((o1 (make-object 'store-object))
          (o2 (make-object 'store-object)))
      (test-assert o1)
      (test-assert o2)
      (test-equal (length (all-store-objects)) 2)
      (test-assert (subsetp (list o1 o2) (all-store-objects)))))

(define-datastore-test "Delete multiple objects"
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

(define-datastore-test "Restore"
    (make-object 'store-object)
  (restore)
  (test-equal (length (all-store-objects)) 1))

(define-datastore-test "Snapshot and Restore"
    (make-object 'store-object)
  (snapshot)
  (restore)
  (test-equal (length (all-store-objects)) 1))

(define-datastore-test "Restore multiple objects"
    (dotimes (i 10) (make-object 'store-object))
  (restore)
  (test-equal (length (all-store-objects)) 10))

(define-datastore-test "Snapshot and Restore multiple objects"
    (dotimes (i 10) (make-object 'store-object))
  (snapshot)
  (restore)
  (test-equal (length (all-store-objects)) 10))

(defconstant +stress-size+ 10000)

(define-datastore-test "Stress test object creation"
    (format t "Creating ~a objects~%" +stress-size+)
  (time (bknr.datastore::without-sync ()
          (dotimes (i +stress-size+)
            (make-object 'store-object))))
  (test-equal (length (all-store-objects)) +stress-size+)
  (format t "Delete ~A objects~%" +stress-size+)
  (time (bknr.datastore::without-sync ()
          (map-store-objects #'delete-object)))
  (test-equal (all-store-objects) nil))

(define-persistent-class parent ()
  ((child :update :initform nil)))

(define-persistent-class child ()
  ())

(define-datastore-test "Serialize circular dependency in anonymous txn"
    (let ((parent (make-object 'parent)))
      (with-transaction (:circular)
        (setf (parent-child parent) (make-object 'child))))
  (restore)
  (test-equal (find-class 'child)
              (class-of (parent-child (first (class-instances 'parent))))))