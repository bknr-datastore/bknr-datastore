(in-package :bknr.utils)

#+(not (or allegro sbcl cmu openmcl lispworks))
(error "missing port for this compiler, please provide for locking primitives for this compiler in ~A" *load-pathname*)

(defun mp-make-lock (&optional (name "Anonymous"))
  #+allegro
  (mp:make-process-lock :name name)
  #+sbcl
  (sb-thread:make-mutex :name name)
  #+(and cmu x86)
  (mp:make-lock name)
  #+(and cmu (not x86))
  (declare (ignore name))
  #+openmcl
  (ccl:make-lock name)
  #+lispworks
  (mp:make-lock :name name))

(defmacro mp-with-lock-held ((lock) &body body)
  #+allegro
  `(mp:with-process-lock (,lock)
    ,@body)
  #+sbcl
  `(sb-thread:with-mutex (,lock)
     ,@body)
  #+cmu
  `(mp:with-lock-held (,lock)
    ,@body)
  #+openmcl
  `(ccl:with-lock-grabbed (,lock)
    ,@body)
  #+lispworks
  `(mp:with-lock (,lock)
    ,@body))

(defmacro mp-with-recursive-lock-held ((lock) &body body)
  #+allegro
  `(mp:with-process-lock (,lock)
    ,@body)
  #+sbcl
  `(sb-thread:with-recursive-lock (,lock)
     ,@body)
  #+cmu
  `(mp:with-lock-held (,lock)
    ,@body)
  #+openmcl
  `(ccl:with-lock-grabbed (,lock)
    ,@body)
  #+lispworks
  `(mp:with-lock (,lock)
    ,@body))

#+(not (or sbcl openmcl cmu))
(error "missing port for this compiler, please provide for multiprocessing primitives for this compiler in ~A" *load-pathname*)

(defun make-process (function &key name)
  #+sbcl (sb-thread:make-thread function :name name)
  #+openmcl (ccl:process-run-function name function)
  #+cmu (mp:make-process function :name name))

(defun destroy-process (process)
  #+sbcl (sb-thread:terminate-thread process)
  #+openmcl (ccl:process-kill process)
  #+cmu (mp:destroy-process process))

(defun process-active-p (process)
  #+sbcl (sb-thread:thread-alive-p process)
  #+openmcl (ccl::process-active-p process)
  #+cmu (mp:process-active-p process))

