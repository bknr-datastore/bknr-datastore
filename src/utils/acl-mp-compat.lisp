(in-package :bknr.utils)

#+(not (or allegro sbcl cmu openmcl lispworks))
(error "missing port for this compiler, please provide for locking primitives for this compiler in ~A" *load-pathname*)

(defun mp-make-lock (&optional (name "Anonymous"))
  #+allegro
  (mp:make-process-lock :name name)
  #+sbcl
  (sb-thread:make-mutex :name name)
  #+cmu
  (mp:make-lock name)
  #+openmcl
  (ccl:make-lock name)
  #+lispworks
  (mp:make-lock :name name))

(defmacro mp-with-lock-held ((lock) &rest body)
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

(defmacro mp-with-recursive-lock-held ((lock) &rest body)
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
