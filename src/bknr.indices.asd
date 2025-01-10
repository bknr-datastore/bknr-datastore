;;;; -*- Mode: LISP -*-

(defsystem :bknr.indices
  :name "bknr indices"
  :author "Manuel Odendahl <manuel@bl0rg.net>"
  :version "0"
  :maintainer "Manuel Odendahl <manuel@bl0rg.net>"
  :licence "BSD"
  :description "CLOS class indices"
  :depends-on (:cl-interpol :bknr.utils :bknr.skip-list :closer-mop)
  :components ((:module "indices"
                :components
                ((:file "package")
                 (:file "protocol" :depends-on ("package"))
                 (:file "indices" :depends-on ("package" "protocol"))
                 (:file "indexed-class" :depends-on ("package" "indices"))
                 (:file "category-index" :depends-on ("package" "protocol" "indices")))))
  :in-order-to ((test-op (test-op :bknr.indices/test))))

(defsystem :bknr.indices/test  
  :depends-on (:bknr.indices :unit-test)
  :components ((:module "indices" :components ((:file "indices-tests"))))
  :perform (test-op (o c)
             (symbol-call :unit-test :run-all-tests :unit :index)))

