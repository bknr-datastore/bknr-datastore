
(defsystem :bknr.skip-list
  :name "skip-list"
  :author "Manuel Odendahl <manuel@bl0rg.net>"
  :version "0"
  :maintainer "Manuel Odendahl <manuel@bl0rg.net>"
  :licence "BSD"
  :description "Skiplist implementation for bknr"
  :components ((:module "skip-list"
                :components
                ((:file "package")
                 (:file "skip-list" :depends-on ("package")))))
  :in-order-to ((test-op (test-op :bknr.skip-list/test))))

(defsystem :bknr.skip-list/test
  :depends-on (:unit-test :bknr.skip-list) 
  :components ((:module "skip-list" :components ((:file "skip-list-tests"))))
  :perform (test-op (o c)
             (symbol-call :unit-test :run-all-tests :unit :skip-list)))


