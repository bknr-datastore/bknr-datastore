;;;; -*- Mode: LISP -*-

(defsystem :bknr.impex
  :name "BKNR impex"
  :author "Manuel Odendahl <manuel@bl0rg.net>"
  :version "0"
  :maintainer "Manuel Odendahl <manuel@bl0rg.net>"
  :licence "BSD"
  :description "BKNR XML import/export"
  :depends-on (:cl-interpol :cxml :closer-mop :bknr.utils :bknr.xml :bknr.indices)
  :components ((:module "xml-impex"
                :components
                ((:file "package")
                 (:file "xml-class" :depends-on ("package"))
                 (:file "xml-import"
                  :depends-on ("package" "xml-class"))
                 (:file "xml-export"
                  :depends-on ("package" "xml-class"))))))
