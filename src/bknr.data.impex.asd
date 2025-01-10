
(defsystem :bknr.data.impex
    :name "baikonour datastore with xml impex"
    :author "Hans Huebner <hans@huebner.org>"
    :author "Manuel Odendahl <manuel@bl0rg.net>"
    :version "0"
    :maintainer "Manuel Odendahl <manuel@bl0rg.net>"
    :licence "BSD"
    :description "baikonour - launchpad for lisp satellites"
    :depends-on (:cl-interpol :unit-test :bknr.utils :bknr.indices
                 :bknr.datastore :bknr.impex)
    :components ((:module "data" :components ((:file "xml-object")))))
