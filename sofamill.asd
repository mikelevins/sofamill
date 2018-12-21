;;;; sofamill.asd

(asdf:defsystem #:sofamill
  :description "Tools for inspecting and editing CouchDB instances"
  :author "mikel evins <mikel@evins.net>"
  :license  "Apache 2"
  :version "0.0.1"
  :serial t
  :depends-on (:clouchdb :fset)
  :components ((:module "src"
                        :serial t
                        :components ((:file "package")
                                     (:file "sofamill")
                                     (:file "instance-view")))))

;;; (asdf:load-system :sofamill)

;;; start Lispworks environment:
;;; (env:start-environment)
