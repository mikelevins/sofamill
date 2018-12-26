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
                                     (:file "map-utils")
                                     (:file "couch")
                                     (:file "sofamill")
                                     (:file "couches-pane")
                                     (:file "instance-pane")))))

;;; (asdf:load-system :sofamill)


;;; (sofamill::put-couch "localhost" (sofamill::couch :host "localhost"))
;;; (sofamill::put-couch "mars.local" (sofamill::couch :host "mars.local" :db-name "oppsdaily"))

;;; (sofamill::probe-couch (sofamill::couch :host "localhost"))
;;; (sofamill::probe-couch (sofamill::couch :host "mars.local"))
;;; (sofamill::probe-couch (sofamill::couch :host "db.delect.us" :port ""))

