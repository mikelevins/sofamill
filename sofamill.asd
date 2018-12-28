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
                                     (:file "string-utils")
                                     (:file "list-utils")
                                     (:file "map-utils")
                                     (:file "couch")
                                     (:file "sofamill")
                                     (:file "couches-pane")
                                     (:file "instance-pane")
                                     (:file "databases-pane")
                                     (:file "documents-pane")))))

;;; (asdf:load-system :sofamill)


;;; (sofamill::put-couch "localhost" (sofamill::couch :host "localhost"))
;;; (sofamill::put-couch "mars.local" (sofamill::couch :host "mars.local"))
;;; (sofamill::put-couch "db.delect.us" (sofamill::couch :host "db.delect.us" :port ""))

;;; (sofamill::update-couch "mars.local" :name "reddit_corpus")
;;; (sofamill::update-couch "mars.local" :protocol "https")

;;; (sofamill::probe-couch (sofamill::couch :host "localhost"))
;;; (sofamill::probe-couch (sofamill::couch :host "mars.local"))
;;; (sofamill::probe-couch (sofamill::couch :host "db.delect.us" :port ""))

