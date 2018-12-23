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
                                     (:file "sofamill")
                                     (:file "couches-pane")
                                     (:file "instance-pane")))))

;;; (asdf:load-system :sofamill)


;;; (sofamill::add-couch "localhost" :host "localhost")
;;; (sofamill::add-couch "mars.local" :host "mars.local" :dbname "oppsdaily")
;;; (sofamill::put-couch-key "mars.local" "NAME" "delectus")
;;; (sofamill::put-couch-key "mars.local" :protocol "https")
;;; (defparameter $win (capi:contain (make-instance 'sofamill::couches-pane)))

