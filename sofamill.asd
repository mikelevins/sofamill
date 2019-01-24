;;;; ***********************************************************************
;;;;
;;;; Name:          sofamill.asd
;;;; Project:       SofaMill: a CouchDB browser
;;;; Purpose:       system definition
;;;; Author:        mikel evins
;;;; Copyright:     2018 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package #:cl-user)

(asdf:defsystem #:sofamill
  :description "Tools for inspecting and editing CouchDB instances"
  :author "mikel evins <mikel@evins.net>"
  :license  "Apache 2"
  :version "0.0.1"
  :serial t
  :depends-on (:clouchdb :fset :yason)
  :components ((:module "src"
                        :serial t
                        :components ((:file "package")
                                     (:file "string-utils")
                                     (:file "list-utils")
                                     (:file "map-utils")
                                     (:file "url-utils")
                                     (:file "couch")
                                     (:file "sofamill")
                                     ;;(:file "couches-pane")
                                     ;;(:file "instance-pane")
                                     ;;(:file "databases-pane")
                                     ;;(:file "documents-pane")
                                     ))))

;;; (asdf:load-system :sofamill)
