;;;; ***********************************************************************
;;;;
;;;; Name:          couch.lisp
;;;; Project:       SofaMill: a CouchDB browser
;;;; Purpose:       communication with couch servers
;;;; Author:        mikel evins
;;;; Copyright:     2018 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package #:sofamill)

(defun probe-couch (&key
                    (host "localhost")
                    (port "5984")
                    (protocol "http")
                    (dbname ""))
  (let* ((couch (clouchdb:make-db :host host
                                  :port port
                                  :name dbname
                                  :protocol protocol)))
    (handler-case (get-couchdb-info :db couch) 
      (simple-error (err)
        nil))))


