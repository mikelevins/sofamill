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

(defparameter *default-host* "localhost")
(defparameter *default-port* "5984")
(defparameter *default-protocol* "http")
(defparameter *default-dbname* "")
(defparameter *default-user* "")
(defparameter *default-password* "")

(defun make-default-couch ()
  (clouchdb:make-db :host *default-host*
                    :port *default-port*
                    :name *default-dbname*
                    :protocol *default-protocol*
                    :user *default-user*
                    :password *default-password*))

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

(defmethod couchdb-slot-value ((couchdb clouchdb::db)(key string))
  (slot-value couchdb (intern key :clouchdb)))

(defmethod couchdb-slot-value ((couchdb clouchdb::db)(key symbol))
  (couchdb-slot-value couchdb (symbol-name key)))

