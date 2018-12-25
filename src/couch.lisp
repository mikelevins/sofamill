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

(defmacro with-couch ((&rest args &key (db *couchdb*)
                             name port protocol host user password
                             document-update-fn document-fetch-fn)
                      &body body)
  (declare (ignore host port name protocol user password document-update-fn
                   document-fetch-fn db))
  `(let ((*couchdb* (make-db ,@args)))
     (progn ,@body)))

(defun make-default-couch ()
  (finite-map :host *default-host*
              :port *default-port*
              :name *default-dbname*
              :protocol *default-protocol*
              :user *default-user*
              :password *default-password*))

(defun probe-couch (&key
                    (host "localhost")
                    (port "5984")
                    (protocol "http")
                    (db-name ""))
  (with-couch (:host host
               :port port
               :name db-name
               :protocol protocol)
    (handler-case (get-couchdb-info) 
      (simple-error (err)
        nil))))
