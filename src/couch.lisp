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
(defparameter *default-db-name* "")
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

(defun couch (&key
              (host *default-host*)
              (port *default-port*)
              (db-name *default-db-name*)
              (protocol *default-protocol*)
              (user *default-user*)
              (password *default-password*))
  (finite-map :host host
              :port port
              :name db-name
              :protocol protocol
              :user user
              :password password))

(defun make-default-couch ()
  (finite-map :host *default-host*
              :port *default-port*
              :name *default-db-name*
              :protocol *default-protocol*
              :user *default-user*
              :password *default-password*))

(defun probe-couch (couch)
  (let ((host (get-key couch :host))
        (port (get-key couch :port))
        (name (get-key couch :name))
        (protocol (get-key couch :protocol)))
    (with-couch (:host host :port port
                 :name name :protocol protocol)
      (handler-case (get-couchdb-info) 
        (simple-error (err)
          nil)))))

(defun list-databases (couch)
  (let ((host (get-key couch :host))
        (port (get-key couch :port))
        (name (get-key couch :name))
        (protocol (get-key couch :protocol)))
    (with-couch (:host host :port port
                 :name name :protocol protocol)
      (handler-case (clouchdb::db-request "_all_dbs" :method :get)
        (simple-error (err)
          nil)))))

;;; (sofamill::put-couch "mars.local" (sofamill::couch :host "mars.local" :db-name "oppsdaily"))
;;; (sofamill::list-databases (sofamill::get-couch "mars.local"))

;;; (sofamill::put-couch "db.delect.us" (sofamill::couch :host "db.delect.us" :port ""))
;;; (sofamill::list-databases (sofamill::get-couch "db.delect.us"))
