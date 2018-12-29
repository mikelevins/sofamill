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

(defmacro with-couch ((&rest args &key
                             name port protocol host user password
                             document-update-fn document-fetch-fn)
                      &body body)
  (declare (ignore host port name protocol user password document-update-fn
                   document-fetch-fn db))
  `(let ((clouchdb::*couchdb* (clouchdb::make-db ,@args)))
     (progn ,@body)))

(defun couch (&key
              (host *default-host*)
              (port *default-port*)
              (db-name *default-db-name*)
              (protocol *default-protocol*)
              (user *default-user*)
              (password *default-password*))
  (wb-map :host host
          :port port
          :name db-name
          :protocol protocol
          :user user
          :password password))

(defun make-default-couch () (couch))

(defun probe-couch (couch)
  (let ((host (get-key couch :host))
        (port (get-key couch :port))
        (name (get-key couch :name))
        (protocol (get-key couch :protocol)))
    (with-couch (:host host :port port
                 :name name :protocol protocol)
      (handler-case (clouchdb::get-couchdb-info) 
        (simple-error (err)
          nil)))))

;;; (sofamill::probe-couch (sofamill::couch :host "localhost"))
;;; (sofamill::probe-couch (sofamill::couch :host "mars.local"))
;;; (sofamill::probe-couch (sofamill::couch :host "db.delect.us" :port ""))

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

;;; (sofamill::put-couch "mars.local" (sofamill::couch :host "mars.local"))
;;; (sofamill::list-databases (sofamill::get-couch "mars.local"))

;;; (sofamill::put-couch "db.delect.us" (sofamill::couch :host "db.delect.us" :port ""))
;;; (sofamill::list-databases (sofamill::get-couch "db.delect.us"))

(defmethod userdb-name-string? ((s string))
  (prefix-match? "userdb-" s))

(defmethod decode-userdb-name ((dbstring string))
  (if (userdb-name-string? dbstring)
      (let* ((numstr (subseq dbstring (length "userdb-")))
             (char-strings (take-by 2 numstr))
             (char-codes (mapcar (lambda (cs)(parse-integer cs :radix 16))
                                 char-strings))
             (chars (mapcar (lambda (cc)(code-char cc))
                            char-codes)))
        (coerce chars 'string))
    ;; not a userdb name string
    nil))

;;; get-document-list
;;; clouchdb::get-all-documents returns data in the following format:
;;; (<total_rows> <offset> <rows>)
;;; where:
;;;   <total_rows> is (:|total_rows| . <an integer>)
;;;   <offset> is (:|offset| . <an integer>)
;;;   <rows> is (:|rows| <row>*)
;;;   where: 
;;;     <row> is ((:|id| . <id-string>) (:|key| . <key-string>) (:|value| <document-data>))
;;;     where:
;;;       <document-data> is (:|rev| . <revision-string>), but if we call Couch with the
;;;                          :include-documents arg true then we'll get the full document contents
;;;                          in the value, as well

(defun get-document-list (couch dbname &key (skip 0)(limit nil))
  (let ((host (get-key couch :host))
        (port (get-key couch :port))
        (protocol (get-key couch :protocol)))
    (with-couch (:host host :port port
                 :name dbname :protocol protocol)
      (handler-case (clouchdb::get-all-documents :skip skip :limit limit)
        (simple-error (err)
          (warn "Error getting document list: ~S" err)
          nil)))))

;;; (sofamill::put-couch "mars.local" (sofamill::couch :host "mars.local" :db-name "oppsdaily"))
;;; (sofamill::get-document-list (get-couch "mars.local") "oppsdaily")
;;; (sofamill::get-document-list (get-couch "mars.local") "oppsdaily" :skip 20 :limit 10)

;;; list-document-ids
;;; returns a list of document IDs

(defun list-document-ids (couch dbname &key (skip 0)(limit nil))
  (let* ((records (get-document-list couch dbname :skip skip :limit limit))
         (rows (alist-get-key records :|rows|)))
    (alist-vals (mapcar #'car rows))))

(defmethod get-document ((couch fset:wb-map) (dbname string) (document-id string)
                         &key revision revisions conflicts
                         revision-info (if-missing nil))
  (let* ((host (get-key couch :host))
         (port (get-key couch :port))
         (protocol (get-key couch :protocol))
         (url (url-string protocol "://" host ":" port "/" dbname "/" document-id)))
    (let ((stream (drakma:http-request url :want-stream t)))
      (setf (flexi-streams:flexi-stream-external-format stream) :utf-8)
      (reverse (yason:parse stream :object-as :alist)))))

;;; (sofamill::put-couch "mars.local" (sofamill::couch :host "mars.local"))
;;; (sofamill::list-document-ids (get-couch "mars.local") "oppsdaily" :skip 20 :limit 10)
;;; (sofamill::get-document (get-couch "mars.local") "oppsdaily" "b449e4da4e28616d4f59f5d5be2123ea")
;;; (sofamill::get-document (get-couch "mars.local") "reddit_corpus" "117f6fa420864c640055b3529ff8ef68")

;;; this one fails when a local couch isn't running
;;; (sofamill::put-couch "localhost" (sofamill::couch :host "localhost"))
;;; (sofamill::get-document (get-couch "localhost") "oppsdaily" "b449e4da4e28616d4f59f5d5be2123ea")
