;;;; ***********************************************************************
;;;;
;;;; Name:          sofamill.lisp
;;;; Project:       SofaMill: a CouchDB browser
;;;; Purpose:       the application's state
;;;; Author:        mikel evins
;;;; Copyright:     2018 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package :sofamill)

(defparameter *sofamill* nil)

(defparameter *initial-state*
  (finite-map
   :couches (empty-map)))

(defclass sofamill ()
  ((state :accessor state :initform *initial-state* :initarg :state)))

(defun sofamill ()
  (or *sofamill*
      (setf *sofamill* (make-instance 'sofamill))))

(defmethod print-object ((mill sofamill)(stream stream))
  (let ((state (state mill)))
    (pprint-logical-block (stream nil :prefix "#<sofamill> {")
      (fset:do-map (x y state)
        (pprint-pop)
        (write-char #\Space stream)
        (pprint-newline :linear stream)
        (write x :stream stream)
        (write-char #\Space stream)
        (write y :stream stream))
      (format stream " }"))))

(defmethod get-state ((key symbol) &optional (default nil))
  (let* ((mill-default (fset:map-default (state (sofamill))))
         (found (fset:@ (state (sofamill)) key)))
    (if (equal found mill-default)
        default
      found)))

(defmethod update-state ((key symbol) val)
  (let* ((old-state (state (sofamill)))
         (new-state (merge-keys old-state
                                (finite-map key val))))
    (setf (state (sofamill))
          new-state)))

(defun couches ()(get-state :couches))

(defun list-couches ()
  (let ((couches (get-state :couches)))
    (map-keys couches)))

(defun get-couch (couch-name &optional (key nil))
  (let ((couches (get-state :couches)))
    (if couches
        (let ((couch (get-key couches couch-name)))
          (if key
              (couchdb-slot-value couch key)
            couch))
      nil)))

(defun update-couch (namestring new-couch)
  (let* ((old-couches (couches))
         (new-couches (merge-keys old-couches
                                  (finite-map namestring new-couch))))
    (update-state :couches new-couches)))

(defmethod put-couch-key ((namestring string) (slot-name string) new-value)
  (let ((old-couch (get-couch namestring)))
    (if old-couch
        (let ((new-couch (clouchdb:make-db :host (db-host old-couch)
                                           :port (db-port old-couch)
                                           :name (db-name old-couch)
                                           :protocol (db-protocol old-couch)
                                           :user (db-user old-couch)
                                           :password (db-password old-couch)))
              (slot-symbol (intern slot-name :clouchdb)))
          (setf (slot-value new-couch slot-symbol)
                new-value)
          (update-couch namestring new-couch))
      (error "No such CouchDB instance: ~S" namestring))))

(defmethod put-couch-key ((namestring string) (slot-name symbol) new-value)
  (put-couch-key namestring (symbol-name slot-name) new-value))

(defun add-couch (namestring 
                  &key
                  (host "localhost")
                  (port "5984")
                  (protocol "http")
                  (dbname nil)
                  (user nil)
                  (password nil))
  (let* ((new-couch (clouchdb:make-db :host host
                                      :port port
                                      :name dbname
                                      :protocol protocol
                                      :user user
                                      :password password)))
    (update-couch namestring new-couch)))

#|
(add-couch "localhost" :host "localhost")
(add-couch "mars.local" :host "mars.local" :dbname "oppsdaily")
(put-couch-key "mars.local" "NAME" "delectus")
(put-couch-key "mars.local" :protocol "https")
(setf $edwin (editor::current-window))
|#
