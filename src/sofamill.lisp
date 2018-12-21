;;;; ***********************************************************************
;;;;
;;;; Name:          sofamill.lisp
;;;; Project:       SofaMill: a CouchDB browser
;;;; Purpose:       the application's state
;;;; Author:        mikel evins
;;;; Copyright:     2018 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package #:sofamill)

(defparameter *sofamill* nil)

(defclass sofamill ()
  ((state :accessor state :initform (empty-map) :initarg :state)))

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

(defun update-state (key val)
  (setf (state (sofamill))
        (fset:with (state (sofamill))
                   key val)))

(defun get-state (key &optional (default nil))
  (let* ((mill-default (fset:map-default (state (sofamill))))
         (found (fset:@ (state (sofamill)) key)))
    (if (equal found mill-default)
        default
        found)))

(defun couches ()
  (get-state :couches))

(defun add-couch (namestring 
                  &key
                  (host "localhost")
                  (port "5984")
                  (protocol "http")
                  (dbname nil)
                  (user nil)
                  (password nil))
  (let* ((couch-structure (clouchdb:make-db :host host
                                            :port port
                                            :name dbname
                                            :protocol protocol
                                            :user user
                                            :password password))
         (old-couches (couches))
         (new-couches (merge-keys old-couches
                                  (finite-map namestring couch-structure))))
    (update-state :couches new-couches)))

#|
(add-couch "localhost" :host "localhost")
(add-couch "mars.local" :host "mars.local" :dbname "oppsdaily")
|#
