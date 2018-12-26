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
  (print-unreadable-object (mill stream :type t :identity t)
    (print-object (state mill) stream)))

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
          (if couch
              (if key
                  (couchdb-slot-value couch key)
                couch)
            nil))
      nil)))

(defun update-couch (namestring new-couch)
  (let* ((old-couches (couches))
         (new-couches (merge-keys old-couches
                                  (finite-map namestring new-couch))))
    (update-state :couches new-couches)))

(defmethod put-couch (namestring (new-couch fset:wb-map) &optional (key nil) (val nil))
  (let* ((new-couch (if key
                        (put-key new-couch key val)
                      new-couch)))
    (update-couch namestring new-couch)))

#|
(add-couch "localhost" :host "localhost")
(add-couch "mars.local" :host "mars.local" :dbname "oppsdaily")
|#
