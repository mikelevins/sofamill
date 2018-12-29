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
  (wb-map :couches (empty-map)))

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
                                (wb-map key val))))
    (setf (state (sofamill))
          new-state)))

(defun couches ()(get-state :couches))

(defun list-couches ()
  (let ((couches (get-state :couches)))
    (map-keys couches)))

(defun get-couch (couch-name &optional (key nil)(default nil))
  (let ((couches (get-state :couches)))
    (if couches
        (let ((couch (get-key couches couch-name :default nil)))
          (if couch
              (if key
                  (get-key couch key :default default)
                couch)
            default))
      default)))

(defun put-couch (namestring new-couch)
  (let* ((old-couches (couches))
         (new-couches (merge-keys old-couches
                                  (wb-map namestring new-couch))))
    (update-state :couches new-couches)))

;;; (sofamill::put-couch "localhost" (sofamill::couch :host "localhost"))
;;; (sofamill::put-couch "mars.local" (sofamill::couch :host "mars.local"))
;;; (sofamill::put-couch "db.delect.us" (sofamill::couch :host "db.delect.us" :port ""))

(defmethod update-couch (namestring key val)
  (let* ((old-couch (get-couch namestring))
         (new-couch (if old-couch
                        (put-key old-couch key val)
                      (couch key val))))
    (put-couch namestring new-couch)))

;;; (sofamill::update-couch "mars.local" :name "reddit_corpus")
;;; (sofamill::update-couch "mars.local" :protocol "https")

