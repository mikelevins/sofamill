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
  ((state :accessor state :initform (fset:wb-map) :initarg :state)))

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

(defmethod update-state ((mill sofamill)(key symbol) val)
  (setf (state mill)
        (fset:with (state mill)
                   key val)))

(defmethod get-state ((mill sofamill)(key symbol) &optional (default nil))
  (let* ((mill-default (fset:map-default (state mill)))
         (found (fset:@ (state mill) key)))
    (if (equal found mill-default)
        default
        found)))

;;; (set-connection :host "mars.local")
;;; (set-connection :name "oppsdaily")
;;; *couchdb*
;;; (get-all-documents)
