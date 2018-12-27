;;;; ***********************************************************************
;;;;
;;;; Name:          databases-pane.lisp
;;;; Project:       SofaMill: a CouchDB browser
;;;; Purpose:       a UI that displays a list of documents in a couchDB instance
;;;; Author:        mikel evins
;;;; Copyright:     2018 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package #:sofamill)

(define-interface databases-pane ()
  ;; -- slots ---------------------------------------------
  ((database-names :reader get-database-names :initform nil :initarg :database-names))
  ;; -- panes ---------------------------------------------
  (:panes
   (databases-pane list-panel :reader get-databases-pane
                   :items nil :visible-min-width 280))
  ;; -- layouts ---------------------------------------------
  (:layouts
   (main-layout row-layout '(databases-pane)))
  ;; -- default ---------------------------------------------
  (:default-initargs :layout 'main-layout
    :initial-focus 'main-layout
    :title "Databases"
    :create-callback (lambda (intf)
                       (setf (collection-items (get-databases-pane intf))
                             (get-database-names intf)))))

;;; (put-couch "mars.local" (couch :host "mars.local"))
;;; (defparameter $databases (list-databases (get-couch "mars.local")))
;;; (defparameter $win (capi:contain (make-instance 'databases-pane :database-names $databases)))
