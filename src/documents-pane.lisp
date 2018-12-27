;;;; ***********************************************************************
;;;;
;;;; Name:          documents-pane.lisp
;;;; Project:       SofaMill: a CouchDB browser
;;;; Purpose:       a UI that displays a list of documents in a couchDB instance
;;;; Author:        mikel evins
;;;; Copyright:     2018 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package #:sofamill)

(define-interface documents-pane ()
  ;; -- slots ---------------------------------------------
  ((document-ids :reader get-document-ids :initform nil :initarg :document-ids))
  ;; -- panes ---------------------------------------------
  (:panes
   (ids-pane list-panel :reader get-ids-pane
             :items nil)
   (contents-pane editor-pane :reader get-contents-pane
                  :text "<placeholder text>"))
  ;; -- layouts ---------------------------------------------
  (:layouts
   (main-layout row-layout '(ids-pane contents-pane)))
  ;; -- default ---------------------------------------------
  (:default-initargs :layout 'main-layout
    :initial-focus 'main-layout
    :title "Documents"
    :create-callback (lambda (intf)
                       (setf (collection-items (get-ids-pane intf))
                             (get-document-ids intf)))))

;;; (put-couch "mars.local" (couch :host "mars.local"))
;;; (defparameter $docids (list-document-ids (get-couch "mars.local") "oppsdaily" :skip 20 :limit 10))
;;; (defparameter $win (capi:contain (make-instance 'documents-pane :document-ids $docids)))
