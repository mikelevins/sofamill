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
  ((instance-url :reader get-instance-url :initform nil :initarg :instance-url)
   (database-name :reader get-database-name :initform nil :initarg :database-name)
   (document-ids :reader get-document-ids :initform nil :initarg :document-ids))
  ;; -- panes ---------------------------------------------
  (:panes
   (ids-pane list-panel :reader get-ids-pane
             :items nil :visible-min-width 280
             :callback-type :interface-item
             :selection-callback 'handle-select-document-id)
   (contents-pane editor-pane :text "" :reader get-contents-pane
                  :buffer-name "SofaMill Document Contents"))
  ;; -- layouts ---------------------------------------------
  (:layouts
   
   (main-layout row-layout '(ids-pane contents-pane)
                :ratios '(nil 1)))
  ;; -- default ---------------------------------------------
  (:default-initargs :layout 'main-layout
    :initial-focus 'main-layout
    :title "Documents"
    :create-callback (lambda (intf)
                       (setf (collection-items (get-ids-pane intf))
                             (get-document-ids intf)))))

;;; (put-couch "mars.local" (couch :host "mars.local"))
;;; (defparameter $docids (list-document-ids (get-couch "mars.local") "oppsdaily" :skip 20 :limit 10))
;;; (defparameter $win (capi:contain (make-instance 'documents-pane :document-ids $docids :database-name "oppsdaily" :instance-url "mars.local")))

(defun handle-select-document-id (interface item)
  (let* ((contents (get-document-contents (get-couch (get-instance-url interface))
                                          (get-database-name interface)
                                          item))
         (contents-string (format nil "~S" contents)))
    (setf (editor-pane-text (get-contents-pane interface))
          contents-string)))
