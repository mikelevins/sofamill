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

(defparameter *ids-pane-font-size* 14)

(define-interface documents-pane ()
  ;; -- slots ---------------------------------------------
  ((instance-url :reader get-instance-url :initform nil :initarg :instance-url)
   (database-name :reader get-database-name :initform nil :initarg :database-name)
   (document-ids :reader get-document-ids :initform nil :initarg :document-ids))
  ;; -- panes ---------------------------------------------
  (:panes
   (ids-pane list-panel :reader get-ids-pane
             :items nil 
             :font (gp:make-font-description :size *ids-pane-font-size*)
             :visible-min-width 280
             :visible-min-height 200
             :callback-type :interface-item
             :interaction :single-selection
             :selection-callback 'handle-select-document-id))
  ;; -- layouts ---------------------------------------------
  (:layouts
   (contents-layout grid-layout '() :reader get-contents-layout :columns 2
                    :background :white
                    :horizontal-scroll t
                    :vertical-scroll t)
   (main-layout row-layout '(ids-pane contents-layout)
                :ratios '(nil 1)))
  ;; -- default ---------------------------------------------
  (:default-initargs :layout 'main-layout
   :initial-focus 'main-layout
   :title "Documents"
   :create-callback (lambda (intf)
                      (setf (collection-items (get-ids-pane intf))
                            (get-document-ids intf))
                      (update-contents-layout intf
                                              (choice-selected-item (get-ids-pane intf))))))

;;; (put-couch "mars.local" (couch :host "mars.local"))
;;; (defparameter $couch (get-couch "mars.local"))
;;; (defparameter $docids (list-document-ids $couch "reddit_corpus" :skip 1200 :limit 100))
;;; (defparameter $win (capi:contain (make-documents-pane "mars.local" $docids "reddit_corpus")))

(defun make-documents-pane (url docids dbname )
  (make-instance 'documents-pane
                 :document-ids docids
                 :database-name dbname
                 :instance-url url))

(defparameter *data-pane-font-size* 14)

(defmethod make-data-pane ((x null))
  (make-instance 'display-pane :text "" 
                 :font (gp:make-font-description :size *data-pane-font-size*)))

(defmethod make-data-pane ((x symbol))
  (make-instance 'display-pane :text (symbol-name x) 
                 :font (gp:make-font-description :size *data-pane-font-size*)))

(defmethod make-data-pane ((x string))
  (if (< (length x) 128)
      (make-instance 'display-pane :text x
                     :font (gp:make-font-description :size *data-pane-font-size*))
    (make-instance 'display-pane :text x
                   :font (gp:make-font-description :size *data-pane-font-size*)
                   :visible-min-width 280
                   :visible-max-width 400
                   :visible-min-height 200
                   :visible-max-height 400
                   :horizontal-scroll t
                   :vertical-scroll t)))

(defmethod make-data-pane ((x number)) 
  (make-instance 'display-pane :text (format nil "~A" x) 
                 :font (gp:make-font-description :size *data-pane-font-size*)))

(defun handle-select-document-id (interface item)
  (update-contents-layout interface item))

(defun update-contents-layout (intf selected-item)
  (let* ((contents (alist->plist (get-document (get-couch (get-instance-url intf))
                                               (get-database-name intf)
                                               selected-item)))
         (contents-panes (mapcar 'make-data-pane contents)))
    (setf (layout-description (get-contents-layout intf))
          contents-panes)))

