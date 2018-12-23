;;;; ***********************************************************************
;;;;
;;;; Name:          couches-pane.lisp
;;;; Project:       SofaMill: a CouchDB browser
;;;; Purpose:       a UI that displays a list of couchDB instances
;;;; Author:        mikel evins
;;;; Copyright:     2018 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package #:sofamill)

(define-interface couches-pane ()
  ;; -- slots ---------------------------------------------
  ()
  ;; -- panes ---------------------------------------------
  (:panes
   (couches-list multi-column-list-panel :reader get-couches-list
                 :items (list-couches) :columns `((:title "Name"))
                 :column-function (lambda (item)(list item)))
   (add-button push-button :text "Add..." :reader get-add-button
               :visible-max-height 32
               :external-max-height 32))
  ;; -- layouts ---------------------------------------------
  (:layouts
   (buttons-layout row-layout '(add-button) :reader get-buttons-layout
                   :visible-min-height 32)
   (couches-layout simple-layout '(couches-list) 
                   :visible-min-height 96
                   :visible-min-width 128)
   (main-layout column-layout '(buttons-layout couches-layout)
                :ratios '(nil 1)))
  ;; -- default ---------------------------------------------
  (:default-initargs :layout 'main-layout
   :initial-focus 'main-layout
   :title "CouchDB Instances"
   :create-callback (lambda (intf)
                      )))