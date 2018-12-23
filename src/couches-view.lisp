;;;; ***********************************************************************
;;;;
;;;; Name:          couches-view.lisp
;;;; Project:       SofaMill: a CouchDB browser
;;;; Purpose:       a UI for browsing known couch instances
;;;; Author:        mikel evins
;;;; Copyright:     2018 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package #:sofamill)

;;; ---------------------------------------------------------------------
;;; UI
;;; ---------------------------------------------------------------------

(define-interface couches-view ()
  ;; -- slots ---------------------------------------------
  ()
  ;; -- panes ---------------------------------------------
  (:panes
   (couches-pane list-panel :items (list-couches))
   (add-button push-button :text "Add..." :reader get-add-button
               :visible-max-height 32
               :external-max-height 32)
   (remove-button push-button :text "Remove" :reader get-remove-button
                  :visible-max-height 32
                  :external-max-height 32))
  ;; -- layouts ---------------------------------------------
  (:layouts
   (buttons-layout row-layout '(add-button remove-button) :reader get-buttons-layout
                   :visible-min-height 32)
   (couches-layout simple-layout '(couches-pane) 
                   :visible-min-height 96)
   (main-layout column-layout '(buttons-layout couches-layout)
                :ratios '(nil 1)))
  ;; -- default ---------------------------------------------
  (:default-initargs :layout 'main-layout
   :initial-focus 'main-layout
   :title "CouchDB Instances"
   :create-callback (lambda (intf) )))

;;; (defparameter $win (contain (make-instance 'couches-view)))
