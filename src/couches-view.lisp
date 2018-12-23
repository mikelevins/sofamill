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
   (couches-pane list-panel :items (list-couches)))
  ;; -- layouts ---------------------------------------------
  (:layouts
   (main-layout column-layout '(couches-pane)))
  ;; -- default ---------------------------------------------
  (:default-initargs :layout 'main-layout
   :initial-focus 'main-layout
   :title "CouchDB Instances"
   :create-callback (lambda (intf) )))

;;; (defparameter $win (contain (make-instance 'couches-view)))
