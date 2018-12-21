;;;; ***********************************************************************
;;;;
;;;; Name:          instance-view.lisp
;;;; Project:       SofaMill: a CouchDB browser
;;;; Purpose:       a UI for contacting and inspecting CouchDB instances
;;;; Author:        mikel evins
;;;; Copyright:     2018 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package #:sofamill)

;;; ---------------------------------------------------------------------
;;; UI
;;; ---------------------------------------------------------------------

(define-interface instance-view ()
  ;; -- slots ---------------------------------------------
  ()
  ;; -- panes ---------------------------------------------
  (:panes
   )
  ;; -- layouts ---------------------------------------------
  (:layouts
   
   (main-layout column-layout '()))
  ;; -- default ---------------------------------------------
  (:default-initargs :layout 'main-layout
    :initial-focus 'main-layout
    :title "CouchDB"
    :width 800 :height 600
    :create-callback (lambda (intf) nil)))

;;; (defparameter $win (contain (make-instance 'instance-view)))
