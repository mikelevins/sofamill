;;;; ***********************************************************************
;;;;
;;;; Name:          instance-pane.lisp
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

(define-interface instance-pane ()
  ;; -- slots ---------------------------------------------
  ()
  ;; -- panes ---------------------------------------------
  (:panes
   (protocol-pane text-input-pane :text *default-protocol*)
   (protocol-label-pane title-pane :text "Protocol: ")
   (host-pane text-input-pane :text *default-host*)
   (host-label-pane title-pane :text "Hostname: ")
   (port-pane text-input-pane :text *default-port*)
   (port-label-pane title-pane :text "Port: ")
   (dbname-pane text-input-pane :text *default-dbname* :external-min-width 256)
   (dbname-label-pane title-pane :text "Database name: ")
   (username-pane text-input-pane :text *default-user*)
   (username-label-pane title-pane :text "Username: ")
   (password-pane password-pane :text *default-password*)
   (password-label-pane title-pane :text "Password: ")
   (accept-button push-button :text "Okay" :external-min-height 32)
   (cancel-button push-button :text "Cancel" :external-min-height 32))
  ;; -- layouts ---------------------------------------------
  (:layouts
   (buttons-layout row-layout '(accept-button cancel-button))
   (main-layout grid-layout '(protocol-label-pane protocol-pane 
                                                  host-label-pane host-pane
                                                  port-label-pane port-pane dbname-label-pane dbname-pane 
                                                  username-label-pane username-pane
                                                  password-label-pane password-pane
                                                  nil buttons-layout)
                :columns 2 :x-adjust :right :y-adjust :center))
  ;; -- default ---------------------------------------------
  (:default-initargs :layout 'main-layout
   :initial-focus 'main-layout
   :title "CouchDB"
   :create-callback (lambda (intf)
                      (setf (interface-title intf) 
                            *default-host*))))

;;; (defparameter $win (contain (make-instance 'instance-pane)))
