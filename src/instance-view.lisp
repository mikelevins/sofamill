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
   (name-pane text-input-pane :text (clouchdb:db-name *couchdb*) :external-min-width 256)
   (name-label-pane title-pane :text "Server name: ")
   (protocol-pane text-input-pane :text (clouchdb:db-protocol *couchdb*))
   (protocol-label-pane title-pane :text "Protocol: ")
   (host-pane text-input-pane :text (clouchdb:db-host *couchdb*))
   (host-label-pane title-pane :text "Hostname: ")
   (port-pane text-input-pane :text (clouchdb:db-port *couchdb*))
   (port-label-pane title-pane :text "Port: ")
   (username-pane text-input-pane :text (clouchdb:db-user *couchdb*))
   (username-label-pane title-pane :text "Username: ")
   (password-pane text-input-pane :text (clouchdb:db-password *couchdb*))
   (password-label-pane title-pane :text "Password: "))
  ;; -- layouts ---------------------------------------------
  (:layouts
   (main-layout grid-layout '(name-label-pane name-pane host-label-pane host-pane
                                              port-label-pane port-pane username-label-pane username-pane
                                              password-label-pane password-pane)
                :columns 2 :x-adjust :right :y-adjust :center))
  ;; -- default ---------------------------------------------
  (:default-initargs :layout 'main-layout
   :initial-focus 'main-layout
   :title "CouchDB"
   :create-callback (lambda (intf)
                      (setf (interface-title intf) 
                            (or (clouchdb:db-host *couchdb*)
                                "CouchDB")))))

;;; (defparameter $win (contain (make-instance 'instance-view)))
