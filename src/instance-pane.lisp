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
  ((instance-name :accessor get-instance-name :initform nil :initarg :instance-name))
  ;; -- panes ---------------------------------------------
  (:panes
   (host-pane text-input-pane :reader get-host-pane :text *default-host*)
   (host-label-pane title-pane :text "Hostname: ")
   (port-pane text-input-pane :reader get-port-pane :text *default-port*)
   (port-label-pane title-pane :text "Port: ")
   (protocol-pane text-input-pane :reader get-protocol-pane :text *default-protocol*)
   (protocol-label-pane title-pane :text "Protocol: ")
   (db-name-pane text-input-pane :reader get-db-name-pane :text *default-db-name* :external-min-width 256)
   (db-name-label-pane title-pane :text "Database name: ")
   (username-pane text-input-pane :reader get-username-pane :text *default-user*)
   (username-label-pane title-pane :text "Username: ")
   (password-pane password-pane :reader get-password-pane :text *default-password*)
   (password-label-pane title-pane :text "Password: ")
   (accept-button push-button :text "Okay" :external-min-height 32)
   (cancel-button push-button :text "Cancel" :external-min-height 32))
  ;; -- layouts ---------------------------------------------
  (:layouts
   (buttons-layout row-layout '(accept-button cancel-button))
   (main-layout grid-layout '(protocol-label-pane 
                              protocol-pane 
                              host-label-pane host-pane
                              port-label-pane port-pane
                              db-name-label-pane db-name-pane 
                              username-label-pane username-pane
                              password-label-pane password-pane
                              nil buttons-layout)
                :columns 2 :x-adjust :right :y-adjust :center))
  ;; -- default ---------------------------------------------
  (:default-initargs :layout 'main-layout
   :initial-focus 'main-layout
   :title "CouchDB"
   :create-callback (lambda (intf)
                      (let* ((instance-name (get-instance-name intf))
                             (couch-instance (get-couch instance-name)))
                        (if couch-instance
                            (let ((title instance-name)
                                  (host (get-key couch-instance :host))
                                  (port (get-key couch-instance :port))
                                  (protocol (get-key couch-instance :protocol))
                                  (db-name (get-key couch-instance :name))
                                  (username (get-key couch-instance :user))
                                  (password (get-key couch-instance :password)))
                              (setf (interface-title intf) title
                                    (text-input-pane-text (get-host-pane intf)) host
                                    (text-input-pane-text (get-port-pane intf)) port
                                    (text-input-pane-text (get-protocol-pane intf)) protocol
                                    (text-input-pane-text (get-db-name-pane intf)) db-name
                                    (text-input-pane-text (get-username-pane intf)) username))
                          (setf (interface-title intf)
                                (text-input-pane-text (get-host-pane intf))))))))

;;; (defparameter $win (contain (make-instance 'instance-pane)))
;;; (put-couch "localhost" (couch :host "localhost"))
;;; (put-couch "mars.local" (couch :host "mars.local" :db-name "oppsdaily"))
;;; (defparameter $win (contain (make-instance 'instance-pane :instance-name "mars.local")))

