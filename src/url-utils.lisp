;;;; ***********************************************************************
;;;;
;;;; Name:          url-utils.lisp
;;;; Project:       SofaMill: a CouchDB browser
;;;; Purpose:       utilities for working with urls
;;;; Author:        mikel evins
;;;; Copyright:     2018 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package #:sofamill)

(defmacro url-encode (string)
  `(drakma:url-encode ,string +utf-8+))

(defun url-string (&rest parts)
  (apply #'concatenate 'string parts))

