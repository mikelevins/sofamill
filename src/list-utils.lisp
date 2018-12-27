;;;; ***********************************************************************
;;;;
;;;; Name:          list-utils.lisp
;;;; Project:       SofaMill: a CouchDB browser
;;;; Purpose:       utilities for working with lists
;;;; Author:        mikel evins
;;;; Copyright:     2018 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package #:sofamill)

;;; alist utils

(defmethod alist-contains-key? ((c null) key) nil)

(defmethod alist-contains-key? ((c cons) key) 
  (if (equal key (caar c))
      (cdar c)
    (alist-contains-key? (cdr c)
                         key)))

(defmethod alist-get-key ((c null) key &key (default nil)) default)

(defmethod alist-get-key ((c cons) key &key (default nil)) 
  (if (equal key (caar c))
      (cdar c)
    (alist-get-key (cdr c) key :default default)))

(defmethod alist-keys ((c null)) nil)

(defmethod alist-keys ((c cons)) 
  (mapcar #'car c))

(defmethod alist-vals ((c null)) nil)

(defmethod alist-vals ((c cons)) 
  (mapcar #'cdr c))
