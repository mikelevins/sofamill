;;;; ***********************************************************************
;;;;
;;;; Name:          map-utils.lisp
;;;; Project:       SofaMill: a CouchDB browser
;;;; Purpose:       utilities for working with FSet finite maps
;;;; Author:        mikel evins
;;;; Copyright:     2018 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package #:sofamill)

(defmethod contains-key? ((m fset:map) key &key &allow-other-keys)
  (fset:domain-contains? m key))

(defmethod contains-value? ((m fset:map) value &key &allow-other-keys)
  (fset:range-contains? m value))

(defun empty-map (&rest args)
  (fset:empty-map))

(defun finite-map (&rest args)
  (fset:convert 'fset:wb-map
                (loop for tail on args by #'cddr
                      collect (cons (car tail)
                                    (cadr tail)))))

(defmethod get-key ((m fset:map) key &key (default nil) &allow-other-keys)
  (let* ((result (fset:@ m key)))
    (if (fset:equal? result (fset:map-default m))
        default
      result)))

(defmethod map-keys ((m fset:map))
  (fset:convert 'cl:list (fset:domain m)))

(defmethod map-values ((m fset:map))
  (fset:convert 'cl:list (fset:range m)))

(defmethod merge-keys ((map1 cl:null) (map2 fset:map) &key test &allow-other-keys)
  (declare (ignore map1 test))
  map2)

(defmethod merge-keys ((map1 fset:map)(map2 cl:null) &key test &allow-other-keys)
  (declare (ignore map2 test))
  map1)

(defmethod merge-keys ((map1 fset:map)(map2 fset:map)  &key &allow-other-keys)
  (fset:map-union map1 map2))

(defmethod put-key ((m fset:map) key value &key &allow-other-keys)
  (fset:with m key value))

(defmethod remove-key ((m fset:map) key &key &allow-other-keys)
  (fset:less m key))
