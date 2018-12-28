;;;; ***********************************************************************
;;;;
;;;; Name:          string-utils.lisp
;;;; Project:       SofaMill: a CouchDB browser
;;;; Purpose:       string utility functions
;;;; Author:        mikel evins
;;;; Copyright:     2018 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package #:sofamill)

(defmethod code-vector->string ((v vector))
  (coerce (map 'vector #'code-char v) 
          'string))

(defmethod prefix-match? ((prefix string)(s string))
  (let ((prefix-length (length prefix))
        (string-length (length s)))
    (if (< string-length prefix-length)
        nil
      (string-equal prefix s
                    :start1 0 :end1 prefix-length
                    :start2 0 :end2 prefix-length))))

(defun random-alpha-string (&optional (count 16))
  (let* ((buf (make-array 16 :adjustable t :fill-pointer 0))
         (alphabet "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz")
         (len (length alphabet)))
    (loop for i from 0 below count do (vector-push-extend (elt alphabet (random len)) buf))
    (coerce buf 'string)))

(defmethod take ((n integer)(s string) &key (start 0))
  (subseq s start (+ start n)))

(defmethod take-by ((n integer)(s string) &key (start 0))
  (let ((len (length s)))
    (loop for i from start below len by n 
          collect (let ((end (min len (+ i n))))
                    (subseq s i end)))))

