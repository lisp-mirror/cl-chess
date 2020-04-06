(defpackage #:cl-chess/util
  (:use #:cl
        #:zr-utils)
  (:export #:do-read-char
           #:do-space-separated-line))

(in-package #:cl-chess/util)

(defmacro do-read-char ((char stream &key eof no-hang end-var) &body body)
  (let* ((read-char (if no-hang 'read-char-no-hang 'read-char))
         (end-condition `(eql ,eof ,char))
         (end-condition (if no-hang `(or (null ,char) ,end-condition) end-condition))
         (end-condition (if end-var `(and ,end-var ,end-condition) end-condition)))
    `(do ((,char (,read-char ,stream nil ,eof)
                 (,read-char ,stream nil ,eof)))
         (,end-condition)
       ,@body)))

(defmacro do-space-separated-line ((line word start-position end-position &optional end-var)
                                   &body body)
  (let* ((end-condition `(null ,start-position))
         (end-condition (if end-var `(or ,end-condition ,end-var) end-condition)))
    `(loop ,@(if end-var `(:with ,end-var := nil) nil)
           :for ,start-position :of-type (maybe fixnum) := 0
             :then (if ,end-position (1+ ,end-position) nil)
           :for ,end-position :of-type (maybe fixnum)
             := (if (null ,start-position)
                    nil
                    (position #\Space ,line :start ,start-position))
           :for ,word :of-type fixnum :from 0
           :until ,end-condition
           :do
              (progn ,@body))))
