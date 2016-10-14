;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-

;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          tools.lisp
;;;; Purpose:       cl-xspf Tools
;;;; Programmer:    Nicolas Lamirault <nicolas.lamirault@gmail.com>
;;;;
;;;; This file, part of cl-xspf, is Copyright (c) 2007 by Nicolas Lamirault
;;;;
;;;; cl-xspf users are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser GNU Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.
;;;;
;;;; *************************************************************************


(in-package :cl-xspf)


(eval-when (:compile-toplevel :load-toplevel :execute)

  (defun as-keyword (sym)
    "Translate a symbol to the corresponding keyword symbol"
    (intern (string sym) :keyword))


  (defun keyword-name (keyword)
    "Translate a keyword."
    (format nil "~A" keyword))


  (defun symbol-to-slot (slot)
    (let ((initarg (substitute #\- #\_ (string-downcase (symbol-name slot)))))
      `(,slot
        :initarg ,(as-keyword (string-upcase initarg))
        :initform nil
        :accessor ,slot))))

;; ------
;; Tools
;; ------


(defun xml-to-initargs (xml)
  "Extract initargs from XML."
  (let (args)
    (loop for couple in (cdr xml)
       do (destructuring-bind (key value)
              `(,(as-keyword (string-upcase
                              (substitute #\- #\_
                                          (symbol-name (car couple)))))
                 ,(cadr couple))
            (push value args)
            (push key args)))
    args))


 (defun make-xspf (class xml)
   "Creates a new XSPF CLASS object with initargs from XML."
   ;;(format t "~&--->~A~%XML : ~A ~A" class xml (type-of xml))
   (apply 'make-instance class (xml-to-initargs xml)))


(defmacro define-xspf-class (name slots)
  "Macro which creates a new object."
  `(defclass ,name ()
     ,(mapcar 'symbol-to-slot slots)))