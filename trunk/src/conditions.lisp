;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-

;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          conditions.lisp
;;;; Purpose:       cl-xspf conditions.
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



(define-condition xspf-error (simple-error)
  ()
  (:documentation "Main XSPF error."))


