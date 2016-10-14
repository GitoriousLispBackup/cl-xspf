;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-

;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          package.lisp
;;;; Purpose:       Package definition file for cl-xspf
;;;; Programmer:    Nicolas Lamirault <nicolas.lamirault@gmail.com>
;;;;
;;;; This file, part of cl-xspf, is Copyright (c) 2007 by Nicolas Lamirault
;;;;
;;;; cl-xspf users are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser GNU Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.
;;;;
;;;; *************************************************************************



(defpackage :cl-xspf
  (:use :cl)
  (:documentation "XSPF Common Lisp package")
  (:export #:xspf

           #:playlist
           #:track

           #:add-playlist
           #:add-track
           #:get-tracks

           #:location
           #:identifier
           #:title
           #:creator
           #:annotation
           #:info
           #:image
           #:link
           #:meta
           #:extension

           #:date
           #:licence
           #:attribution       
           #:tracklist

           #:album
           #:trackNum
           #:duration
           #:extension
           
           ;; input/output

           #:read-playlist
           #:write-playlist
   
           #:*print-xspf*

           ;; conditions

           #:xspf-error
           
   ))



