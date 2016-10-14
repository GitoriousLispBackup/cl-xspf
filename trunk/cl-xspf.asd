;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-

;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          cl-xspf.asd
;;;; Purpose:       ASDF definition for cl-xspf
;;;; Programmer:    Nicolas Lamirault <nicolas.lamirault@gmail.com>
;;;;
;;;; This file, part of cl-xspf, is Copyright (c) 2007 by Nicolas Lamirault
;;;;
;;;; cl-xspf users are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser GNU Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.
;;;;
;;;; *************************************************************************


(in-package :asdf)


(defsystem cl-xspf
    :name "cl-xspf"
    :author "Nicolas Lamirault <nicolas.lamirault@gmail.com>"
    :maintainer "Nicolas Lamirault <nicolas.lamirault@gmail.com>"
    :version "0.1"
    :licence "Lisp Lesser GNU General Public License"
    :description "Common Lisp API for reading and writing XSPF."
    :depends-on (:s-xml)
    :components
    ((:module :src
              :components
              ((:file "package")
               (:file "conditions" :depends-on ("package"))
               (:file "specials" :depends-on ("package"))
               (:file "tools" :depends-on ("package"))
               (:file "api" :depends-on ("conditions" "specials" "tools"))))))

