;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-

;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          api.lisp
;;;; Purpose:       XSPF Common Lisp API
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



(defclass xspf ()
  ((playlist :initform nil
             :initarg :playlist
             :accessor xspf-playlist
             :documentation "The playlist."))
  (:documentation "XSPF"))


(defmethod print-object ((xspf xspf) stream)
  (if *print-xspf*
      (with-slots (playlist) xspf
        (format stream "<?xml version=\"1.0\" encoding=\"UTF-8\"?>~%")
        (print-object playlist stream))
      (print-unreadable-object (xspf stream :type t :identity t))))


(define-xspf-class playlist
    (title creator annotation info location identifier image date licence
           attribution link meta extension tracklist))


(defgeneric add-playlist (xspf &key title creator annotation info location
                               identifier image date licence attribution link
                               meta extension)
  (:documentation "Creates a new playlist."))


(defmethod add-playlist ((xspf xspf) &key title creator annotation info location
                         identifier image date licence attribution link
                         meta extension)
  (with-slots (playlist) xspf
    (setf playlist
          (make-instance 'playlist
                         :title title :creator creator :annotation annotation
                         :info info :location location :identifier identifier
                         :image image :date date :licence licence
                         :attribution attribution :link link
                         :meta meta :extension extension
                         :tracklist (make-instance 'track-list :tracks '())))))


(defmethod print-object ((playlist playlist) stream)
  (if *print-xspf*
      (with-slots (title creator annotation info location identifier image date
                         licence attribution link meta extension
                         tracklist) playlist
        (format stream "<playlist version=\"0\" xmlns=\"http://xspf.org/ns/0/\">~%")
        (when title
          (format stream "<title>~A</title>~%" title))
        (when creator
          (format stream "<creator>~A</creator>~%" creator))
        (when annotation
          (format stream "<annotation>~A</annotation>~%" annotation))
        (when identifier
          (format stream "<identifier>~A</identifier>~%" identifier))
        (when image
          (format stream "<image>~A</image>~%" image))
        (when date
          (format stream "<date>~A</date>~%" date))
        (when licence
          (format stream "<licence>~A</licence>~%" licence))
;;         (when attribution
;;           (format stream "<attribution>~A</attribution>~%" attribution))
        (when link
          (format stream "<link>~A</link>~%" link))
        ;;         (when meta
        ;;           (format stream "<meta>~A</meta>~%" meta))
        (when extension
          (format stream "<extension>~A</extension>~%" extension))
        (print-object tracklist stream)
        (format stream "</playlist>"))
      (print-unreadable-object (playlist stream :type t :identity t))))


(define-xspf-class track
    (location identifier title creator annotation info album image
              trackNum duration link meta extension))


(defmethod print-object ((track track) stream)
  (if *print-xspf*
      (with-slots (location identifier title creator annotation info album image
                            trackNum duration link meta extension) track
        (format stream "<track>~%")
        (when identifier
          (format stream "<identifier>~A</identifier>~%" identifier))
        (when title
          (format stream "<title>~A</title>~%" title))
        (when creator
          (format stream "<creator>~A</creator>~%" creator))
        (when duration
          (format stream "<duration>~A</duration>~%" duration))
        (when location
          (format stream "<location>~A</location>~%" location))
        (when annotation
          (format stream "<annotation>~A</annotation>~%" annotation))
        (when info
          (format stream "<info>~A</info>~%" info))
        (when album
          (format stream "<album>~A</album>~%" album))
        (when image
          (format stream "<image>~A</image>~%" image))
        (when trackNum
          (format stream "<trackNum>~A</trackNum>~%" trackNum))
        (when link
          (format stream "<link>~A</link>~%" link))
;;         (when meta
;;           (format stream "<meta>~A</meta>~%" meta))
        (when extension
          (format stream "<extension>~A</extension>~%" extension))
        (format stream "</track>~%"))
      (print-unreadable-object (track stream :type t :identity t))))



(define-xspf-class track-list
    (tracks))


;; (defgeneric add-track-list (xspf)
;;   (:documentation "Creates a new TRACK-LIST"))


;; (defmethod add-track-list ((xspf xspf))
;;   (with-slots (track-list) xspf
;;     (setf track-list
;;           (make-instance 'track-list :tracks '()))))



(defmethod print-object ((track-list track-list) stream)
  (if *print-xspf*
      (when track-list
        (with-slots (tracks) track-list
          (format stream "<trackList>~%")
          (loop for track in tracks
             do (print-object track stream))
          (format stream "</trackList>~%")))
      (print-unreadable-object (track-list stream :type t :identity t))))



(defgeneric add-track (xspf location title creator album
                            &key annotation info image
                            trackNum duration link meta extension identifier)
  (:documentation "Add a new TRACK to the XSPF."))


(defmethod add-track ((xspf xspf) location title creator album
                      &key annotation info image
                      trackNum duration link meta extension identifier)
  (with-slots (playlist) xspf
    (with-slots (tracklist) playlist
      (with-slots (tracks) tracklist
        (unless tracks
          (setf tracks '()))
        ;;(format t "Add ~A - ~A - ~A~%" title creator album)
        (setf tracks
              (nconc tracks
                     (list (make-instance 'track
                                          :location location :title title
                                          :creator creator :album album
                                          :annotation annotation :info info
                                          :image image
                                          :trackNum trackNum :duration duration
                                          :meta meta :link link
                                          :extension extension
                                          :identifier identifier))))))))


(defgeneric get-tracks (xspf)
  (:documentation "Return a list of Track object."))


(defmethod get-tracks ((xspf xspf))
  (with-slots (playlist) xspf
    (with-slots (tracklist) playlist
      (tracks tracklist))))


;; ---------------
;; Input / Output
;; ---------------


(defgeneric read-playlist (xspf filename)
  (:documentation "Read a XSPF file and creates a new XSPF object."))


(defmethod read-playlist ((xspf xspf) filename)
  (let ((ns "urn:xmethods-delayed-quotes")
        (xml (s-xml:parse-xml-file filename :output-type :sxml)))
    (s-xml:register-namespace ns "ns0" :ns0)
    (with-slots (playlist) xspf
      (setf playlist (make-xspf 'playlist (cdddr xml)))
      (when playlist
        ;;(format t "~&Data ~A" playlist)
        (let ((data-playlist (find :|trackList| (cddr xml) :key #'first)))
          (when data-playlist
            (with-slots (tracklist) playlist
              (setf tracklist (make-instance 'track-list))
              (with-slots (tracks) trackList
                (setf tracks
                      (loop for info in (cdr data-playlist)
                         when (consp info)
                         collect (make-xspf 'track info))))))))))
  xspf)

    
(defgeneric write-playlist (xspf filename)
  (:documentation "Write a XSPF file named FILENAME."))


(defmethod write-playlist ((xspf xspf) filename)
  (setf *print-xspf* t)
  (with-open-file (stream filename
                          :external-format :utf-8
                          :direction :output
                          :if-exists :supersede
                          :if-does-not-exist :create)
      (print-object xspf stream))
  (setf *print-xspf* nil))

