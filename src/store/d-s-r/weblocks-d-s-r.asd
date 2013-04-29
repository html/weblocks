
;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-
(defpackage #:weblocks-d-s-r-asd
  (:use :cl :asdf))

(in-package :weblocks-d-s-r-asd)

(defsystem weblocks-d-s-r
  :name "weblocks-clsql"
  :maintainer "Olexiy Zamkoviy"
  :author "Olexiy Zamkoviy"
  :licence "LLGPL"
  :description "A weblocks backend for de.setf.resource."
  :depends-on (:de.setf.resource :weblocks)
  :components ((:file "d-s-r")))

