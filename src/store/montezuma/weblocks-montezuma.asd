
;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-
(defpackage #:weblocks-montezuma-asd
  (:use :cl :asdf))

(in-package :weblocks-montezuma-asd)

(defsystem weblocks-montezuma
  :name "weblocks-clsql"
  :maintainer "Olexiy Zamkoviy"
  :author "Olexiy Zamkoviy"
  :licence "LLGPL"
  :description "A weblocks backend for montezuma."
  :depends-on (:montezuma :weblocks)
  :components ((:file "montezuma")))

