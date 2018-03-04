;;;; This file is part of cl-octet-streams
;;;; Copyright 2018 Guillaume LE VAILLANT
;;;; Distributed under the GNU GPL v3 or later.
;;;; See the file LICENSE for terms of use and distribution.


(defpackage :cl-octet-streams/tests
  (:use :cl :octet-streams :fiveam))

(in-package :cl-octet-streams/tests)


(def-suite cl-octet-streams
  :description "Unit tests for cl-octet-streams")
