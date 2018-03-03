;;;; This file is part of cl-octet-streams
;;;; Copyright 2018 Guillaume LE VAILLANT
;;;; Distributed under the GNU GPL v3 or later.
;;;; See the file LICENSE for terms of use and distribution.


(defsystem "cl-octet-streams"
  :name "cl-octet-streams"
  :description "In-memory octet streams"
  :version "1.0"
  :license "GPL-3"
  :author "Guillaume LE VAILLANT"
  :depends-on ("trivial-gray-streams")
  :components ((:module "src"
                :serial t
                :components ((:file "package")
                             (:file "octet-streams")
                             (:file "octet-pipes")
                             (:file "connected-stream-pairs")))))
