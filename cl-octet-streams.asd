;;;; This file is part of cl-octet-streams
;;;; Copyright 2018-2020 Guillaume LE VAILLANT
;;;; Distributed under the GNU GPL v3 or later.
;;;; See the file LICENSE for terms of use and distribution.


(defsystem "cl-octet-streams"
  :name "cl-octet-streams"
  :description "In-memory octet streams"
  :version "1.0"
  :license "GPL-3"
  :author "Guillaume LE VAILLANT"
  :depends-on ("trivial-gray-streams")
  :in-order-to ((test-op (test-op "cl-octet-streams/tests")))
  :components ((:module "src"
                :serial t
                :components ((:file "package")
                             (:file "ring-buffer")
                             (:file "octet-streams")
                             (:file "octet-pipes")
                             (:file "connected-octet-streams")))))

(defsystem "cl-octet-streams/tests"
  :name "cl-octet-streams/tests"
  :description "Tests for cl-octet-streams"
  :version "1.0"
  :license "GPL-3"
  :author "Guillaume LE VAILLANT"
  :depends-on ("cl-octet-streams" "fiveam")
  :in-order-to ((test-op (load-op "cl-octet-streams/tests")))
  :perform (test-op (o s)
                    (let ((tests (uiop:find-symbol* 'cl-octet-streams
                                                    :cl-octet-streams/tests)))
                      (uiop:symbol-call :fiveam 'run! tests)))
  :components ((:module "tests"
                :serial t
                :components ((:file "tests")
                             (:file "octet-input-streams")
                             (:file "octet-output-streams")
                             (:file "octet-pipes")
                             (:file "connected-octet-streams")))))
