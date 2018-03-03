;;;; This file is part of cl-octet-streams
;;;; Copyright 2018 Guillaume LE VAILLANT
;;;; Distributed under the GNU GPL v3 or later.
;;;; See the file LICENSE for terms of use and distribution.


(defpackage :cl-octet-streams
  (:nicknames :octet-streams)
  (:use :cl :trivial-gray-streams)
  (:export #:make-octet-input-stream
           #:with-octet-input-stream
           #:make-octet-output-stream
           #:get-output-stream-octets
           #:with-octet-output-stream
           #:make-octet-pipe
           #:with-octet-pipe
           #:make-connected-stream-pair
           #:with-connected-stream-pair))
