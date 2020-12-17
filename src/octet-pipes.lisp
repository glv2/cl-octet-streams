;;;; This file is part of cl-octet-streams
;;;; Copyright 2018-2020 Guillaume LE VAILLANT
;;;; Distributed under the GNU GPL v3 or later.
;;;; See the file LICENSE for terms of use and distribution.


(in-package :octet-streams)


(defclass octet-pipe (octet-input-stream octet-output-stream)
  ())

(defun make-octet-pipe ()
  "Return a stream which will supply the bytes that have been written
to it in order."
  (let* ((length 128)
         (buffer (make-array length :element-type '(unsigned-byte 8)))
         (ring-buffer (make-instance 'ring-buffer
                                     :buffer buffer
                                     :size length
                                     :start 0
                                     :end 0
                                     :count 0)))
    (make-instance 'octet-pipe
                   :buffer ring-buffer)))

(defmacro with-octet-pipe ((var) &body body)
  "Within BODY, VAR is bound to an octet pipe. The result of the last
form of BODY is returned."
  `(with-open-stream (,var (make-octet-pipe))
     ,@body))
