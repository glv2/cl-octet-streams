;;;; This file is part of cl-octet-streams
;;;; Copyright 2018 Guillaume LE VAILLANT
;;;; Distributed under the GNU GPL v3 or later.
;;;; See the file LICENSE for terms of use and distribution.


(in-package :cl-octet-streams)


(defun make-connected-stream-pair ()
  (let* ((pipe1 (make-octet-pipe))
         (pipe2 (make-octet-pipe))
         (stream1 (make-two-way-stream pipe1 pipe2))
         (stream2 (make-two-way-stream pipe2 pipe1)))
    (values stream1 stream2)))

(defmacro with-connected-stream-pair ((var1 var2) &body body)
  `(multiple-value-bind (,var1 ,var2)
       (make-io-stream-pair)
     (unwind-protect
          (progn ,@body)
       (close ,var1)
       (close ,var2))))
