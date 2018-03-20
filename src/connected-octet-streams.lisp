;;;; This file is part of cl-octet-streams
;;;; Copyright 2018 Guillaume LE VAILLANT
;;;; Distributed under the GNU GPL v3 or later.
;;;; See the file LICENSE for terms of use and distribution.


(in-package :octet-streams)


(defun make-connected-octet-streams ()
  "Return two streams connected to each other. The bytes written to
the first stream can be read from the second, and the bytes written to
the second stream can be read from the first."
  (let* ((pipe1 (make-octet-pipe))
         (pipe2 (make-octet-pipe))
         (stream1 (make-two-way-stream pipe1 pipe2))
         (stream2 (make-two-way-stream pipe2 pipe1)))
    (values stream1 stream2)))

(defmacro with-connected-octet-streams ((var1 var2) &body body)
  "Within BODY, VAR1 and VAR2 are bound to octet streams connected to each
other. The result of the last form of BODY is returned."
  `(multiple-value-bind (,var1 ,var2)
       (make-connected-octet-streams)
     (unwind-protect
          (progn ,@body)
       (close ,var1)
       (close ,var2))))
