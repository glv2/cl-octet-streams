;;;; This file is part of cl-octet-streams
;;;; Copyright 2018 Guillaume LE VAILLANT
;;;; Distributed under the GNU GPL v3 or later.
;;;; See the file LICENSE for terms of use and distribution.


(in-package :cl-octet-streams)


(defclass octet-pipe (fundamental-binary-input-stream fundamental-binary-output-stream)
  ((input :initarg :input :accessor octet-pipe-input)
   (output :initarg :output :accessor octet-pipe-output)))

(defmethod stream-element-type ((stream octet-pipe))
  '(unsigned-byte 8))

(defun flush-output-to-input (octet-pipe)
  (let ((buffer (get-output-stream-octets (octet-pipe-output octet-pipe))))
    (unless (zerop (length buffer))
      (setf (octet-pipe-input octet-pipe)
            (make-concatenated-stream (octet-pipe-input octet-pipe)
                                      (make-octet-input-stream buffer))))))

(defmethod stream-write-byte ((stream octet-pipe) byte)
  (write-byte byte (octet-pipe-output stream)))

(defmethod stream-read-byte ((stream octet-pipe))
  (let* ((input (octet-pipe-input stream))
         (byte (read-byte input nil :eof)))
    (if (equal byte :eof)
        (progn
          (flush-output-to-input stream)
          (read-byte input nil :eof))
        byte)))

(defmethod stream-write-sequence ((stream octet-pipe) seq start end &key &allow-other-keys)
  (write-sequence seq (octet-pipe-output stream) :start start :end end))

(defmethod stream-read-sequence ((stream octet-pipe) seq start end &key &allow-other-keys)
  (flush-output-to-input stream)
  (read-sequence seq (octet-pipe-input stream) :start start :end end))

(defmethod stream-clear-output ((stream octet-pipe))
  (clear-output (octet-pipe-output stream)))

(defmethod stream-clear-output ((stream octet-pipe))
  (clear-input (octet-pipe-input stream)))

(defun make-octet-pipe ()
  (let ((empty (make-array 0 :element-type '(unsigned-byte 8))))
    (make-instance 'octet-pipe
                   :input (make-octet-input-stream empty)
                   :output (make-octet-output-stream))))

(defmacro with-octet-pipe ((var) &body body)
  `(with-open-stream (,var (make-octet-pipe))
     ,@body))
