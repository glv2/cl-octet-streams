;;;; This file is part of cl-octet-streams
;;;; Copyright 2018 Guillaume LE VAILLANT
;;;; Distributed under the GNU GPL v3 or later.
;;;; See the file LICENSE for terms of use and distribution.


(in-package :octet-streams)


(defclass octet-pipe (octet-input-stream fundamental-binary-output-stream)
  ())

(defmethod stream-write-byte ((stream octet-pipe) byte)
  (with-slots (buffer buffer-start buffer-end)
      stream
    (declare (type simple-octet-vector buffer))
    (let* ((buffer-length (length buffer))
           (used-space (- buffer-end buffer-start))
           (free-space (- buffer-length used-space)))
      (cond
        ((zerop free-space)
         (let ((new-buffer (make-array (* 2 buffer-length)
                                       :element-type '(unsigned-byte 8))))
           (replace-fast new-buffer buffer 0 used-space buffer-start buffer-end)
           (setf buffer new-buffer)
           (setf buffer-length (length buffer))
           (setf buffer-start 0)
           (setf buffer-end used-space)))
        ((= buffer-end buffer-length)
         (replace-fast buffer buffer 0 used-space buffer-start buffer-end)
         (setf buffer-start 0)
         (setf buffer-end used-space)))
      (setf (aref buffer buffer-end) byte)
      (incf buffer-end)
      byte)))

(defmethod stream-write-sequence ((stream octet-pipe) seq start end &key &allow-other-keys)
  (with-slots (buffer buffer-start buffer-end)
      stream
    (declare (type simple-octet-vector buffer))
    (let* ((buffer-length (length buffer))
           (used-space (- buffer-end buffer-start))
           (free-space (- buffer-length used-space))
           (length (- end start)))
      (cond
        ((< free-space length)
         (let ((new-buffer (make-array (* 2 (max buffer-length length))
                                       :element-type '(unsigned-byte 8))))
           (replace-fast new-buffer buffer 0 used-space buffer-start buffer-end)
           (setf buffer new-buffer)
           (setf buffer-start 0)
           (setf buffer-end used-space)))
        ((> (+ buffer-end length) buffer-length)
         (replace-fast buffer buffer 0 used-space buffer-start buffer-end)
         (setf buffer-start 0)
         (setf buffer-end used-space)))
      (if (typep seq 'simple-octet-vector)
          ;; Force the use of optimized memory copy functions for simple arrays
          (replace-fast buffer seq buffer-end (+ buffer-end length) start end)
          (replace buffer seq :start1 buffer-end :start2 start :end2 end))
      (incf buffer-end length)
      seq)))

(defmethod stream-clear-output ((stream octet-pipe))
  (setf (octet-stream-buffer-start stream) 0)
  (setf (octet-stream-buffer-end stream) 0)
  nil)

(defun make-octet-pipe ()
  "Return a stream which will supply the bytes that have been written
to it in order."
  (make-instance 'octet-pipe
                 :buffer (make-array 128 :element-type '(unsigned-byte 8))
                 :buffer-start 0
                 :buffer-end 0))

(defmacro with-octet-pipe ((var) &body body)
  "Within BODY, VAR is bound to an octet pipe. The result of the last
form of BODY is returned."
  `(with-open-stream (,var (make-octet-pipe))
     ,@body))
