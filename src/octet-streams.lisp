;;;; This file is part of cl-octet-streams
;;;; Copyright 2018-2020 Guillaume LE VAILLANT
;;;; Distributed under the GNU GPL v3 or later.
;;;; See the file LICENSE for terms of use and distribution.


(in-package :octet-streams)


(defclass octet-stream ()
  ((buffer :initarg :buffer
           :accessor buffer
           :type ring-buffer)))

(defmethod stream-element-type ((stream octet-stream))
  '(unsigned-byte 8))

(defun octet-stream-length (stream)
  "Return the number of bytes available in STREAM."
  (buffer-count (buffer stream)))

(defun octet-stream-ref (stream index)
  "Return the byte at INDEX in STREAM."
  (buffer-ref (buffer stream) index))


(defclass octet-input-stream (octet-stream fundamental-binary-input-stream)
  ())

(defmethod stream-listen ((stream octet-input-stream))
  (plusp (buffer-count (buffer stream))))

(defmethod stream-read-byte ((stream octet-input-stream))
  (let ((buffer (buffer stream)))
    (if (zerop (buffer-count buffer))
        :eof
        (let ((data (make-array 1 :element-type '(unsigned-byte 8))))
          (declare (dynamic-extent data))
          (pop-data buffer 1 data 0)
          (aref data 0)))))

(defmethod stream-read-sequence ((stream octet-input-stream) seq start end
                                 &key &allow-other-keys)
  (let ((length (- end start))
        (buffer (buffer stream)))
    (multiple-value-bind (seq length) (pop-data buffer length seq start)
      (declare (ignore seq))
      (+ start length))))

(defmethod stream-clear-input ((stream octet-input-stream))
  (clear (buffer stream))
  nil)

(defun make-octet-input-stream (seq &optional (start 0) end)
  "Return an input stream which will supply the bytes of SEQ between
START and END in order."
  (let* ((end (or end (length seq)))
         (length (- end start))
         (buffer (make-array length :element-type '(unsigned-byte 8))))
    (replace* buffer seq :start1 0 :end1 length :start2 start :end2 end)
    (let ((ring-buffer (make-instance 'ring-buffer
                                      :buffer buffer
                                      :size length
                                      :start 0
                                      :end length
                                      :count length)))
      (make-instance 'octet-input-stream
                     :buffer ring-buffer))))

(defmacro with-octet-input-stream ((var seq &optional (start 0) end) &body body)
  "Within BODY, VAR is bound to an octet input stream defined by SEQ,
START and END. The result of the last form of BODY is returned."
  `(with-open-stream (,var (make-octet-input-stream ,seq ,start ,end))
     ,@body))


(defclass octet-output-stream (octet-stream fundamental-binary-output-stream)
  ())

(defmethod stream-write-byte ((stream octet-output-stream) byte)
  (push-data (buffer stream) (vector byte) 0 1)
  byte)

(defmethod stream-write-sequence ((stream octet-output-stream) seq start end
                                  &key &allow-other-keys)
  (push-data (buffer stream) seq start end)
  seq)

(defmethod stream-clear-output ((stream octet-output-stream))
  (clear (buffer stream))
  nil)

(defun get-output-stream-octets (stream)
  "Return the bytes that were written to an octet output STREAM."
  (let ((buffer (buffer stream)))
    (pop-data buffer (buffer-count buffer))))

(defun make-octet-output-stream ()
  "Return an output stream which will accumulate all the bytes written
to it for the benefit of the function GET-OUTPUT-STREAM-OCTETS."
  (let* ((length 128)
         (buffer (make-array length :element-type '(unsigned-byte 8)))
         (ring-buffer (make-instance 'ring-buffer
                                     :buffer buffer
                                     :size length
                                     :start 0
                                     :end 0
                                     :count 0)))
    (make-instance 'octet-output-stream
                   :buffer ring-buffer)))

(defmacro with-octet-output-stream ((var) &body body)
  "Within BODY, VAR is bound to an octet output stream. After all the
forms in BODY have been executed, the bytes that have been written to
VAR (and that haven't been consumed by a call to
GET-OUTPUT-STREAM-OCTETS within BODY) are returned."
  `(with-open-stream (,var (make-octet-output-stream))
     ,@body
     (get-output-stream-octets ,var)))
