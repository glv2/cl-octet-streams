;;;; This file is part of cl-octet-streams
;;;; Copyright 2018 Guillaume LE VAILLANT
;;;; Distributed under the GNU GPL v3 or later.
;;;; See the file LICENSE for terms of use and distribution.


(in-package :octet-streams)


(deftype simple-octet-vector ()
  '(simple-array (unsigned-byte 8) (*)))

(deftype index ()
  '(mod #.array-dimension-limit))

(defclass octet-stream ()
  ((buffer :initarg :buffer
           :accessor octet-stream-buffer
           :type simple-octet-vector)
   (buffer-start :initarg :buffer-start
                 :accessor octet-stream-buffer-start
                 :type index)
   (buffer-end :initarg :buffer-end
               :accessor octet-stream-buffer-end
               :type index)))

(defmethod stream-element-type ((stream octet-stream))
  '(unsigned-byte 8))


(defclass octet-input-stream (octet-stream fundamental-binary-input-stream)
  ())

(defmethod stream-listen ((stream octet-input-stream))
  (< (octet-stream-buffer-start stream) (octet-stream-buffer-end stream)))

(defmethod stream-read-byte ((stream octet-input-stream))
  (let ((buffer (octet-stream-buffer stream))
        (buffer-start (octet-stream-buffer-start stream))
        (buffer-end (octet-stream-buffer-end stream)))
    (if (= buffer-start buffer-end)
        :eof
        (progn
          (setf (octet-stream-buffer-start stream) (1+ buffer-start))
          (aref buffer buffer-start)))))

(defun replace-fast (buffer1 buffer2 start1 end1 start2 end2)
  (declare (type simple-octet-vector buffer1 buffer2)
           (type index start1 end1 start2 end2)
           (optimize (speed 3)))
  (replace buffer1 buffer2 :start1 start1 :end1 end1 :start2 start2 :end2 end2))

(defmethod stream-read-sequence ((stream octet-input-stream) seq start end &key &allow-other-keys)
  (let* ((buffer (octet-stream-buffer stream))
         (buffer-start (octet-stream-buffer-start stream))
         (buffer-end (octet-stream-buffer-end stream))
         (length (min (- end start) (- buffer-end buffer-start))))
    (if (typep seq 'simple-octet-vector)
        ;; Force the use of optimized memory copy functions for simple arrays
        (replace-fast seq buffer start end buffer-start buffer-end)
        (replace seq buffer :start1 start :end1 end :start2 buffer-start :end2 buffer-end))
    (setf (octet-stream-buffer-start stream) (+ buffer-start length))
    (+ start length)))

(defmethod stream-clear-input ((stream octet-input-stream))
  (setf (octet-stream-buffer-start stream) 0)
  (setf (octet-stream-buffer-end stream) 0)
  nil)

(defun make-octet-input-stream (seq &optional (start 0) end)
  "Return an input stream which will supply the bytes of SEQ between
START and END in order."
  (let ((end (or end (length seq))))
    (make-instance 'octet-input-stream
                   :buffer (coerce (subseq seq start end) 'simple-octet-vector)
                   :buffer-start 0
                   :buffer-end (- end start))))

(defmacro with-octet-input-stream ((var seq &optional (start 0) end) &body body)
  "Within BODY, VAR is bound to an octet input stream defined by SEQ,
START and END. The result of the last form of BODY is returned."
  `(with-open-stream (,var (make-octet-input-stream ,seq ,start ,end))
     ,@body))


(defclass octet-output-stream (octet-stream fundamental-binary-output-stream)
  ())

(defmethod stream-write-byte ((stream octet-output-stream) byte)
  (let* ((buffer (octet-stream-buffer stream))
         (buffer-end (octet-stream-buffer-end stream))
         (buffer-length (length buffer)))
    (when (= buffer-end buffer-length)
      (let ((new-buffer (make-array (* 2 buffer-length)
                                    :element-type '(unsigned-byte 8))))
        (replace-fast new-buffer buffer 0 buffer-end 0 buffer-end)
        (setf buffer new-buffer)
        (setf (octet-stream-buffer stream) buffer)))
    (setf (aref buffer buffer-end) byte)
    (setf (octet-stream-buffer-end stream) (1+ buffer-end))
    byte))

(defmethod stream-write-sequence ((stream octet-output-stream) seq start end &key &allow-other-keys)
  (let* ((buffer (octet-stream-buffer stream))
         (buffer-end (octet-stream-buffer-end stream))
         (buffer-length (length buffer))
         (length (- end start)))
    (when (> (+ buffer-end length) buffer-length)
      (let ((new-buffer (make-array (* 2 (max buffer-length length))
                                    :element-type '(unsigned-byte 8))))
        (replace-fast new-buffer buffer 0 buffer-end 0 buffer-end)
        (setf buffer new-buffer)
        (setf (octet-stream-buffer stream) buffer)))
    (if (typep seq 'simple-octet-vector)
        ;; Force the use of optimized memory copy functions for simple arrays
        (replace-fast buffer seq buffer-end (+ buffer-end length) start end)
        (replace buffer seq :start1 buffer-end :start2 start :end2 end))
    (setf (octet-stream-buffer-end stream) (+ buffer-end length))
    seq))

(defmethod stream-clear-output ((stream octet-output-stream))
  (setf (octet-stream-buffer-end stream) 0)
  nil)

(defun get-output-stream-octets (stream)
  "Return the bytes that were written to an octet output STREAM."
  (let ((buffer (octet-stream-buffer stream))
        (buffer-end (octet-stream-buffer-end stream)))
    (setf (octet-stream-buffer-end stream) 0)
    (subseq buffer 0 buffer-end)))

(defun make-octet-output-stream ()
  "Return an output stream which will accumulate all the bytes written
to it for the benefit of the function GET-OUTPUT-STREAM-OCTETS."
  (make-instance 'octet-output-stream
                 :buffer (make-array 128 :element-type '(unsigned-byte 8))
                 :buffer-start 0
                 :buffer-end 0))

(defmacro with-octet-output-stream ((var) &body body)
  "Within BODY, VAR is bound to an octet output stream. After all the
forms in BODY have been executed, the bytes that have been written to
VAR (and that haven't been consumed by a call to
GET-OUTPUT-STREAM-OCTETS within BODY) are returned."
  `(with-open-stream (,var (make-octet-output-stream))
     ,@body
     (get-output-stream-octets ,var)))
