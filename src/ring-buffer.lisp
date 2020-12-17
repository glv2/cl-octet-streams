;;;; This file is part of cl-octet-streams
;;;; Copyright 2020 Guillaume LE VAILLANT
;;;; Distributed under the GNU GPL v3 or later.
;;;; See the file LICENSE for terms of use and distribution.


(in-package :octet-streams)


(deftype simple-octet-vector ()
  '(simple-array (unsigned-byte 8) (*)))

(deftype index ()
  '(mod #.array-dimension-limit))

(defun replace-fast (buffer1 buffer2 start1 end1 start2 end2)
  (declare (type simple-octet-vector buffer1 buffer2)
           (type index start1 end1 start2 end2)
           (optimize (speed 3)))
  (replace buffer1 buffer2
           :start1 start1 :end1 end1
           :start2 start2 :end2 end2))

(defmacro replace* (seq1 seq2 &key (start1 0) end1 (start2 0) end2)
  `(if (and (typep ,seq1 'simple-octet-vector)
            (typep ,seq2 'simple-octet-vector))
       ;; Force use of optimized memory copy functions for simple arrays
       (replace-fast ,seq1 ,seq2 ,start1 ,end1 ,start2 ,end2)
       (replace ,seq1 ,seq2
                :start1 ,start1 :end1 ,end1
                :start2 ,start2 :end2 ,end2)))

(defclass ring-buffer ()
  ((buffer :initarg :buffer
           :accessor buffer
           :type simple-octet-vector)
   (size :initarg :size
         :accessor buffer-size
         :type index)
   (start :initarg :start
          :accessor buffer-start
          :type index)
   (end :initarg :end
        :accessor buffer-end
               :type index)
   (count :initarg :count
          :accessor buffer-count
          :type index)))

(defgeneric clear (ring-buffer))

(defmethod clear ((ring-buffer ring-buffer))
  (with-slots (start end count) ring-buffer
    (setf start 0)
    (setf end 0)
    (setf count 0))
  ring-buffer)

(defgeneric resize (ring-buffer new-size))

(defmethod resize ((ring-buffer ring-buffer) new-size)
  (with-slots (buffer size start end count) ring-buffer
    (when (> count new-size)
      (error "A size of ~d is too small for a buffer containing ~d bytes."
             new-size count))
    (let ((new-buffer (make-array new-size :element-type '(unsigned-byte 8))))
      (when (plusp count)
        (if (< start end)
            (replace-fast new-buffer buffer 0 count start end)
            (let* ((length1 (- size start))
                   (length2 (- count length1)))
              (replace-fast new-buffer buffer 0 length1 start size)
              (when (plusp length2)
                (replace-fast new-buffer buffer length1 count 0 length2)))))
      (setf buffer new-buffer)
      (setf size new-size)
      (setf start 0)
      (setf end count)))
  ring-buffer)

(defgeneric push-data (ring-buffer seq &optional start end))

(defmethod push-data ((ring-buffer ring-buffer) seq &optional (start 0) end)
  (let* ((seq-start start)
         (seq-end (or end (length seq)))
         (length (- seq-end seq-start)))
    (let ((size (buffer-size ring-buffer))
          (count (buffer-count ring-buffer)))
      (when (> (+ count length) size)
        (resize ring-buffer (* 2 (max size length)))))
    (with-slots (buffer size start end count) ring-buffer
      (cond
        ((< start end)
         (let* ((length1 (min length (- size end)))
                (length2 (- length length1)))
           (replace* buffer seq
                     :start1 end :end1 (+ end length1)
                     :start2 seq-start :end2 (+ seq-start length1))
           (when (plusp length2)
             (replace* buffer seq
                       :start1 0 :end1 length2
                       :start2 (+ seq-start length1) :end2 seq-end))
           (incf count length)
           (incf end length)
           (when (> end size)
             (decf end size))))
        (t
         (replace* buffer seq
                   :start1 end :end1 (+ end length)
                   :start2 seq-start :end2 seq-end)
         (incf count length)
         (incf end length)))))
  ring-buffer)

(defgeneric pop-data (ring-buffer length &optional seq start))

(defmethod pop-data ((ring-buffer ring-buffer) length &optional seq (start 0))
  (let* ((length (min length (buffer-count ring-buffer)))
         (seq-start (if seq start 0))
         (seq (or seq (make-array length :element-type '(unsigned-byte 8)))))
    (with-slots (buffer size start end count) ring-buffer
      (cond
        ((< start end)
         (replace* seq buffer
                   :start1 seq-start :end1 (+ seq-start length)
                   :start2 start :end2 (+ start length))
         (incf start length)
         (decf count length))
        (t
         (let* ((length1 (min length (- size start)))
                (length2 (- length length1)))
           (replace* seq buffer
                     :start1 seq-start :end1 (+ seq-start length1)
                     :start2 start :end2 (+ start length1))
           (when (plusp length2)
             (replace* seq buffer
                       :start1 (+ seq-start length1) :end1 (+ seq-start length)
                       :start2 0 :end2 length2))
           (setf start (mod (+ start length) size))
           (decf count length)))))
    (values seq length)))
