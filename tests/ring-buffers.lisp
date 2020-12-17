;;;; This file is part of cl-octet-streams
;;;; Copyright 2020 Guillaume LE VAILLANT
;;;; Distributed under the GNU GPL v3 or later.
;;;; See the file LICENSE for terms of use and distribution.


(in-package :cl-octet-streams/tests)


(def-suite ring-buffers
  :description "Unit tests for ring buffers"
  :in cl-octet-streams)

(in-suite ring-buffers)

(defun make-buffer (size)
  (let ((buffer (make-array size :element-type '(unsigned-byte 8))))
    (dotimes (i size buffer)
      (setf (aref buffer i) (mod i size)))))

(test clear
  (let ((ring-buffer (make-instance 'ring-buffer
                                    :buffer (make-buffer 10)
                                    :size 10
                                    :start 2
                                    :end 6
                                    :count 4)))
    (clear ring-buffer)
    (with-slots (start end count) ring-buffer
      (is (= 0 start))
      (is (= 0 end))
      (is (= 0 count))))
  (let ((ring-buffer (make-instance 'ring-buffer
                                    :buffer (make-buffer 10)
                                    :size 10
                                    :start 6
                                    :end 2
                                    :count 6)))
    (clear ring-buffer)
    (with-slots (start end count) ring-buffer
      (is (= 0 start))
      (is (= 0 end))
      (is (= 0 count)))))

(test resize
  (let ((ring-buffer (make-instance 'ring-buffer
                                    :buffer (make-buffer 10)
                                    :size 10
                                    :start 3
                                    :end 5
                                    :count 2)))
    (resize ring-buffer 2)
    (with-slots (buffer size) ring-buffer
      (is (= 2 size))
      (is (equalp #(3 4) buffer))))
  (let ((ring-buffer (make-instance 'ring-buffer
                                    :buffer (make-buffer 10)
                                    :size 10
                                    :start 1
                                    :end 10
                                    :count 9)))
    (resize ring-buffer 20)
    (with-slots (buffer size) ring-buffer
      (is (= 20 size))
      (is (equalp #(1 2 3 4 5 6 7 8 9) (subseq buffer 0 9)))))
  (let ((ring-buffer (make-instance 'ring-buffer
                                    :buffer (make-buffer 10)
                                    :size 10
                                    :start 0
                                    :end 10
                                    :count 10)))
    (signals error (resize ring-buffer 5))))

(test push-data
  (let ((ring-buffer (make-instance 'ring-buffer
                                    :buffer (make-buffer 10)
                                    :size 10
                                    :start 0
                                    :end 0
                                    :count 0)))
    (push-data ring-buffer #(55 56 57))
    (with-slots (buffer start end count) ring-buffer
      (is (= 3 count))
      (is (= 0 start))
      (is (= 3 end))
      (is (equalp #(55 56 57 3 4 5 6 7 8 9) buffer))))
  (let ((ring-buffer (make-instance 'ring-buffer
                                    :buffer (make-buffer 10)
                                    :size 10
                                    :start 5
                                    :end 8
                                    :count 3)))
    (push-data ring-buffer #(55 56 57 58 59))
    (with-slots (buffer start end count) ring-buffer
      (is (= count 8))
      (is (= start 5))
      (is (= end 3))
      (is (equalp #(57 58 59 3 4 5 6 7 55 56) buffer))))
  (let ((ring-buffer (make-instance 'ring-buffer
                                    :buffer (make-buffer 10)
                                    :size 10
                                    :start 8
                                    :end 5
                                    :count 7)))
    (push-data ring-buffer #(55 56 57 58 59))
    (with-slots (buffer size start end count) ring-buffer
      (is (= 20 size))
      (is (= count 12))
      (is (= start 0))
      (is (= end 12))
      (is (equalp #(8 9 0 1 2 3 4 55 56 57 58 59) (subseq buffer 0 12))))))

(test pop-data
  (let ((ring-buffer (make-instance 'ring-buffer
                                    :buffer (make-buffer 10)
                                    :size 10
                                    :start 0
                                    :end 0
                                    :count 0)))
    (multiple-value-bind (data length) (pop-data ring-buffer 5)
      (is (= 0 length))
      (is (equalp #() data))))
  (let ((ring-buffer (make-instance 'ring-buffer
                                    :buffer (make-buffer 10)
                                    :size 10
                                    :start 2
                                    :end 8
                                    :count 6))
        (seq (make-array 10 :element-type '(unsigned-byte 8))))
    (multiple-value-bind (data length) (pop-data ring-buffer 5 seq)
      (with-slots (start end count) ring-buffer
        (is (= 1 count))
        (is (= 7 start))
        (is (= 8 end))
        (is (eq seq data))
        (is (equalp #(2 3 4 5 6) (subseq data 0 length))))))
  (let ((ring-buffer (make-instance 'ring-buffer
                                    :buffer (make-buffer 10)
                                    :size 10
                                    :start 7
                                    :end 3
                                    :count 6))
        (seq (make-array 20 :element-type '(unsigned-byte 8))))
    (multiple-value-bind (data length) (pop-data ring-buffer 10 seq 5)
      (with-slots (start end count) ring-buffer
        (is (= 0 count))
        (is (= 3 start))
        (is (= 3 end))
        (is (eq seq data))
        (is (equalp #(7 8 9 0 1 2) (subseq data 5 (+ 5 length))))))))

(test buffer-ref
  (let ((ring-buffer (make-instance 'ring-buffer
                                    :buffer (make-buffer 10)
                                    :size 10
                                    :start 0
                                    :end 0
                                    :count 0)))
    (signals error (buffer-ref ring-buffer 0)))
  (let ((ring-buffer (make-instance 'ring-buffer
                                    :buffer (make-buffer 10)
                                    :size 10
                                    :start 0
                                    :end 7
                                    :count 7)))
    (is (= 0 (buffer-ref ring-buffer 0)))
    (is (= 4 (buffer-ref ring-buffer 4)))
    (is (= 6 (buffer-ref ring-buffer 6)))
    (signals error (buffer-ref ring-buffer 7))
    (signals error (buffer-ref ring-buffer 8)))
  (let ((ring-buffer (make-instance 'ring-buffer
                                    :buffer (make-buffer 10)
                                    :size 10
                                    :start 3
                                    :end 8
                                    :count 5)))
    (is (= 3 (buffer-ref ring-buffer 0)))
    (is (= 7 (buffer-ref ring-buffer 4)))
    (signals error (buffer-ref ring-buffer 8)))
  (let ((ring-buffer (make-instance 'ring-buffer
                                    :buffer (make-buffer 10)
                                    :size 10
                                    :start 6
                                    :end 3
                                    :count 7)))
    (is (= 6 (buffer-ref ring-buffer 0)))
    (is (= 0 (buffer-ref ring-buffer 4)))
    (is (= 2 (buffer-ref ring-buffer 6)))
    (signals error (buffer-ref ring-buffer 7))
    (signals error (buffer-ref ring-buffer 17))))

(test search-data
  (let ((ring-buffer (make-instance 'ring-buffer
                                    :buffer (make-buffer 10)
                                    :size 10
                                    :start 4
                                    :end 6
                                    :count 2))
        (jump-table (make-array 256
                                :element-type '(unsigned-byte 8)
                                :initial-element 3)))
    (setf (aref jump-table 4) 1)
    (setf (aref jump-table 3) 2)
    (is-false (search-data ring-buffer #(3 4 5) jump-table))
    (is-false (search-data ring-buffer #(3 4 15) jump-table)))
  (let ((ring-buffer (make-instance 'ring-buffer
                                    :buffer (make-buffer 10)
                                    :size 10
                                    :start 0
                                    :end 7
                                    :count 7))
        (jump-table (make-array 256
                                :element-type '(unsigned-byte 8)
                                :initial-element 3)))
    (setf (aref jump-table 4) 1)
    (setf (aref jump-table 3) 2)
    (is (= 3 (search-data ring-buffer #(3 4 5) jump-table)))
    (is-false (search-data ring-buffer #(3 4 15) jump-table)))
  (let ((ring-buffer (make-instance 'ring-buffer
                                    :buffer (make-buffer 10)
                                    :size 10
                                    :start 8
                                    :end 7
                                    :count 9))
        (jump-table (make-array 256
                                :element-type '(unsigned-byte 8)
                                :initial-element 3)))
    (setf (aref jump-table 4) 1)
    (setf (aref jump-table 3) 2)
    (is (= 5 (search-data ring-buffer #(3 4 5) jump-table)))
    (is-false (search-data ring-buffer #(3 4 15) jump-table))))
