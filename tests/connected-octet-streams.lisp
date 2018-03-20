;;;; This file is part of cl-octet-streams
;;;; Copyright 2018 Guillaume LE VAILLANT
;;;; Distributed under the GNU GPL v3 or later.
;;;; See the file LICENSE for terms of use and distribution.


(in-package :cl-octet-streams/tests)


(def-suite connected-octet-streams
  :description "Unit tests for connected octet streams"
  :in cl-octet-streams)

(in-suite connected-octet-streams)

(test make-connected-octet-streams
  (multiple-value-bind (s1 s2)
      (make-connected-octet-streams)
    (is-true (typep s1 'stream))
    (is-true (typep s2 'stream)))
  (with-connected-octet-streams (s1 s2)
    (is-true (typep s1 'stream))
    (is-true (typep s2 'stream))))

(test listen-connected-streams
  (with-connected-octet-streams (s1 s2)
    (write-byte 3 s1)
    (write-byte 6 s2)
    (is-true (listen s1))
    (read-byte s1 nil :eof)
    (is-false (listen s1))
    (is-true (listen s2))
    (read-byte s2 nil :eof)
    (is-false (listen s2))))

(test read-byte-connected-octet-streams
  (with-connected-octet-streams (s1 s2)
    (write-sequence #(0 1 2 3 4) s1)
    (is (eql 0 (read-byte s2 nil :eof)))
    (is (eql 1 (read-byte s2 nil :eof)))
    (is (eql 2 (read-byte s2 nil :eof)))
    (is (eql 3 (read-byte s2 nil :eof)))
    (is (eql 4 (read-byte s2 nil :eof)))
    (is-false (listen s2))
    (is (eql :eof (read-byte s2 nil :eof)))
    (write-sequence #(55 91 2 211 41) s2)
    (is (eql 55 (read-byte s1 nil :eof)))
    (is (eql 91 (read-byte s1 nil :eof)))
    (is (eql 2 (read-byte s1 nil :eof)))
    (is (eql 211 (read-byte s1 nil :eof)))
    (is (eql 41 (read-byte s1 nil :eof)))
    (is (eql :eof (read-byte s1 nil :eof)))
    (write-byte 118 s1)
    (is (eql 118 (read-byte s2 nil :eof)))
    (is (eql :eof (read-byte s2 nil :eof)))))

(test read-sequence-connected-octet-streams
  (with-connected-octet-streams (s1 s2)
    (let ((buffer (make-array 5 :element-type '(unsigned-byte 8))))
      (write-sequence #(0 1 2 3 4 55 91 2 211 41) s1)
      (is (eql 5 (read-sequence buffer s2)))
      (is (equalp #(0 1 2 3 4) buffer))
      (is (eql 3 (read-sequence buffer s2 :end 3)))
      (is (equalp #(55 91 2 3 4) buffer))
      (is (eql 4 (read-sequence buffer s2 :start 2)))
      (is (equalp #(55 91 211 41 4) buffer))
      (is (eql 0 (read-sequence buffer s2)))
      (write-sequence #(0 1 2 3 4 55 91 2 211 41) s2)
      (write-sequence #(0 1 2 3 4 55 91 2 211 41) s2)
      (is (eql 5 (read-sequence buffer s1)))
      (is (equalp #(0 1 2 3 4) buffer))
      (is (eql 5 (read-sequence buffer s1)))
      (is (equalp #(55 91 2 211 41) buffer))
      (is (eql 5 (read-sequence buffer s1)))
      (is (equalp #(0 1 2 3 4) buffer))
      (is (eql 5 (read-sequence buffer s1)))
      (is (equalp #(55 91 2 211 41) buffer))
      (is (eql 0 (read-sequence buffer s1))))))

(test clear-input-connected-octet-streams
  (with-connected-octet-streams (s1 s2)
    (write-sequence #(0 1 2 3 4 55 91 2 211 41) s1)
    (is (eql 0 (read-byte s2 nil :eof)))
    (clear-input s2)
    (is (eql :eof (read-byte s2 nil :eof)))
    (write-sequence #(0 1 2 3 4 55 91 2 211 41) s2)
    (is (eql 0 (read-byte s1 nil :eof)))
    (clear-input s1)
    (is (eql :eof (read-byte s1 nil :eof)))))

(test write-byte-connected-octet-streams
  (with-connected-octet-streams (s1 s2)
    (let ((buffer (make-array 5 :element-type '(unsigned-byte 8))))
      (is (eql 15 (write-byte 15 s1)))
      (is (eql 1 (write-byte 1 s1)))
      (is (eql 156 (write-byte 156 s1)))
      (is (eql 53 (write-byte 53 s1)))
      (is (eql 12 (write-byte 12 s1)))
      (is (eql 5 (read-sequence buffer s2)))
      (is (equalp #(15 1 156 53 12) buffer))
      (is (eql 0 (write-byte 0 s2)))
      (is (eql 1 (write-byte 1 s2)))
      (is (eql 2 (write-byte 2 s2)))
      (is (eql 3 (write-byte 3 s2)))
      (is (eql 4 (write-byte 4 s2)))
      (is (eql 5 (read-sequence buffer s1)))
      (is (equalp #(0 1 2 3 4) buffer)))))

(test write-sequence-connected-octet-streams
  (with-connected-octet-streams (s1 s2)
    (let ((buffer (make-array 500 :element-type '(unsigned-byte 8)))
          (temp (make-array 500 :element-type '(unsigned-byte 8))))
      (dotimes (i 500)
        (setf (aref buffer i) (mod i 256)))
      (is (eq buffer (write-sequence buffer s1 :end 10)))
      (is (eql 10 (read-sequence temp s2)))
      (is (equalp #(0 1 2 3 4 5 6 7 8 9) (subseq temp 0 10)))
      (is (eq buffer (write-sequence buffer s1 :start 10 :end 20)))
      (is (eql 10 (read-sequence temp s2)))
      (is (equalp #(10 11 12 13 14 15 16 17 18 19) (subseq temp 0 10)))
      (is (eq buffer (write-sequence buffer s1)))
      (is (eql 500 (read-sequence temp s2)))
      (is (equalp buffer temp))
      (is (eq buffer (write-sequence buffer s2 :end 40)))
      (is (eq buffer (write-sequence buffer s2 :start 40 :end 213)))
      (is (eq buffer (write-sequence buffer s2 :start 213)))
      (is (eql 500 (read-sequence temp s1)))
      (is (equalp buffer temp)))))

(test clear-output-connected-octet-streams
    (with-connected-octet-streams (s1 s2)
      (let ((buffer (make-array 500 :element-type '(unsigned-byte 8))))
        (dotimes (i 500)
          (setf (aref buffer i) (mod i 256)))
        (is-false (listen s2))
        (is (eq buffer (write-sequence buffer s1)))
        (clear-output s1)
        (is (eql :eof (read-byte s2 nil :eof)))
        (is-false (listen s1))
        (is (eq buffer (write-sequence buffer s2)))
        (clear-output s2)
        (is (eql :eof (read-byte s1 nil :eof))))))
