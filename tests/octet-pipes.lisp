;;;; This file is part of cl-octet-streams
;;;; Copyright 2018 Guillaume LE VAILLANT
;;;; Distributed under the GNU GPL v3 or later.
;;;; See the file LICENSE for terms of use and distribution.


(in-package :cl-octet-streams/tests)


(def-suite octet-pipes
  :description "Unit tests for octet pipes"
  :in cl-octet-streams)

(in-suite octet-pipes)

(test make-octet-pipe
  (is-true (typep (make-octet-pipe) 'stream))
  (with-octet-pipe (s)
    (is-true (typep (make-octet-pipe) 'stream))))

(test read-byte-octet-pipe
  (with-octet-pipe (s)
    (write-sequence #(0 1 2 3 4 55 91 2 211 41) s)
    (is (= 0 (read-byte s nil :eof)))
    (is (= 1 (read-byte s nil :eof)))
    (is (= 2 (read-byte s nil :eof)))
    (is (= 3 (read-byte s nil :eof)))
    (is (= 4 (read-byte s nil :eof)))
    (is (= 55 (read-byte s nil :eof)))
    (is (= 91 (read-byte s nil :eof)))
    (is (= 2 (read-byte s nil :eof)))
    (is (= 211 (read-byte s nil :eof)))
    (is (= 41 (read-byte s nil :eof)))
    (is (eql :eof (read-byte s nil :eof)))
    (write-byte 118 s)
    (is (= 118 (read-byte s nil :eof)))
    (is (eql :eof (read-byte s nil :eof)))))

(test read-sequence-octet-pipe
  (with-octet-pipe (s)
    (let ((buffer (make-array 5 :element-type '(unsigned-byte 8))))
      (write-sequence #(0 1 2 3 4 55 91 2 211 41) s)
      (is (= 5 (read-sequence buffer s)))
      (is (equalp #(0 1 2 3 4) buffer))
      (is (= 3 (read-sequence buffer s :end 3)))
      (is (equalp #(55 91 2 3 4) buffer))
      (is (= 4 (read-sequence buffer s :start 2)))
      (is (equalp #(55 91 211 41 4) buffer))
      (is (= 0 (read-sequence buffer s)))
      (write-sequence #(0 1 2 3 4 55 91 2 211 41) s)
      (write-sequence #(0 1 2 3 4 55 91 2 211 41) s)
      (is (= 5 (read-sequence buffer s)))
      (is (equalp #(0 1 2 3 4) buffer))
      (is (= 5 (read-sequence buffer s)))
      (is (equalp #(55 91 2 211 41) buffer))
      (is (= 5 (read-sequence buffer s)))
      (is (equalp #(0 1 2 3 4) buffer))
      (is (= 5 (read-sequence buffer s)))
      (is (equalp #(55 91 2 211 41) buffer))
      (is (= 0 (read-sequence buffer s))))))

(test clear-input-octet-pipe
  (with-octet-pipe (s)
    (write-sequence #(0 1 2 3 4 55 91 2 211 41) s)
    (is (= 0 (read-byte s nil :eof)))
    (clear-input s)
    (is (eql :eof (read-byte s nil :eof)))))

(test write-byte-octet-pipe
  (with-octet-pipe (s)
    (let ((buffer (make-array 5 :element-type '(unsigned-byte 8))))
      (is (= 15 (write-byte 15 s)))
      (is (= 1 (write-byte 1 s)))
      (is (= 156 (write-byte 156 s)))
      (is (= 53 (write-byte 53 s)))
      (is (= 12 (write-byte 12 s)))
      (is (= 5 (read-sequence buffer s)))
      (is (equalp #(15 1 156 53 12) buffer))
      (is (= 0 (write-byte 0 s)))
      (is (= 1 (write-byte 1 s)))
      (is (= 2 (write-byte 2 s)))
      (is (= 3 (write-byte 3 s)))
      (is (= 4 (write-byte 4 s)))
      (is (= 5 (read-sequence buffer s)))
      (is (equalp #(0 1 2 3 4) buffer)))))

(test write-sequence-octet-pipe
  (with-octet-pipe (s)
    (let ((buffer (make-array 500 :element-type '(unsigned-byte 8)))
          (temp (make-array 500 :element-type '(unsigned-byte 8))))
      (dotimes (i 500)
        (setf (aref buffer i) (mod i 256)))
      (is (eq buffer (write-sequence buffer s :end 10)))
      (is (= 10 (read-sequence temp s)))
      (is (equalp #(0 1 2 3 4 5 6 7 8 9) (subseq temp 0 10)))
      (is (eq buffer (write-sequence buffer s :start 10 :end 20)))
      (is (= 10 (read-sequence temp s)))
      (is (equalp #(10 11 12 13 14 15 16 17 18 19) (subseq temp 0 10)))
      (is (eq buffer (write-sequence buffer s)))
      (is (= 500 (read-sequence temp s)))
      (is (equalp buffer temp))
      (is (eq buffer (write-sequence buffer s :end 40)))
      (is (eq buffer (write-sequence buffer s :start 40 :end 213)))
      (is (eq buffer (write-sequence buffer s :start 213)))
      (is (= 500 (read-sequence temp s)))
      (is (equalp buffer temp)))))

(test clear-output-octet-pipe
    (with-octet-pipe (s)
      (let ((buffer (make-array 500 :element-type '(unsigned-byte 8))))
        (dotimes (i 500)
          (setf (aref buffer i) (mod i 256)))
        (is (eql :eof (read-byte s nil :eof)))
        (is (eq buffer (write-sequence buffer s)))
        (clear-output s)
        (is (eql :eof (read-byte s nil :eof))))))
