;;;; This file is part of cl-octet-streams
;;;; Copyright 2018 Guillaume LE VAILLANT
;;;; Distributed under the GNU GPL v3 or later.
;;;; See the file LICENSE for terms of use and distribution.


(in-package :cl-octet-streams/tests)


(def-suite octet-output-streams
  :description "Unit tests for octet output streams"
  :in cl-octet-streams)

(in-suite octet-output-streams)

(test make-octet-output-stream
  (is-true (typep (make-octet-output-stream) 'stream))
  (with-octet-output-stream (s)
    (is-true (typep s 'stream))))

(test write-byte-output-stream
  (with-octet-output-stream (s)
    (is (equalp #() (get-output-stream-octets s)))
    (is (= 15 (write-byte 15 s)))
    (is (= 1 (write-byte 1 s)))
    (is (= 156 (write-byte 156 s)))
    (is (= 53 (write-byte 53 s)))
    (is (= 12 (write-byte 12 s)))
    (is (equalp #(15 1 156 53 12) (get-output-stream-octets s)))
    (is (= 0 (write-byte 0 s)))
    (is (= 1 (write-byte 1 s)))
    (is (= 2 (write-byte 2 s)))
    (is (= 3 (write-byte 3 s)))
    (is (= 4 (write-byte 4 s)))
    (is (equalp #(0 1 2 3 4) (get-output-stream-octets s)))))

(test write-sequence-output-stream
  (with-octet-output-stream (s)
    (let ((buffer (make-array 500 :element-type '(unsigned-byte 8))))
      (is (equalp #() (get-output-stream-octets s)))
      (dotimes (i 500)
        (setf (aref buffer i) (mod i 256)))
      (is (eq buffer (write-sequence buffer s :end 10)))
      (is (equalp #(0 1 2 3 4 5 6 7 8 9) (get-output-stream-octets s)))
      (is (eq buffer (write-sequence buffer s :start 10 :end 20)))
      (is (equalp #(10 11 12 13 14 15 16 17 18 19) (get-output-stream-octets s)))
      (is (eq buffer (write-sequence buffer s)))
      (is (equalp buffer (get-output-stream-octets s)))
      (is (eq buffer (write-sequence buffer s :end 40)))
      (is (eq buffer (write-sequence buffer s :start 40 :end 213)))
      (is (eq buffer (write-sequence buffer s :start 213)))
      (is (equalp buffer (get-output-stream-octets s))))))

(test clear-output-output-stream
  (with-octet-output-stream (s)
    (let ((buffer (make-array 500 :element-type '(unsigned-byte 8))))
      (dotimes (i 500)
        (setf (aref buffer i) (mod i 256)))
      (is (equalp #() (get-output-stream-octets s)))
      (is (eq buffer (write-sequence buffer s)))
      (clear-output s)
      (is (equalp #() (get-output-stream-octets s))))))
