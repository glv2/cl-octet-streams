;;;; This file is part of cl-octet-streams
;;;; Copyright 2018 Guillaume LE VAILLANT
;;;; Distributed under the GNU GPL v3 or later.
;;;; See the file LICENSE for terms of use and distribution.


(in-package :cl-octet-streams/tests)


(def-suite octet-input-streams
  :description "Unit tests for octet input streams"
  :in cl-octet-streams)

(in-suite octet-input-streams)

(test make-octet-input-stream
  (is-true (typep (make-octet-input-stream #()) 'stream))
  (is-true (typep (make-octet-input-stream #(1 2 3)) 'stream))
  (is-true (typep (make-octet-input-stream #(1 2 3)) 'stream))
  (is-true (typep (make-octet-input-stream #(0 1 2 3 4 5 6) 3) 'stream))
  (is-true (typep (make-octet-input-stream #(0 1 2 3 4 5 6) 3 5) 'stream))
  (with-octet-input-stream (s #())
    (is-true (typep s 'stream)))
  (with-octet-input-stream (s #(1 2 3))
    (is-true (typep s 'stream)))
  (with-octet-input-stream (s #(0 1 2 3 4 5 6) 3)
    (is-true (typep s 'stream)))
  (with-octet-input-stream (s #(0 1 2 3 4 5 6) 3 5)
    (is-true (typep s 'stream))))

(test listen-input-stream
  (with-octet-input-stream (s #(0 1 2))
    (is-true (listen s))
    (read-byte s nil :eof)
    (is-true (listen s))
    (read-byte s nil :eof)
    (is-true (listen s))
    (read-byte s nil :eof)
    (is-false (listen s))))

(test read-byte-input-stream
  (with-octet-input-stream (s #(0 1 2 3 4 55 91 2 211 41))
    (is (eql 0 (read-byte s nil :eof)))
    (is (eql 1 (read-byte s nil :eof)))
    (is (eql 2 (read-byte s nil :eof)))
    (is (eql 3 (read-byte s nil :eof)))
    (is (eql 4 (read-byte s nil :eof)))
    (is (eql 55 (read-byte s nil :eof)))
    (is (eql 91 (read-byte s nil :eof)))
    (is (eql 2 (read-byte s nil :eof)))
    (is (eql 211 (read-byte s nil :eof)))
    (is (eql 41 (read-byte s nil :eof)))
    (is (eql :eof (read-byte s nil :eof))))
  (with-octet-input-stream (s #(0 1 2 3 4 55 91 2 211 41) 3 6)
    (is (eql 3 (read-byte s nil :eof)))
    (is (eql 4 (read-byte s nil :eof)))
    (is (eql 55 (read-byte s nil :eof)))
    (is (eql :eof (read-byte s nil :eof)))))

(test read-sequence-input-stream
  (with-octet-input-stream (s #(0 1 2 3 4 55 91 2 211 41))
    (let ((buffer (make-array 5 :element-type '(unsigned-byte 8))))
      (is (eql 5 (read-sequence buffer s)))
      (is (equalp #(0 1 2 3 4) buffer))
      (is (eql 3 (read-sequence buffer s :end 3)))
      (is (equalp #(55 91 2 3 4) buffer))
      (is (eql 4 (read-sequence buffer s :start 2)))
      (is (equalp #(55 91 211 41 4) buffer))
      (is (eql 0 (read-sequence buffer s))))))

(test clear-input-input-stream
  (with-octet-input-stream (s #(0 1 2 3 4 55 91 2 211 41))
    (is (eql 0 (read-byte s nil :eof)))
    (clear-input s)
    (is (eql :eof (read-byte s nil :eof)))))
