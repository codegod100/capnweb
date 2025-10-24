;;;; utils.lisp --- shared utilities for capnweb Lisp examples

(in-package #:capnweb.examples)

(defun %coerce-output-path (designator)
  (etypecase designator
    (pathname designator)
    (string (pathname designator))))

(defun %write-output-file (pathname content if-exists)
  (with-open-file (stream pathname
                           :direction :output
                           :if-does-not-exist :create
                           :if-exists if-exists
                           :external-format :utf-8)
    (write-string content stream))
  pathname)
