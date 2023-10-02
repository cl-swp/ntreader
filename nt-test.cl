(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :tester))

(defpackage nt-test
  (:use :cl :nt))

(defun nt-test (input-str)
  (read-ntriples (make-string-input-stream input-str)))