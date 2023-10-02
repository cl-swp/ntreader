;;;-*- Mode: common-lisp; syntax: common-lisp; package: nt; base: 10 -*-
;;;
;;;; N-Triples Reader Module
;;;
;;; See, http://www.w3.org/TR/2014/REC-n-triples-20140225/ for N-Triples
;;;      http://www.w3.org/TR/2014/REC-n-quads-20140225/ for N-Quads
;;;
;;; ----------------------------------------------------------------------------------
;;; Copyright (c) 2015-2016, 2023 Seiji Koide <koide@ontolonomy.co.jp>
;;; All rights reserved.
;;;
;;; Permission is hereby granted, free of charge, to any person
;;; obtaining a copy of this software and associated documentation
;;; files (the "Software"), to deal in the Software without
;;; restriction, including without limitation the rights to use, copy,
;;; modify, merge, publish, distribute, sublicense, and/or sell copies
;;; of the Software, and to permit persons to whom the Software is
;;; furnished to do so, subject to the following conditions:
;;;
;;; The above copyright notice and this permission notice shall be
;;; included in all copies or substantial portions of the Software.
;;;
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;;; NONINFRINGEMENT.  IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
;;; HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
;;; WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
;;; DEALINGS IN THE SOFTWARE.
;;;
;; History
;; -------
;; 2014/06/28    File created

(cl:provide :ntreader)

(eval-when (:execute :load-toplevel :compile-toplevel)
  (cl:require :line-reader)
  ) ; end of eval-when

(defpackage :nt 
  (:use :common-lisp)
  (:shadowing-import-from :line #:stream #:simple-stream-read-line #:line-count)
  (:shadow #:with-open-file)
  (:export #:stream #:with-open-file
           #:read-ntriples-file #:read-nquads-file
           ))
(cl:defpackage :swlisp
  (:nicknames :sw)
  (:shadowing-import-from :ndtree #:*predicates*)
  (:shadow uri parse-uri type typep value)
  (:use :common-lisp)
  (:export #:*ntriples-home* #:*predicates*)
  )
(in-package :nt)

(defvar *this-file* (load-time-value (or #.*compile-file-pathname* *load-pathname*)))

(defparameter *ntriples-home*
  (make-pathname :name nil
                 :type nil
                 :defaults *this-file*))

;;;
;;;; N-Triples file stream
;;;
;;; N-Triple stream is a line stream.
;;;

(defun ask-user-nt-file ()
  "asks an NTriples file to user."
  #+:common-graphics
  (cg:ask-user-for-existing-pathname
   "" :allowed-types '(("N-Triples format file" . "*.nt")
                       ("Any file" . "*.*")))
  #-:common-graphics
  (progn
    (format t "~%N-Triples format file name? ")
    (let ((filename (read-line t)))
      (if (zerop (length filename)) nil filename))))

(defun ask-user-nq-file ()
  "asks an N-Quads file to user."
  #+:common-graphics
  (cg:ask-user-for-existing-pathname
   "" :allowed-types '(("N-Triples format file" . "*.nq")
                       ("Any file" . "*.*")))
  #-:common-graphics
  (progn
    (format t "~%N-Quads format file name? ")
    (let ((filename (read-line t)))
      (if (zerop (length filename)) nil filename))))

(defmacro with-open-file (varargs &rest body)
  "calling sequence: `with-open-file (stream filespec {options}*) {declaration}* {form}*'
   this macro adds class option for <line:stream>."
  `(line:with-open-file (,(car varargs) ,(cadr varargs) ,@(cddr varargs))
     ,@body))

#|
(in-package :nt)
:cd
:cd C:\allegro-projects\ntreader
(with-open-file (stream "test.nt" :external-format :utf-8)
  (let (len (line (make-string 1 :initial-element #\Null)))
    (loop while (multiple-value-setq (line len) (simple-stream-read-line stream nil nil line))
        do (format t "~%~D: ~A" (line:line-count stream) (subseq line 0 len)))))

:cd \\landisk-ede03f\disk1\ontologies\schemaorg\data\releases\3.3
(with-open-file (stream "schema.nq" :external-format :utf-8)
  (let (len (line (make-string 1 :initial-element #\Null)))
    (loop while (multiple-value-setq (line len) (simple-stream-read-line stream nil nil line))
        do (format t "~%~D: ~A" (line:line-count stream) (subseq line 0 len)))))

:cd \\landisk-ede03f\disk1\ontologies\DBPedia   ; it takes very very long time to execute
(with-open-file (stream "jawiki-20160407-labels.ttl" :external-format :utf-8)
  (let (len (line (make-string 1 :initial-element #\Null)))
    (loop while (multiple-value-setq (line len) (simple-stream-read-line stream nil nil line))
        do (format t "~%~D: ~A" (line:line-count stream) (subseq line 0 len)))))
|#

(defun read-ntriples-file (&rest args)
  "read-ntriples-file [file] &key tripe-fn subject-fn predicate-fn object-fn
   This function read NTriples from <file>. The real work is done by <triple-fn> and/or 
   additional <subject-fn>, <predicate-fn>, and <object-fn>."
  (let (file)
    (cond ((keywordp (first args))
           (setq file (ask-user-nt-file)))
          (t (setq file (car args))
             (setq args (cdr args))))
    (unless file (return-from read-ntriples-file nil))
    (let ((trpl-fn (second (member :triple-fn args)))
          (subj-fn (second (member :subject-fn args)))
          (pred-fn (second (member :predicate-fn args)))
          (obj-fn (second (member :object-fn args))))
      (with-open-file (stream (merge-pathnames file *ntriples-home*) :external-format :utf-8)
        (read-ntriples stream trpl-fn :subject-fn subj-fn :predicate-fn pred-fn :object-fn obj-fn)))))

(defun read-nquads-file (&rest args)
  "read-nquads-file [file] &key quad-fn subject-fn predicate-fn object-fn graph-fn
   This function read N-Quads from <file>. The real work is done by <quad-fn> and/or 
   additional <subject-fn>, <predicate-fn>, <object-fn>, and <graph-fn>."
  (let (file)
    (cond ((keywordp (first args))
           (setq file (ask-user-nq-file)))
          (t (setq file (car args))
             (setq args (cdr args))))
    (unless file (return-from read-nquads-file nil))
    (let ((quad-fn (second (member :quad-fn args)))
          (subj-fn (second (member :subject-fn args)))
          (pred-fn (second (member :predicate-fn args)))
          (obj-fn (second (member :object-fn args)))
          (graph-fn (second (member :graph-fn args))))
      (with-open-file (stream (merge-pathnames file *ntriples-home*) :external-format :utf-8)
        (read-nquads stream quad-fn :subject-fn subj-fn :predicate-fn pred-fn :object-fn obj-fn :graph-fn graph-fn)))))

(defun read-ntriples (stream triple-fn &key subject-fn predicate-fn object-fn)
  ;; ntriplesDoc ::= triple? (EOL triple)* EOL? 
  (declare (special stream))
  (let (len (line (make-string 1 :initial-element #\Null)))
    (loop while (multiple-value-setq (line len) (line:simple-stream-read-line stream nil nil line))
        do ;(princ ".")
          (restart-case (read-triple-line stream line len triple-fn
                                          :subject-fn subject-fn 
                                          :predicate-fn predicate-fn 
                                          :object-fn object-fn)
             (skip-ntriple-line ()
                                :report "Skip this line."))))
  :done)

(defun read-nquads (stream quad-fn &key subject-fn predicate-fn object-fn graph-fn)
  ;; ntriplesDoc ::= triple? (EOL triple)* EOL? 
  (declare (special stream))
  (let (len (line (make-string 1 :initial-element #\Null)))
    (loop while (multiple-value-setq (line len) (line:simple-stream-read-line stream nil nil line))
        do ;(princ ".")
          (restart-case (read-nquad-line stream line len quad-fn
                                         :subject-fn subject-fn 
                                         :predicate-fn predicate-fn 
                                         :object-fn object-fn
                                         :graph-fn graph-fn)
             (skip-nquad-line ()
                              :report "Skip this line."))))
  :done)

(defun read-triple-line (stream line len triple-fn &key subject-fn predicate-fn object-fn)
  "line ::= ws* ( comment | triple )? eoln  "
  (declare (special stream))
  (when (zerop len) (return-from read-triple-line nil))
  (let ((i (skipbl line 0)))
    ;; The order is very important.
    (cond ((<= len i) nil)       ; null line
          ((nt-comment-p line i) (skip-nt-comment line i))
          (t (multiple-value-bind (subject-start subject-end predicate-start predicate-end object-start object-end)
                 (parse-triple line len i)
               (when triple-fn (funcall triple-fn line subject-start subject-end predicate-start predicate-end object-start object-end))
               (when subject-fn (funcall subject-fn line subject-start subject-end))
               (when predicate-fn (funcall predicate-fn line predicate-start predicate-end))
               (when object-fn (funcall object-fn line object-start object-end))
               t)))))

(defun read-nquad-line (stream line len quad-fn &key subject-fn predicate-fn object-fn graph-fn)
  "line ::= ws* ( comment | quad )? eoln  "
  (declare (special stream))
  (when (zerop len) (return-from read-nquad-line nil))
  (let ((i (skipbl line 0)))
    ;; The order is very important.
    (cond ((<= len i) nil)       ; null line
          ((nt-comment-p line i) (skip-nt-comment line i))
          (t (multiple-value-bind (subject-start subject-end predicate-start predicate-end object-start object-end
                                                 graph-start graph-end)
                 (parse-nquad line len i)
               (when quad-fn (funcall quad-fn line subject-start subject-end predicate-start predicate-end object-start object-end graph-start graph-end))
               (when subject-fn (funcall subject-fn line subject-start subject-end))
               (when predicate-fn (funcall predicate-fn line predicate-start predicate-end))
               (when object-fn (funcall object-fn line object-start object-end))
               (when graph-fn (funcall object-fn line graph-start graph-end))
               t)))))

(defun nt-comment-p (line i)
  (char= (char line i) #\#))
(defun skip-nt-comment (line i)
  (declare (ignore line i))
  nil)

;;;(defun tribial-subject-load (line start-pos end-pos)
;;;  (declare (special stream))
;;;  (let ((line-count (line-count stream)))
;;;    (pinc line-count)
    

#|
(in-package :nt)
:cd \\landisk-ede03f\disk1\ontologies\schemaorg\data\releases\3.3
(nt:read-ntriples-file "all-layers.nt" :triple-fn #'nt:add-triple)
(ndtree:fetch `(,(ns:intern-iri "http://www.w3.org/2000/01/rdf-schema#label") ?s . ?o))
(ndtree:fetch `(,(ns:intern-iri "http://www.w3.org/1999/02/22-rdf-syntax-ns#type") ?s . ?o))
(defun mappend (fn &rest lists)
  "Apply <fn> to respective elements of list(s), and append results."
  (reduce #'append (apply #'mapcar fn lists) :from-end t))
:ld C:\allegro-projects\ttlreader\basic-packages.lisp
(mappend #'(lambda (p) (ndtree:fetch `(,p ?s ,(ns:intern-iri "http://www.w3.org/1999/02/22-rdf-syntax-ns#Property"))))
         ndtree:*predicates*)
(nt:read-nquads-file "all-layers.nq" :quad-fn #'nt:add-triple)
(ndtree:fetch `(,(ns:intern-iri "http://www.w3.org/2000/01/rdf-schema#label") ?s ?o ?g))
(ndtree:fetch `(,(ns:intern-iri "http://www.w3.org/1999/02/22-rdf-syntax-ns#type") ?s ?o ?g))
|#
#|
(in-package :gx)
(setq *readtable* *rdf-readtable*)
(mapcar #'(lambda (trpl)
            (destructuring-bind (p s o) trpl
              (list (sw:node-iri s) (sw:node-iri p) (sw:node-iri o))))
  (car (ndtree:fetch `(,<http://rdf.freebase.com/ns/type.property.expected_type> ?s . ?o))))
(mapcar #'(lambda (trpl)
            (destructuring-bind (p s o) trpl
              (list (sw:node-iri s) (sw:node-iri p) o)))
  (car (ndtree:fetch `(,<http://www.w3.org/2000/01/rdf-schema#label> ?s . ?o))))
|#
#|
:cd \\LS220D95A\ontologies\jwo
(nt:read-ntriples-file "jwo_infoboxproperty_nt.nt" :triple-fn #'nt:add-triple)
(line:with-open-file (stream "jwo_infoboxproperty_nt.nt" :external-format :utf-8)
  (let (len (line (make-string 1 :initial-element #\Null)))
    (loop while (multiple-value-setq (line len) (nt::simple-stream-read-line stream nil nil line))
        do (format t "~%~D: ~A" (line:line-count stream) (subseq line 0 len)))))
|#
