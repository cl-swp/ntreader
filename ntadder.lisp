;;;-*- Mode: common-lisp; syntax: common-lisp; package: nc; base: 10 -*-
;;;
;;; Add N-Triple Module
;;;
;;; ----------------------------------------------------------------------------------
;;; Copyright (c) 2016, Seiji Koide <koide@ontolonomy.co.jp>
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
;; 2016/01/01    File created.

(cl:provide :ntadder)

(eval-when (:execute :load-toplevel :compile-toplevel)
  (require :ntparser)
  #+:ndtree  (require :ndtree)
) ; end of eval-when

(defpackage :nt
  (:use :common-lisp)
  (:export #:add-triple #:add-subject #:add-predicate #:add-object #:add-graph
           #:bnode-p)
  )

(in-package :nt)

(defun bnode-p (x)
  (etypecase x
    (string (and (char= (char x 0) #\_)
                 (char= (char x 1) #\:)))
    (symbol (find-symbol (string x) :_))))

(defun prefix-part-of (x)
  (etypecase x
    (string (let ((pos (position #\: x :test #'char=)))
              (values (subseq x 0 pos) (subseq x (1+ pos)))))
    (symbol (values (package-name (symbol-package x)) (symbol-name x)))))

;;;
;;; Adder
;;;
#-:ndtree
(defun add-triple (line subject-start subject-end predicate-start predicate-end object-start object-end
                        &optional (graph-start nil) (graph-end (length line)))
  (let ((subject (add-subject line subject-start subject-end))
        (predicate (add-predicate line predicate-start predicate-end))
        (object  (add-object line object-start object-end))
        (graph (when graph-start (add-graph line graph-start graph-end))))
    (cond ((null graph)
           (km::add-clause (list (list predicate subject object))))
          (t (km::add-clause (list (list predicate subject object graph)))))
    (values)))
#+:ndtree
(defun add-triple (line subject-start subject-end predicate-start predicate-end object-start object-end
                        &optional (graph-start nil) (graph-end (length line)))
  (let ((subject (add-subject line subject-start subject-end))
        (predicate (add-predicate line predicate-start predicate-end))
        (object  (add-object line object-start object-end))
        (graph (when graph-start (add-graph line graph-start graph-end))))
    (cond ((null graph)
           (ndtree:index (list predicate subject object)))
          (t (ndtree:index (list predicate subject object graph))))
    (values)))

(defun add-subject (line start end)
  (cond ((char= (char line start) #\<)
         (let ((iri (ns:intern-iri line (1+ start) (1- end))))
           iri))
        ((and (char= (char line start) #\_)
              (char= (char line (1+ start)) #\:))
         (let ((bname (subseq line start end)))
           bname))
        ((error "Cant happen!"))))

(defun add-predicate (line start end)
  (cond ((char= (char line start) #\<)
         (let ((iri (ns:intern-iri line (1+ start) (1- end))))
           iri))
        ((error "Cant happen!"))))

(defun add-object (line start end)
  (cond ((char= (char line start) #\<)
         (let ((iri (ns:intern-iri line (1+ start) (1- end))))
           iri))
        ((and (char= (char line start) #\_)
              (char= (char line (1+ start)) #\:))
         (let ((bname (subseq line start end)))
           bname))
        ((char= (char line start) #\")
         (subseq line start end))
        ((error "Cant happen!"))))

(defun add-graph (line start end)
  (cond ((char= (char line start) #\<)
         (ns:intern-iri line (1+ start) (1- end)))
        ((error "Cant happen!"))))
