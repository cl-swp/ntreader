;;;-*- Mode: common-lisp; syntax: common-lisp; package: iri; base: 10 -*-
;;;
;;; Angle Bracket Reader 
;;;
;;; ----------------------------------------------------------------------------
;;; Copyright (c) 2014-2016 Seiji Koide <koide@ontolonomy.co.jp>
;;; Permission is hereby granted, free of charge, to any person obtaining a 
;;; copy of this software and associated documentation files (the "Software"), 
;;; to deal in the Software without restriction, including without limitation 
;;; the rights to use, copy, modify, merge, publish, distribute, sublicense, 
;;; and/or sell copies of the Software, and to permit persons to whom the 
;;; Software is furnished to do so, subject to the following conditions:
;;;
;;; The above copyright notice and this permission notice shall be included 
;;; in all copies or substantial portions of the Software.
;;;
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS 
;;; OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, 
;;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL 
;;; THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER 
;;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING 
;;; FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER 
;;; DEALINGS IN THE SOFTWARE.
;;; ----------------------------------------------------------------------------

(cl:provide :iri-reader)

(in-package :iri)

;;; ==================================================================================
;;;
;;;; Reader Macro '<' for IRI
;;;

(eval-when (:execute :load-toplevel)
  
(defun angle-bracket-reader (stream char)
  "This is used to read an IRI that is enveloped by opening and closing angle bracket.
   This is not intended to be used by users, instead it is used by lisp reader."
  (assert (char= char #\<))
  (let ((nc (peek-char cl:nil stream t cl:nil t)))
    (cond ((char= nc #\<)          ; double #\<
           (error "Too many angle brackets for IRI~%~D: ~S"
             (line:line-count stream) (line:expose-one-line-buf stream)))
          ((char= nc #\<)
           (error "Double angle bracket reader, Not Yet!"))
          ((char= nc #\h) ; maybe <http 
           (ns:intern-iri 
            (coerce
             (loop with char until (char= #\> (setq char (read-char stream))) collect char)
             'cl:string)))
          (t (excl::read-token stream char)))))

(defun trim-angle (str)
  (cond ((and (char= (char str 0) #\<)
              (char= (char str (1- (length str))) #\>))
         (subseq str 1 (1- (length str))))))

) ;end-of-eval-when
