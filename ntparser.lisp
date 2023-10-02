;;;-*- Mode: common-lisp; syntax: common-lisp; package: nt; base: 10 -*-
;;;
;;;; N-Triples Parser Module
;;;
;;; See, http://www.w3.org/TR/2014/REC-n-triples-20140225/ for N-Triples
;;;      http://www.w3.org/TR/2014/REC-n-quads-20140225/ for N-Quads
;;;
;;; ----------------------------------------------------------------------------------
;;; Copyright (c) 2015-2016 Seiji Koide <koide@ontolonomy.co.jp>
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

;; 2014/06/25    File created

(cl:provide :ntparser)

(defpackage :nt 
  (:use :common-lisp)
  (:export #:skipbl #:parse-triple #:blank-node-label-p
           #:uchar-p #:echar-p #:pnCharsBase-p #:hex-p #:pnChars-p #:pnCharsU-p)
  )

(in-package :nt)

;;;
;;;
;;;

(defun ws-p (char)
  (declare (optimize (speed 3) (safety 0)))
  (declare (type character char))
  (let ((code (char-code char)))
    (declare (type fixnum code))
    (or (= code #x20) (= code #x9))))

(defun eoln-p (char)
  (declare (optimize (speed 3) (safety 0)))
  (declare (type character char))
  ;; [7]	EOL	::=	[#xD#xA]+
  (or (char= char #\Newline)
      (char= char (code-char #xD))
      (char= char (code-char #xA))))

(defun hex-p (char)
  (declare (optimize (speed 3) (safety 0)))
  (declare (type character char))
  ;; [162s]	HEX	::=	[0-9] | [A-F] | [a-f]
  (or (char<= #\0 char #\9) (char<= #\A char #\F) (char<= #\a char #\f)))

(defun pnCharsBase-p (char)
  ;(declare (optimize (speed 3) (safety 0)))
  (declare (type character char))
  ;; [157s]	PN_CHARS_BASE	::=	[A-Z] | [a-z] | [#x00C0-#x00D6] | [#x00D8-#x00F6] | [#x00F8-#x02FF] | [#x0370-#x037D] | [#x037F-#x1FFF]
  ;;                                  | [#x200C-#x200D] | [#x2070-#x218F] | [#x2C00-#x2FEF] | [#x3001-#xD7FF] | [#xF900-#xFDCF] | [#xFDF0-#xFFFD]
  ;;                                  | [#x10000-#xEFFFF]
  (or (char<= #\A char #\Z)
      (char<= #\a char #\z)
      (let ((code (char-code char)))
        (declare (type fixnum code))
        (or (<= #x00C0 code #x00D6)
            (<= #x00D8 code #x00F6)
            (<= #x00F8 code #x02FF)
            (<= #x0370 code #x037D)
            (<= #x037F code #x1FFF)
            (<= #x200C code #x200D)
            (<= #x2070 code #x218F)
            (<= #x2C00 code #x2FEF)
            (<= #x3001 code #xD7FF)
            (<= #xF900 code #xFDCF)
            (<= #xFDF0 code #xFFFD)
            (<= #x10000 code #xEFFFF)))))

(defun pnCharsU-p (char)
  ;(declare (optimize (speed 3) (safety 0)))
  (declare (type character char))
  ;; [158s]	PN_CHARS_U	::=	PN_CHARS_BASE | '_' | ':'
  (or (pnCharsBase-p char)
      (char= char #\_)
      (char= char #\:)))

(defun pnChars-p (char)
  ;(declare (optimize (speed 3) (safety 0)))
  (declare (type character char))
  ;; [160s]	PN_CHARS	::=	PN_CHARS_U | '-' | [0-9] | #x00B7 | [#x0300-#x036F] | [#x203F-#x2040]
  (or (pnCharsU-p char)
      (char= char #\-)
      (char<= #\0 char #\9)
      (let ((code (char-code char)))
        (declare (type fixnum code))
        (or (= code #x00B7)
            (<= #x0300 code #x036F)
            (<= #x203F code #x2040)))))

;;;
;;;
;;;

(defun echar-p (str pos &optional (len (length str)))
  "returns true and the next position of the last char of ECHAR, otherwise 
   returns false and the value of <pos>."
  (declare (optimize (speed 3) (safety 0)))
  (declare (type fixnum pos len))
  ;; [153s]	ECHAR	::=	'\' [tbnrf"'\]
  (if (and (<= (+ pos 2) len)
           (char= (char str pos) #\\)
           (find (char str (1+ pos)) "tbnrf\"'\\" :test #'char=))
      (values t (+ pos 1)) ; next pos
    (values nil pos)))

(defun uchar-p (str pos &optional (len (length str)))
  "returns true and the position of the last char of UCHAR, otherwise 
   returns false and the value of <pos>."
  (declare (optimize (speed 3) (safety 0)))
  (declare (type fixnum pos len))
  ;; [10]	UCHAR	::=	'\u' HEX HEX HEX HEX | '\U' HEX HEX HEX HEX HEX HEX HEX HEX
  (unless (char= (char str pos) #\\) (return-from uchar-p (values nil pos)))
  (incf pos)
  (cond ((char= (char str pos) #\u)
         (incf pos)
         (if (and (<= (+ pos 4) len)
                  (hex-p (char str pos))
                  (hex-p (char str (1+ pos)))
                  (hex-p (char str (+ pos 2)))
                  (hex-p (char str (+ pos 3))))
             (values t (+ pos 3))
           (values nil pos)))
        ((char= (char str pos) #\U)
         (incf pos)
         (if (and (<= (+ pos 8) len)
                  (hex-p (char str pos))
                  (hex-p (char str (1+ pos)))
                  (hex-p (char str (+ pos 2)))
                  (hex-p (char str (+ pos 3)))
                  (hex-p (char str (+ pos 4)))
                  (hex-p (char str (+ pos 5)))
                  (hex-p (char str (+ pos 6)))
                  (hex-p (char str (+ pos 7))))
             (values t (+ pos 7))
           (values nil pos)))
        (t (values nil pos))))

(defun iriref-p (str pos &optional (len (length str)))
  "returns true and the next position of closing angle bracket, otherwise 
   returns false and the value of <pos>."
  (declare (optimize (speed 3) (safety 0)))
  (declare (type fixnum pos len))
  (declare (type string str))
  ;; [8]	IRIREF	::=	'<' ([^#x00-#x20<>"{}|^`\] | UCHAR)* '>'
  (let ((next pos))
    (declare (type fixnum next))
    (if (and (<= (+ next 2) len)
             (char= (char str next) #\<)
             (loop while (< (incf next) len)
                   for c = (char str next)
                 until (char= c #\>)
                 always (or (not (or (<= #x00 (char-code c) #x20)
                                     (find c "<>\"{}|^`\\" :test #'char=)))
                            (multiple-value-call #'(lambda (yes p) (when yes (setq next p)))
                              (uchar-p str next len)))))
        (if (< next len) (values t (1+ next)) (values nil pos))
      (values nil pos))))

#|
(iriref-p "<>" 0)
(iriref-p "<http://imi.ipa.go.jp/ns/core/201/#ID>" 0)
(iriref-p "<http://imi.ipa.go.jp/ns/core/201/（括弧）>" 0)
(iriref-p "<http://imi.ipa.go.jp/ns/core/201/(a%20brace)>" 0)
|#

(defun string-literal-quote-p (str pos &optional (len (length str)))
  "returns true and the next position of closing double-qoute, otherwise 
   returns false and the value of <pos>."
  (declare (optimize (speed 3) (safety 0)))
  (declare (type fixnum pos len))
  ;; [9]	STRING_LITERAL_QUOTE	::=	'"' ([^#x22#x5C#xA#xD] | ECHAR | UCHAR)* '"'
  ;; #x22=", #x5C=\\
  (let ((next pos))
    (if (and (<= (+ next 2) len)
             (char= (char str next) #\")
             (loop while (< (incf next) len)
                 for c = (char str next)
                 until (char= c #\")
                 do (when (or (= (char-code c) #xA) (= (char-code c) #xD))
                      (return-from string-literal-quote-p (values nil pos)))
                 always (or (not (find c "\"\\" :test #'char=))
                            (multiple-value-call #'(lambda (yes p) (when yes (setq next p)))
                              (echar-p str next))
                            (multiple-value-call #'(lambda (yes p) (when yes (setq next p)))
                              (uchar-p str next)))))
        (if (< next len) (values t (1+ next)) (values nil pos))
      (values nil pos))))

#|
(in-package :nt)
(string-literal-quote-p "\"This is a literal.\"" 0)
(string-literal-quote-p "\"\\\t escaped tab\"" 0)
(string-literal-quote-p "\"This is a hex \\uABCD.\"" 0)
(string-literal-quote-p "\"In the book Microcomputers and Education is written that: \\\"The dream of universal access to all knowledge is as powerful a dream as that of universal literacy and universal education.\\\"\"" 0)
|#

(defun langtag-p (str pos &optional (len (length str)))
  "returns true and the next position of the last char of LANGTAG, otherwise
   returns false and the value of <pos>."
  (declare (optimize (speed 3) (safety 0)))
  (declare (type fixnum pos len))
  ;; [144s]	LANGTAG	::=	'@' [a-zA-Z]+ ('-' [a-zA-Z0-9]+)*
  (let ((next pos))
    (labels ((%langtag-p (test-char test-fun)
                         (when (and (< (1+ next) len)
                                    (char= (char str next) test-char)
                                    (funcall test-fun (char str (incf next))))
                           ;; eat up the same kind characters
                           (loop while (and (< (incf next) len) (funcall test-fun (char str next))))
                           t)))
      (cond ((%langtag-p #\@ #'(lambda (c) (or (char<= #\a c #\z) (char<= #\A c #\Z))))
             (loop while (%langtag-p #\- #'(lambda (c) (or (char<= #\a c #\z) (char<= #\A c #\Z) (char<= #\0 c #\9)))))
             (values t next))
            (t (values nil pos))))))

(defun blank-node-label-p (str pos &optional (len (length str)))
  (declare (optimize (speed 3) (safety 0)))
  (declare (type fixnum pos len))
  ;; [141s]	BLANK_NODE_LABEL	::=	'_:' (PN_CHARS_U | [0-9]) ((PN_CHARS | '.')* PN_CHARS)?
  (unless (<= (+ pos 3) len)
    (return-from blank-node-label-p (values nil pos)))
  (let ((1stChar (char str pos))
        (2ndChar (char str (+ pos 1)))
        (3rdChar (char str (+ pos 2))))
    (cond ((and (char= 1stChar #\_)
                (char= 2ndChar #\:)
                (or (pnCharsU-p 3rdChar) (char<= #\0 3rdChar #\9)))
           (let ((next (+ pos 3)))
             (loop for c = (and (< (incf next) len) (char str next))
                 while (and c (or (pnChars-p c) (char= c #\.))))
             (if (char= (char str (1- next)) #\.)  ; ended with period
                 (values nil pos)                  ; then not blank-node
               (values t next))))
          (t (values nil pos)))))
#|
(in-package :nt)
(blank-node-label-p "_:x001" 0)
(blank-node-label-p "_:0001" 0)
(blank-node-label-p "_:_001" 0)
(blank-node-label-p "_:abcd" 0)
(blank-node-label-p "_:1234" 0)
(blank-node-label-p "_:1234.567" 0)
(blank-node-label-p "_:小出" 0)
(blank-node-label-p "_:小出.誠二" 0)
(blank-node-label-p "_:小出_誠二" 0)
(blank-node-label-p "_:小出-誠二" 0)
(blank-node-label-p "_:小出誠二." 0)  ; NG
|#
;;;
;;;
;;;

(defun literal-p (str pos &optional (len (length str)))
  "returns true and the next position of literal, otherwise 
   returns false and the value of pos."
  (declare (optimize (speed 3) (safety 0)))
  (declare (type fixnum pos len))
  ;; [6]	literal	::=	STRING_LITERAL_QUOTE ('^^' IRIREF | LANGTAG)?
  (multiple-value-bind (yes p) (string-literal-quote-p str pos len)
    (if yes (if (and (char= (char str p) #\^) (char= (char str (+ p 1)) #\^))
                (multiple-value-bind (yes pp) (iriref-p str (+ p 2) len)
                  (if yes (values t pp) (error "Illegal IRIREF")))
              (multiple-value-bind (yes pp) (langtag-p str p len)
                (if yes (values t pp)
                  (values t p))))
      (values nil pos))))

(defun subject-p (str pos &optional (len (length str)))
  (declare (optimize (speed 3) (safety 0)))
  (declare (type fixnum pos len))
  ;; [3]	subject	::=	IRIREF | BLANK_NODE_LABEL
  (let (subject? next)
    (if (multiple-value-setq (subject? next) (iriref-p str pos len))
        (values subject? next)
      (blank-node-label-p str pos len))))

(defun predicate-p (str pos &optional (len (length str)))
  (declare (optimize (speed 3) (safety 0)))
  (declare (type fixnum pos len))
  ;; [4]	predicate	::=	IRIREF
  (iriref-p str pos len))

(defun object-p (str pos &optional (len (length str)))
  (declare (optimize (speed 3) (safety 0)))
  (declare (type fixnum pos len))
  ;; [5]	object	::=	IRIREF | BLANK_NODE_LABEL | literal
  (let (object? next)
    (if (multiple-value-setq (object? next) (iriref-p str pos len))
        (values object? next)
      (if (multiple-value-setq (object? next) (blank-node-label-p str pos len))
          (values object? next)
        (literal-p str pos len)))))

(defun graphlabel-p (str pos &optional (len (length str)))
  (declare (optimize (speed 3) (safety 0)))
  (declare (type fixnum pos len))
  ;; [6]	graphLabel	::=	IRIREF | BLANK_NODE_LABEL
  (let (graph? next)
    (if (multiple-value-setq (graph? next) (iriref-p str pos len))
        (values graph? next)
      (blank-node-label-p str pos len))))

(defun skipbl (str pos &optional (len (length str)))
  (declare (optimize (speed 3) (safety 0)))
  (declare (type fixnum pos len))
  (loop while (and (< pos len) (ws-p (char str pos))) do (incf pos))
  pos)

(defun null-line-p (line pos &optional (len (length line)))
  (declare (optimize (speed 3) (safety 0)))
  (declare (type fixnum pos len))
  (>= pos len))

(defun period-p (line pos)
  (declare (optimize (speed 3) (safety 0)))
  (declare (type fixnum pos))
  (values (char= (char line pos) #\.) pos))

(defun parse-triple (str len pos)
  "returns start and end positions of subject, predicate, object and optionally graph-label."
  (declare (optimize (speed 3) (safety 0)))
  (declare (type fixnum pos))
  (let (subject? predicate? object? graphlabel?
                 subject-start predicate-start object-start graph-start
                 subject-end predicate-end object-end graph-end)
    (setq subject-start (skipbl str pos len))
    (when (null-line-p str subject-start len) (return-from parse-triple nil))
    ;; subject
    (multiple-value-setq (subject? subject-end) (subject-p str subject-start len))
    (unless subject? (error "Subject missing: ~S" (subseq str 0 len)))
    ;; predicate
    (multiple-value-setq (predicate? predicate-end) (predicate-p str (setq predicate-start (skipbl str subject-end)) len))
    (unless predicate? (error "Predicate missing: ~S" (subseq str 0 len)))
    ;; object
    (multiple-value-setq (object? object-end) (object-p str (setq object-start (skipbl str predicate-end)) len))
    (unless object? (error "Object missing: ~S" (subseq str 0 len)))
    ;; optional graph
    (cond ((period-p str (setq graph-start (skipbl str object-end)))
           (values subject-start subject-end predicate-start predicate-end object-start object-end))
          ((multiple-value-setq (graphlabel? graph-end) (graphlabel-p str graph-start len))
           (if graphlabel? 
               (if (period-p str (skipbl str graph-end))
                   (values subject-start subject-end predicate-start predicate-end object-start object-end graph-start graph-end)
                 (warn "Period missing: ~S" str))
             (if (null-line-p str graph-start len)
                 (warn "Period missing: ~S" str)
               (error "Graph label missing: ~S" str)))))))

(defun parse-nquad (str len pos)
  "returns start and end positions of subject, predicate, object and optionally graph-label."
  (declare (optimize (speed 3) (safety 0)))
  (declare (type fixnum pos))
  (let (subject? predicate? object? graphlabel?
                 subject-start predicate-start object-start graph-start
                 subject-end predicate-end object-end graph-end)
    (setq subject-start (skipbl str pos len))
    (when (null-line-p str subject-start len) (return-from parse-nquad nil))
    ;; subject
    (multiple-value-setq (subject? subject-end) (subject-p str subject-start len))
    (unless subject? (error "Subject missing: ~S" (subseq str 0 len)))
    ;; predicate
    (multiple-value-setq (predicate? predicate-end) (predicate-p str (setq predicate-start (skipbl str subject-end)) len))
    (unless predicate? (error "Predicate missing: ~S" (subseq str 0 len)))
    ;; object
    (multiple-value-setq (object? object-end) (object-p str (setq object-start (skipbl str predicate-end)) len))
    (unless object? (error "Object missing: ~S" (subseq str 0 len)))
    ;; optional graph
    (cond ((period-p str (setq graph-start (skipbl str object-end)))
           (values subject-start subject-end predicate-start predicate-end object-start object-end))
          ((multiple-value-setq (graphlabel? graph-end) (graphlabel-p str graph-start len))
           (if graphlabel? 
               (if (period-p str (skipbl str graph-end))
                   (values subject-start subject-end predicate-start predicate-end object-start object-end graph-start graph-end)
                 (warn "Period missing: ~S" str))
             (if (null-line-p str graph-start len)
                 (warn "Period missing: ~S" str)
               (error "Graph label missing: ~S" str)))))))

;; End of module
;; --------------------------------------------------------------------
