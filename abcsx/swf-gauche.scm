#!/usr/bin/env gosh

;; Usage: swfmake [-w width] [-h height] [-o outfile] [-c classname] abcfiles ...
;;   if -o is ommited, last abcfile name is used for output.

;; Copyright (c) 2010 Takashi Yamamiya
;;
;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:
;;
;; The above copyright notice and this permission notice shall be included in
;; all copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
;; THE SOFTWARE.

(use srfi-1)
(use srfi-4)
(use binary.io)
(use file.util)

(define call-with-output-bytes
  (lambda (proc)
    (string->u8vector (call-with-output-string proc))))

;; Make a byte array from file
(define file->bytes
  (lambda (infile)
    (let ((size (file-size infile)))
      (call-with-input-file infile
        (lambda (input)
          (string->u8vector (read-block size input)))))))

;; Return a copy of the string witout extension.
(define trim-extension
  (lambda (string)
    (let ((reversed (drop-while
                     (lambda (c) (not (eq? c #\.)))
                     (reverse (string->list string)))))
      (list->string
       (reverse (if (pair? reversed) (cdr reversed) reversed))))))

(define bytes u8vector)
(define bytes-length u8vector-length)
(define write-bytes write-block)
(define arithmetic-shift ash)
(define bitwise-and logand)
(define bitwise-ior logior)
(define string->bytes string->u8vector)

(define int
  (lambda (n) (inexact->exact (truncate n))))

(set! *load-path* (cons (sys-dirname *program-name*) *load-path*))
(load "check.scm") ;; srfi-78 Lightweight testing
(load "swf.k")
(load "swf-test.k")

;;;; Main

(define (main args)
  (swf-run (cdr args))
  0)
