#!/usr/bin/env mzscheme

;; Copyright (c) 2009 Takashi Yamamiya
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

#lang scheme
(require srfi/78) ; Lightweight testing
(require srfi/1) ; List library

(define bytes->string bytes->string/utf-8)
(define string->bytes string->bytes/utf-8)

;; Enumeration with two lists

(define fold2 fold)
(define map2 map)
(define for-each2 for-each)

;; Read a file

(define (read-file infile)
  (call-with-input-file infile
    (lambda (port) (read port))))

;; Write a file

(define (write-file asm outfile)
  (call-with-output-file outfile
    (lambda (port)
      (write-asm asm port))
    #:exists 'replace))

(define-syntax while
  (syntax-rules ()
    ((while test body ...)
     (let loop ()
       (if test
           (begin body ... (loop))
           '())))))

(include "instruction.k")
(include "abc.k")
(include "test.scm")

(define (test _ __)
  (check-set-mode! 'report-failed)
  (run-test))

(run (vector->list (current-command-line-arguments)))
