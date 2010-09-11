#!/usr/bin/env gosh

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

(use srfi-1)
(use srfi-4)
(use util.list)
(use gauche.vport)
(use binary.io)
(set! *load-path* (cons "." *load-path*))
(load "check.scm") ;; srfi-78 Lightweight testing

;; Enumeration with two lists

(define fold2 fold)
(define map2 map)
(define for-each2 for-each)

;; Arithmetic

(define bitwise-and logand)
(define bitwise-ior logior)
(define arithmetic-shift ash)
(define (add1 n) (+ n 1))

;; Hash

(define make-immutable-hash alist->hash-table)
(define hash-ref hash-table-get) ;; srfi hash-table-ref
(define hash-set! hash-table-put!)
(define make-hasheq
  (lambda () (make-hash-table 'eq?)))

;; Byte Array

(define bytes u8vector)
(define open-input-bytes open-input-uvector)

(define call-with-output-bytes
  (lambda (proc)
    (string->u8vector (call-with-output-string proc))))

(define bytes->string u8vector->string)
(define string->bytes string->u8vector)

(define bytes-length u8vector-length)

(define write-bytes write-block)
(define read-bytes 
  (lambda (amt in)
    (let ((vec (make-u8vector amt 0)))
      (read-block! vec in)
      vec)))

(define real->floating-point-bytes ;; size and big-endian? are ignored
  (lambda (val size-n big-endian?)
    (let ((bstr (make-u8vector 8 0)))
      (put-f64le! bstr 0 val)
      bstr)))

(define floating-point-bytes->real ;; size and big-endian? are ignored
  (lambda (bstr big-endian?)
    (get-f64le bstr 0)))

(define file-position
  (lambda (port)
    (port-tell port)))

;; List

(define build-list
  (lambda (n proc)
    (map proc (iota n))))

;; Pretty Printer

(define (pretty-print-sexp s)
  (define (do-indent level)
    (dotimes (_ level) (write-char #\space)))
  (define (pp-parenl)
    (write-char #\())
  (define (pp-parenr)
    (write-char #\)))
  (define (pp-atom e prefix)
    (when prefix (write-char #\space))
    (write e))
  (define (pp-list s level prefix)
    (and prefix (do-indent level))
    (pp-parenl)
    (let loop ((s s)
               (prefix #f))
      (if (null? s)
          (pp-parenr)
          (let1 e (car s)
            (if (list? e)
                (begin (and prefix (newline))
                       (pp-list e (+ level 1) prefix))
                (pp-atom e prefix))
            (loop (cdr s) #t)))))
  (if (list? s)
      (pp-list s 0 #f)
      (write s))
  (newline))

(define pretty-print pretty-print-sexp)
(define (check-set-mode! flag) '())

(load "instruction.k")
(load "abc.k")

;;; read a S-expression file
(define (read-file infile)
  (call-with-input-file infile
    (lambda (port) (read port))))

;;; write a ABC file
(define (write-file asm outfile)
  (call-with-output-file outfile
    (lambda (port)
      (write-asm asm port))))

(define (test _ _)
  (load "test.scm")
  (run-test))

(define (main args)
  (run (cdr args))
  0)
