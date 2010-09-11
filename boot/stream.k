;; stream.k -- Stream library

;; Copyright (c) 2010 Ian Piumarta, Takashi Yamamiya
;; All Rights Reserved

;; Permission is hereby granted, free of charge, to any person obtaining a
;; copy of this software and associated documentation files (the 'Software'),
;; to deal in the Software without restriction, including without limitation
;; the rights to use, copy, modify, merge, publish, distribute, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, provided that the above copyright notice(s) and this
;; permission notice appear in all copies of the Software.  Inclusion of the
;; the above copyright notice(s) and this permission notice in supporting
;; documentation would be appreciated but is not required.

;; THE SOFTWARE IS PROVIDED 'AS IS'.  USE ENTIRELY AT YOUR OWN RISK.

(require "boot/std.k")

(define ftell		(extern "ftell"		'(-> long		(* (struct FILE)))))
(define fseek		(extern "fseek"		'(-> int 		(* (struct FILE)) int int)))
(define SEEK_SET	0)
(define fgetc		(extern "fgetc"		'(-> int 		(* (struct FILE)))))
(define ungetc		(extern "ungetc"	'(-> int 		int (* (struct FILE)))))
(define fputc		(extern "fputc"		'(-> int 		int (* (struct FILE)))))
(define fwrite		(extern "fwrite"	'(-> int 		(* void) int int (* (struct FILE)))))

(define stream (namespace global 'stream))

;(in stream

  (define-structure *end* ())
  (define *end* (new-*end*))

;;;; Methods provided by all streams

  (define-generic position (self ))
  (define-generic set-position (self pos))

;;;; Methods provided by read streams

  (define-generic peek (self))
  (define-generic next (self))
  (define-generic at-end? (self))

;;;; Methods provided by write streams

  (define-generic put (self xs))
  (define-generic put-all (self x))

;;;; Input byte stream

  (define-structure input-byte (buffer position))

  (define-method position typeof-input-byte
    (lambda (self) (input-byte-position self)))

  (define-method set-position typeof-input-byte
    (lambda (self n) (set-input-byte-position self n)))
    
  (define open-input-byte
    (lambda (bs)
      (let ((self (new-input-byte)))
	(set-input-byte-buffer self bs)
	(set-input-byte-position self 0)
	self)))

  (define-method peek typeof-input-byte
    (lambda (self)
      (if (< (position self) (bytes-length (input-byte-buffer self)))
          (bytes-ref (input-byte-buffer self) (position self))
          *end*)))

  (define-method next typeof-input-byte
    (lambda (self)
      (let ((c (peek self)))
        (if (!= c *end*)
            (set-position self (+ (position self) 1)))
        c)))

  (define-method at-end? typeof-input-byte
    (lambda (self) (== *end* (peek self))))

;;;; Output byte stream

  (define-structure output-byte (buffer position length))

  (define-method position typeof-output-byte
    (lambda (self) (output-byte-position self)))

  (define-method set-position typeof-output-byte
    (lambda (self n) (set-output-byte-position self n)))
    
  (define open-output-byte
    (lambda ()
      (let ((self (new-output-byte)))
	(set-output-byte-buffer self (make-bytes 0))
	(set-output-byte-position self 0)
	(set-output-byte-length self 0)
	self)))

  (define output-byte-grow
    (lambda (self size)
      (set-output-byte-length self (max size (output-byte-length self)))
      (let ((oldsize (bytes-length (output-byte-buffer self))))
	(if (< oldsize size)
	    (let ((newsize (max size (* oldsize 2))))
	      (let ((newbuf (make-bytes newsize)))
		(bytes-copy! newbuf 0 (output-byte-buffer self))
		(set-output-byte-buffer self newbuf)))))))
  
  ;; todo: PLT's definition uses entire bytes instead of up to the position.
  (define get-output-bytes
    (lambda (self)
      (subbytes (output-byte-buffer self) 0 (position self))))

  (define-method put typeof-output-byte
    (lambda (self byte)
      (let ((pos (position self)))
        (output-byte-grow self (+ pos 1))
        (bytes-set! (output-byte-buffer self) pos byte)
        (set-position self (+ pos 1)))))

  (define-method put-all typeof-output-byte
    (lambda (self bs)
      (let ((pos (position self)))
        (let ((new-pos (+ pos (bytes-length bs))))
          (output-byte-grow self new-pos)
          (bytes-copy! (output-byte-buffer self) pos bs)
          (set-position self new-pos)))))
    
;;;; output and input file stream

  (define-structure file-stream (fp))

  (define file-open
    (lambda (path mode)
      (let ((fp (fopen path mode)))
	(if (= 0 (convert 'long fp)) (error path ": " (strerror (_peek errno))))
	fp)))

  (define file-stream-with-file
    (lambda (fp)
      (let ((s (new-file-stream)))
	(set! (file-stream-fp s) fp)
	s)))

  (define file-stream-open
    (lambda (path mode)
      (file-stream-with-file (file-open path mode))))

  (define file-stream-close
    (lambda (self) (fclose (file-stream-fp self))))

  (define-method position typeof-file-stream
    (lambda (self) (ftell (file-stream-fp self))))

  (define-method set-position typeof-file-stream
    (lambda (self n) (fseek (file-stream-fp self) n SEEK_SET)))

  (define-method peek typeof-file-stream
    (lambda (self)
      (let ((c (ungetc (fgetc (file-stream-fp self)) (file-stream-fp self))))
        (if (= -1 c) *end* c))))

  (define-method next typeof-file-stream
    (lambda (self)
      (let ((c (fgetc (file-stream-fp self))))
        (if (= -1 c) *end* c))))

  (define-method at-end? typeof-file-stream
    (lambda (self)
      (let ((c (ungetc (fgetc (file-stream-fp self)) (file-stream-fp self))))
        (= -1 c))))

  (define-method put typeof-file-stream
    (lambda (self byte) (fputc byte (file-stream-fp self))))

  (define-method put-all typeof-file-stream
    (lambda (self bs)
      (fwrite (addrof (bytes-bits bs)) 1 (bytes-size bs) (file-stream-fp self))))

;)

;;;; PLT compatible functions

(define open-output-bytes open-output-byte)
(define open-input-bytes open-input-byte)

(define byte-port-put put)

(define byte-port-put-all put-all)

(define get-output-bytes get-output-bytes)

(define get-output-string
  (lambda (self)
    (bytes->string (get-output-bytes self))))

(define write-byte (lambda (b p) (put p b)))
(define write-bytes (lambda (bs p) (put-all p bs)))

(define read-byte next)

(define read-bytes 
  (lambda (size s)
    (let ((bs (make-bytes size))
	  (i 0))
      (while (< i size)
	     (if (at-end? s) (error "Stream is too short"))
	     (bytes-set! bs i (read-byte s))
	     (set! i (+ i 1)))
      bs)))

(define file-position
  (lambda (self . option)
    (if option
	(set-position self (car option)))
    (position self)))

;(define port? (lambda (obj) (= (typeof obj) typeof-output-byte))) ;; todo

(define write-string
  (lambda (str self)
    (or (string? str) (error "String is expected, but:" str))
;    (or (port? self) (error "String-stream is expected, but:" self))
    (write-bytes (string->bytes str) self)))

(define call-with-output-bytes
  (lambda (proc)
    (let ((out (open-output-bytes)))
      (proc out)
      (get-output-bytes out))))

(define call-with-file
  (lambda (path proc mode)
    (let ((s (file-stream-open path mode)))
      (let ((result (proc s)))
        (file-stream-close s)
        result))))

(define eof-object? ;; [r6rs-lib]
  (lambda (c) (= c *end*)))

(define call-with-output-file ;; [r6rs-lib]
  (lambda (path proc) (call-with-file path proc "w")))

(define call-with-input-file ;; [r6rs-lib]
  (lambda (path proc) (call-with-file path proc "r")))

(define up-to-end
  (lambda (input)
    (call-with-output-bytes
     (lambda (output)
       (while (not (at-end? input))
              (put output (next input)))))))

;;;; String port (derivative from bytes-port)

(define open-output-string open-output-bytes)

(define call-with-output-string
  (lambda (func)
    (let ((port (open-output-string)))
      (func port)
      (get-output-string port))))

;;;; String Utilities using port

;; Returns a newly allocated string whose characters form the
;; concatenation of the given strings.

(define string-append ;; [r6rs]
  (lambda strings
    (call-with-output-string
     (lambda (out)
       (map (lambda (str) (write-string str out)) strings)))))
