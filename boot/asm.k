;; Abcsx loader and Scheme compatibility layer

(require "boot/std.k")
(require "boot/pretty-print.k")

;;;; Trivial function

(define add1 (lambda (n) (+ n 1)))

(define arithmetic-shift
  (lambda (n m)
    (if (> m 0)
	(<< n m)
	(>> n (- 0 m)))))

;;;; Alias
(define bitwise-and &) ;; [r6rs]
(define bitwise-ior |) ;; [r6rs]
(define else 't)
(define eq? =)
(define memq memq?)
(define display println)

;;;; File Utilities

;; Delete the named file (r6rs).
;; (define delete-file (extern "unlink" '(function int (* char))))

(define format println)

(define real->floating-point-bytes ;; size and big-endian? are ignored
  (lambda (val size-n big-endian?) (double->bytes val)))

(define floating-point-bytes->real ;; size and big-endian? are ignored
  (lambda (bstr big-endian?) (bytes->double bstr)))

(define real->floating-point-bytes double->bytes)

;; forward definition
(define decode-id) ;; forward

;; Dummy definition used in the driver (see abc-asm-run.k)
(define write-file) ;; forward
(define read-file) ;; forward
(define test) ;; forward

(require "abcsx/instruction.k")
(require "abcsx/abc.k")

