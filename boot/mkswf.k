(require "boot/stream.k")

(define log
  (lambda (n)
    ((extern "log" '(-> double double))
     (convert 'double n))))

(define arithmetic-shift
  (lambda (n m)
    (if (> m 0)
	(<< n m)
	(>> n (- 0 m)))))

(define modulo %)

(define file->bytes
  (lambda (infile)
    (call-with-input-file infile
      (lambda (input)
        (call-with-output-bytes
         (lambda (output)
           (let ((data (up-to-end input)))
           (write-bytes data output))))))))

;;;; Alias
(define bitwise-and &) ;; [r6rs]
(define bitwise-ior |) ;; [r6rs]
(define else 't)
(define eq? =)
(define number? long?)
(define memq memq?)
(define display println)

(define string-do
  (lambda (string function)
    (let ((ptr (addrof string))
	  (c))
      (while (!= 0 (set! c (_peek ptr)))
	(function c)
	(set! ptr (+ ptr 1))))))

(define int
  (lambda (x) (convert 'int x)))

(require "abcsx/swf.k")
(require "abcsx/swf-test.k")
(and (pair? *arguments*) (= (car *arguments*) "-test") (exit 0))

(swf-run *arguments*)
(set! *arguments* '())
