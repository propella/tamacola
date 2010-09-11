(require "boot/writer.k")

(check (number->string 42) => "42")
(check (number->string 42 10) => "42")
(check (number->string 42 16) => "2a")
(check (number->string 42 8) => "52")
(check (number->string 42 2) => "101010")
(check (number->string 0xffff 16) => "ffff")
(check (number->string 0x7fffffff 16) => "7fffffff")
;(check (number->string 0xffffffff 16) => "-1")

(check (number->string 3.14) => "3.14")

;; Simple writer

(check (call-with-output-string
	(lambda (out) (write-address 0xffff out))) => "0xffff")
; (check (call-with-output-string
;	(lambda (out) (write-address 0xdeadbeaf out))) => "0xdeadbeaf")

(check (call-with-output-string
	(lambda (out) (write 'hello out))) => "hello")

(check (call-with-output-string
	(lambda (out) (write 0 out))) => "0")
(check (call-with-output-string
	(lambda (out) (write 1 out))) => "1")
(check (call-with-output-string
	(lambda (out) (write 1234 out))) => "1234")
(check (call-with-output-string
	(lambda (out) (write -1234 out))) => "-1234")

(check (call-with-output-string
	(lambda (out) (write-string-quoted "hello" out))) => "\"hello\"")
(check (call-with-output-string
	(lambda (out) (write-string-quoted "\"hello\"" out))) => "\"\\\"hello\\\"\"")
(check (call-with-output-string
	(lambda (out) (write-string-quoted "\n" out))) => "\"\\n\"")
(check (call-with-output-string
	(lambda (out) (write-string-quoted "\t" out))) => "\"\\t\"")

(check (call-with-output-string
	(lambda (out) (write-string-quoted-hex 15 out))) => "\\x0f")
(check (call-with-output-string
	(lambda (out) (write-string-quoted-hex 16 out))) => "\\x10")
(check (call-with-output-string
	(lambda (out) (write-string-quoted-hex 64 out))) => "\\x40")

(check (call-with-output-string
	(lambda (out) (write 3.14 out))) => "3.14")
(check (call-with-output-string
	(lambda (out) (write '(hello) out))) => "(hello)")
(check (call-with-output-string
	(lambda (out) (write '(hello world) out))) => "(hello world)")
(check (call-with-output-string
	(lambda (out) (write '(1 1.1 "hello" world) out))) => "(1 1.1 \"hello\" world)")
(check (call-with-output-string
	(lambda (out) (write '(hello . world) out))) => "(hello . world)")
(check (call-with-output-string
	(lambda (out) (write '(hello (beautiful "world")) out)))
       => "(hello (beautiful \"world\"))")

;; Array

(check (call-with-output-string
	(lambda (out) (write [1 2 3] out)))
       => "[1 2 3]")

;; Structure

(define-structure triplet (fst snd thd))
(define triplet0
  (let ((t (new-triplet)))
    (set-triplet-fst t 'hello)
    (set-triplet-snd t "world")
    (set-triplet-thd t 47)
    t))
(check (call-with-output-string
	(lambda (out) (write triplet0 out)))
       => "{triplet: fst=hello,snd=\"world\",thd=47}")
