;; Simple writer

(check (call-with-output-string
	(lambda (out) (write-on '() out))) => "()")

(check (call-with-output-string
	(lambda (out) (write-on #t out))) => "#t")

(check (call-with-output-string
	(lambda (out) (write-on #f out))) => "#f")

(check (call-with-output-string
	(lambda (out) (write-on 'hello out))) => "hello")

(check (call-with-output-string
	(lambda (out) (write-on (new Error) out))) => "{Error}")

(check (call-with-output-string
	(lambda (out) (write-on 0 out))) => "0")
(check (call-with-output-string
	(lambda (out) (write-on 1 out))) => "1")
(check (call-with-output-string
	(lambda (out) (write-on 1234 out))) => "1234")
(check (call-with-output-string
	(lambda (out) (write-on -1234 out))) => "-1234")

(check (call-with-output-string
	(lambda (out) (write-on "hello" out))) => "\"hello\"")
(check (call-with-output-string
	(lambda (out) (write-on "\"hello\"" out))) => "\"\\\"hello\\\"\"")
(check (call-with-output-string
	(lambda (out) (write-on "\n" out))) => "\"\\n\"")

(check (call-with-output-string
	(lambda (out) (write-string-quoted-hex-on 15 out))) => "\\x0f")
(check (call-with-output-string
	(lambda (out) (write-string-quoted-hex-on 16 out))) => "\\x10")
(check (call-with-output-string
	(lambda (out) (write-string-quoted-hex-on 64 out))) => "\\x40")

(check (call-with-output-string
	(lambda (out) (write-on 3.14 out))) => "3.14")
(check (call-with-output-string
	(lambda (out) (write-on '(hello) out))) => "(hello)")
(check (call-with-output-string
	(lambda (out) (write-on '(hello world) out))) => "(hello world)")
(check (call-with-output-string
	(lambda (out) (write-on '(1 1.1 "hello" world) out))) => "(1 1.1 \"hello\" world)")
(check (call-with-output-string
	(lambda (out) (write-on '(hello . world) out))) => "(hello . world)")
(check (call-with-output-string
	(lambda (out) (write-on '(hello (beautiful "world")) out)))
       => "(hello (beautiful \"world\"))")

;; Array

(check (call-with-output-string
	(lambda (out) (write-on [1 2 3] out)))
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
	(lambda (out) (write-on triplet0 out)))
       => "{triplet: hello \"world\" 47}")
