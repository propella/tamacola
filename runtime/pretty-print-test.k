;(require "boot/pretty-print.k")

;;;; pretty-print

;; lists

(check (call-with-output-string
	(lambda (out) (*pretty-print-pair-on '(1 2 3) 0 out)))
       => "(1\n 2\n 3)")

(set-pretty-print-columns 20)

(check (call-with-output-string
	(lambda (out) (*pretty-print-on '() 0 out))) => "()")

(check (call-with-output-string
	(lambda (out) (pretty-print-on '() out))) => "()\n")

(check (call-with-output-string
	(lambda (out) (pretty-print-on '(1234567890) out))) => "(1234567890)\n")

(check (call-with-output-string
	(lambda (out) (pretty-print-on '(1234567890 1234567890) out)))
       => "(1234567890\n 1234567890)\n")

;; Structure

(define-structure triplet (fst snd thd))
(define triplet0
  (let ((t (new-triplet)))
    (set-triplet-fst t 'hello)
    (set-triplet-snd t "world")
    (set-triplet-thd t 47)
    t))

(check (call-with-output-string
	(lambda (out) (pretty-print-on triplet0 out)))
       => "{triplet:\nhello\n\"world\"\n47}\n")
