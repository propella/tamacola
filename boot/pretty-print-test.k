(require "boot/pretty-print.k")

;;;; pretty-print

;; lists

(check (call-with-output-string
	(lambda (out) (*pretty-print-pair '(1 2 3) 0 out)))
       => "(1\n 2\n 3)")

(pretty-print-columns 20)

(check (call-with-output-string
	(lambda (out) (*pretty-print '() 0 out))) => "()")

(check (call-with-output-string
	(lambda (out) (pretty-print '() out))) => "()\n")

(check (call-with-output-string
	(lambda (out) (pretty-print '(1234567890) out))) => "(1234567890)\n")

(check (call-with-output-string
	(lambda (out) (pretty-print '(1234567890 1234567890) out)))
       => "(1234567890\n 1234567890)\n")
