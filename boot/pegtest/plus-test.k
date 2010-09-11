;;; Create a left recursive structure by PEG

(require "boot/peg.k")
(require "boot/peg-grammar.k")
(require "boot/check.k")

(define make-plus
  (lambda (result xs)
    (if xs
	(make-plus (list '+ result (car xs)) (cdr xs))
	result)))
(check (make-plus 1 '(2)) => '(+ 1 2))

(define grammar (namespace global 'grammar))
(grammar-load "plus.g" grammar)

(in grammar
(check (parse-collection plus "1") => 1)
(check (parse-collection plus "1+2") => '(+ 1 2))
(check (parse-collection plus "1+2+3") => '(+ (+ 1 2) 3))
)
