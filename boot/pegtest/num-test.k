;; Construct a number (left recursion test)

(require "boot/peg.k")
(require "boot/peg-grammar.k")
(require "boot/check.k")

(define make-number
  (lambda (n list)
    (if list
	(make-number (+ (* n 10) (- (car list) #"0")) (cdr list))
	n)))

(define grammar
"
digit = [01234567890]
number = digit+ :xs -> (make-number 0 (->list xs))
")

(let ((expr (->list (parse-collection peg/definitions grammar))))
  (for-each (lambda (expr) (eval expr global)) expr))

(check (make-number 0 '(#"3")) => 3)
(check (make-number 0 '(#"2" #"3")) => 23)
(check (make-number 0 '(#"1" #"2" #"3")) => 123)
(check (make-number 0 '(#"1" #"2" #"3" #"4")) => 1234)

(check (parse-collection digit "1") => #"1")
(check (parse-collection number "1") => 1)
(check (parse-collection number "12") => 12)
(check (parse-collection number "1234") => 1234)

