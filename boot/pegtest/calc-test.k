;; A simple calculator
;; Read from stdin and show the answer.

(require "boot/peg.k")
(require "boot/peg-grammar.k")
(require "boot/check.k")

(define calc (namespace global 'calc))

;; Aliases of operator.
(define plus +)
(define minus -)
(define mul *)
(define div /)

;; Make a left associative expression from a sequence of command like
;; 1 and ((plus 2) (minus 3) ..) to (minus (plus 1 2) 3)
(define make-left
  (lambda (first rest)
    (if rest
	(let ((op     (car  (car rest)))
	      (second (cadr (car rest))))
	  (make-left (list op first second) (cdr rest)))
	first)))

(grammar-load "calc.g" calc)

(in calc
(check (make-left 1 '((plus 2) (minus 3))) => '(minus (plus 1 2) 3))

(check (parse-collection expr "1") => 1)
(check (parse-collection expr "1 + 2") => '(plus 1 2))
(check (parse-collection expr "1 + 2 - 3") => '(minus (plus 1 2) 3))

(check (parse-collection expr "1 * 2") => '(mul 1 2))
(check (parse-collection expr "1 + 2 * 3") => '(plus 1 (mul 2 3)))

(check (parse-collection expr "(1)") => 1)
(check (parse-collection expr "(1 + 2)") => '(plus 1 2))
(check (parse-collection expr "(1 + 2) * 3") => '(mul (plus 1 2) 3))
(check (parse-collection expr "(1 + 2) * (3 / 4)") => '(mul (plus 1 2) (div 3 4)))
)

(while 1
       (print ".")
       (let ((exp
	      (parse-stream calc/expr (stream/file-stream-open "/dev/stdin" "r"))))
	 (println exp)
	 (println (eval exp global))))
