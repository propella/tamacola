;; SRFI-78 style lightweight testing
;; http://srfi.schemers.org/srfi-78/srfi-78.html
;;
;; Usage: (check (+ 3 4) => 7)

(define-syntax check
  (syntax-rules (=>)
    ((_ expr => expected)
     (let ((*expr expr)
	   (*expected expected))
       (if (equal? *expr *expected)
	   ()
	   (check-error 'expr *expr *expected))))))

(define check-error
  (lambda (expr result expected)
    (write expr)
    (display " => ")
    (write result) (newline)
    (display "  ; *** failed ***\n")
    (display "  ; expected result: ")
    (write expected) (newline)))
