;; pattern macro

(define-pattern ((add a b) (+ a b)))
(print (add "Hello, " "World!"))

(define-pattern ((after a b) b))
(after (print "*Pattern Macro does NOT work*") (print "Pattern Macro works"))

;; traditional macro

(define-macro cat (lambda (arg) `(+ ,(car arg) ,(cadr arg))))
(print (cat "Hello, " "World!"))

(define-macro define-form
  (lambda (exp)
    (let ((name (car exp))
          (args (cadr exp))
          (body (caddr exp)))
      `(define-macro ,name
         (lambda (**exp**)
           (let ,(match-assign args '**exp**) ,body))))))

(define-form after (a b) b)
(after (print "*Form Macro does NOT work*") (print "Form Macro works"))
