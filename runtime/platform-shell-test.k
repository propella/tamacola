(check (eval '(+ 3 4)) => 7)

(define result ())
(eval-all* '((+ 3 5)) 'ns (lambda (r) (set! result r)))
(check result => 8)
(check *eval-result* => 8)

(meta-define-macro (compiler) 'double '(lambda (exp) (+ (car exp) (car exp))))
(check (meta-macroexpand '(double 1)) => 2)

(meta-define-macro (compiler)
                   'triple
                   '(lambda (exp)
                      (cons (intern "*")
                            (cons (car exp)
                                  (cons 3 ())))))

(check (meta-macroexpand '(triple 1)) => '(* 1 3))

(check (eval '(double 1)) => 2)
(check (eval '(triple 1)) => 3)

;; list macro function version

(meta-define-macro (compiler)
                   'listf
                   '(lambda (exp)
                      (if (null? exp)
                          '()
                          (cons (intern "cons")
                                (cons (car exp)
                                      (cons (cons (intern "listf")
                                                  (cdr exp))
                                            '()))))))

(check (meta-macroexpand '(listf)) => '())
(check (meta-macroexpand '(listf 1)) => '(cons 1 (listf)))
(check (meta-macroexpand '(listf 1 2)) => '(cons 1 (listf 2)))
(check (meta-macroexpand '(listf 1 2 3)) => '(cons 1 (listf 2 3)))

(check (eval '(listf)) => '())
(check (eval '(listf 1)) => '(1))
(check (eval '(listf 1 2)) => '(1 2))
(check (eval '(listf 1 2 3)) => '(1 2 3))
