;; Unhygienic high-level macros
;; NOTE: Unlike syntax-rules in Scheme, this is not hygienic.

(define *pattern-matcher
  (lambda (formals vals bindings)
    (if (== bindings 'FAIL)
        'FAIL
        (if (pair? formals)
            (if (pair? vals)
                (*pattern-matcher
                 (car formals)
                 (car vals)
                 (*pattern-matcher (cdr formals) (cdr vals) bindings))
                'FAIL)
            (if (null? formals)
                (if (null? vals) bindings 'FAIL)
                (cons (cons formals vals) bindings))))))

(define pattern-matcher
  (lambda (formals vals)
    (*pattern-matcher formals vals '())))

(define pattern-constructor
  (lambda (bindings template)
    (if (pair? template)
        (cons
         (pattern-constructor bindings (car template))
         (pattern-constructor bindings (cdr template)))
        (let ((found (assq template bindings)))
          (if (pair? found)
              (cdr found)
              template)))))

(define pattern-apply
  (lambda (cases form)
    (if (pair? cases)
        (let ((case (car cases)))
          (let ((formals (car case))
                (template (cadr case)))
            (let ((bindings (pattern-matcher (cdr formals) (cdr form))))
          (if (== bindings 'FAIL)
              (pattern-apply (cdr cases) form)
              (pattern-constructor bindings template)))))
        (error "Malformed pattern form: " form))))
