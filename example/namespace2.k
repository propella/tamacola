(define-macro library
  (lambda (exp)
    (let ((name (car exp))
          (rest (cdr exp)))
      `(let ()
         (in-ns ,(symbol->string name))
         (in-imports "" ,(symbol->string name))
         ,@rest
         (in-ns "")
         (in-imports "")))))

(library human
  (define greeting "Hello"))

(library dog
  (define greeting (+ human/greeting ", World")))

(library cat
  (import dog)
  (define meow (+ "Meaw, " greeting)))

(import cat)
(trace meow)
