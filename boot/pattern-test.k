(require "boot/std.k")
(require "boot/pattern.k")

(check (pattern-matcher 'a '(any thing)) => '((a . (any thing))))
(check (pattern-matcher '() '(any thing)) => 'FAIL)
(check (pattern-matcher '(a) '(x)) => '((a . x)))
(check (pattern-matcher '(a b) '(x y)) => '((a . x) (b . y)))
(check (pattern-matcher '(a . b) '(x y)) => '((a . x) (b . (y))))
(check (pattern-matcher '(a . b) '(x y z)) => '((a . x) (b . (y z))))
(check (pattern-matcher '((a b) c) '((x y) (z z))) => '((a . x) (b . y) (c . (z z))))
(check (pattern-matcher '(a (b c)) '((x y) (z z))) => '((a . (x y)) (b . z) (c . z)))
(check (pattern-matcher '(a) '()) => 'FAIL)
(check (pattern-matcher '(a b) '(x)) => 'FAIL)
(check (pattern-matcher '((a b) c) '(x y)) => 'FAIL)

(check (pattern-constructor '((a . x)) 'a) => 'x)
(check (pattern-constructor '((a . x) (b . y)) '(a b)) => '(x y))
(check (pattern-constructor '((a . x)) '(a b)) => '(x b))
(check (pattern-constructor '((a . x) (b . y)) '((a b) (b a))) => '((x y) (y x)))

(check (pattern-apply
        '(((_ a b) (let ((temp a))
                     (set! a b)
                     (set! b temp))))
        '(swap! x y))
       => '(let ((temp x))
             (set! x y)
             (set! y temp)))

(define let-star-pattern
  '(((let* () . body)
     (let () . body))
    ((let* (bindings) . body)
     (let (bindings) . body))
    ((let* (bindings . rest) . body)
     (let (bindings) (let* rest . body)))))

(check (pattern-apply let-star-pattern '(let* () body))
       => '(let () body))

(check (pattern-apply let-star-pattern '(let* ((a x)) a))
       => '(let ((a x)) a))

(check (pattern-apply let-star-pattern '(let* ((a x) (b a)) b))
       => '(let ((a x)) (let* ((b a)) b)))

(define-pattern
  ((swap! a b) (let ((temp a))
                 (set! a b)
                 (set! b temp))))

(check (let ((x 1) (y 2)) (swap! x y) (cons x y)) => '(2 . 1))

(define-pattern
  ((let-star () . body)
   (let () . body))
  ((let-star (bindings) . body)
   (let (bindings) . body))
  ((let-star (bindings . rest) . body)
   (let (bindings) (let-star rest . body))))

(check (let-star ((a 7) (b a)) b) => 7)

;; cond: http://www.scheme.com/tspl3/syntax.html

(define cond-pattern
  '(((cond) ())
    ((cond (c0 . b0) . rest) (if c0 (begin . b0) (cond . rest)))))

(check (pattern-apply cond-pattern
                      '(cond (c b)))
       => '(if c (begin b) (cond)))

(check (pattern-apply cond-pattern
                      '(cond (c0 b0)
                              (c1 b1)))
       => '(if c0 (begin b0) (cond (c1 b1))))

(check (pattern-apply cond-pattern
                      '(cond (c0 b0 b00)
                              (c1 b1)
                              (c2 b2)))
       => '(if c0 (begin b0 b00) (cond (c1 b1) (c2 b2))))
