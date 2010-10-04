;;;; SmallerTalk grammar test

(check (parse-collection dig "1") => 49)
(check (parse-collection num "123") => 123)

(check (parse-collection cmt "\"comment\"") => 'COMMENT)

(check (parse-collection quoted-string "''") => "")
(check (parse-collection quoted-string "'hello'") => "hello")
(check (parse-collection quoted-string "'''hello'''") => "'hello'")
(check (parse-collection sym "#hello") => ''hello)

(check (parse-collection ident "hello") => "hello")
(check (parse-collection reserved "true") => '#t)
(check (parse-collection reserved "false") => '#f)
(check (parse-collection reserved "self") => '#self)
(check (parse-collection variable "something") => 'something)

(check (parse-collection unary "x") => 'x)
(check (parse-collection unary "1 printString") => '(send 1 printString))
(check (parse-collection unary "x printString") => '(send x printString))
(check (parse-collection unary "'hello' printString") => '(send "hello" printString))
(check (parse-collection unary "(3 + 4) printString") => '(send (+ 3 4) printString))

(check (parse-collection unary "1 printString:") => '1)
(check (parse-collection unary "1 first second") => '(send (send 1 first) second))

(check (parse-collection binary "1") => 1)
(check (parse-collection binary "1 + 2") => '(+ 1 2))
(check (parse-collection binary "1 + 2 * 3") => '(* (+ 1 2) 3))
(check (parse-collection binary "1 hoge + 2 hage") => '(+ (send 1 hoge) (send 2 hage)))

(check (parse-collection keyword-part "first: 2") => '("first" . 2))
(check (parse-collection keyword-rest "first: 2") => '("first:" . (2)))
(check (parse-collection keyword-rest "first: 2 second: 3") => '("first:second:" . (2 3)))

(check (parse-collection keyword "1") => 1)
(check (parse-collection keyword "1 first: 2") => '(send 1 first: 2))
(check (parse-collection keyword "1 first: 2 second: 3") => '(send 1 first:second: 2 3))

(check (parse-collection keyword "1 first: 2 second + 3 third: 4")
       => '(send 1 first:third: (+ (send 2 second) 3) 4))

(check (parse-collection keyword "(1 first: 2 second + 3 third: 4) fourth")
       => '(send (send 1 first:third: (+ (send 2 second) 3) 4) fourth))

(check (parse-collection assign "a := 3 + 4") => '(set! a (+ 3 4)))
(check (parse-collection args ":a :b | ") => '(a b))
(check (parse-collection tmps "| a b | ") => '(a b))
(check (parse-collection stmts "1. 2.") => '(1 2))
(check (parse-collection stmts "1") => '(1))
(check (parse-collection stmts "x") => '(x))

(check (parse-collection block "[]") => '(lambda () (let ())))
(check (parse-collection block "[ :a | ]") => '(lambda (a) (let ())))
(check (parse-collection block "[ | t | ]") => '(lambda () (let (t))))
(check (parse-collection block "[ 1 ]") => '(lambda () (let () 1)))
(check (parse-collection block "[ :a | | t | ]") => '(lambda (a) (let (t))))
(check (parse-collection block "[ :a | | t | 1 ]") => '(lambda (a) (let (t) 1)))
(check (parse-collection block "[ :a | | t | 1. 2 ]") => '(lambda (a) (let (t) 1 2)))
(check (parse-collection block "[ :a | | t | 1 unary ]") => '(lambda (a) (let (t) (send 1 unary))))

;; Runtime

(define st/compile (lambda (str) (parse-collection _stmts str)))

(define st/eval
  (lambda (str)
    (eval-all (st/compile str))))

(slot-setq! (slot-getq Function prototype) value
           (lambda (a1) (#self)))

(slot-setq! (slot-getq Function prototype) value:
           (lambda (a1) (#self a1)))

(slot-setq! (slot-getq Function prototype) value:value:
           (lambda (a1 a2) (#self a1 a2)))

(slot-setq! (slot-getq Object prototype) at:
           (lambda (key) (slot-get #self key)))

(slot-setq! (slot-getq Boolean prototype) ifTrue:ifFalse:
            (lambda (a b)
              (if #self (a) (b))))

(slot-setq! (slot-getq Object prototype) methodAt:put:
            (lambda (method block)
              (slot-set! (slot-getq #self prototype) method block)))

(slot-setq! (slot-getq Object prototype) println
            (lambda () (trace #self)))

(check (st/eval "3 + 4") => 7)
(check (st/eval "[:x | x + 4] value: 3") => 7)
(check (st/eval "'hello' at: #length") => 5)
(check (st/eval "(1 > 0) ifTrue: [ #yes ] ifFalse: [ #no ]") => 'yes)

(check (st/eval "
[ | fibs |
  fibs := [ :n |
   (n < 2) ifTrue: [ 1 ]
           ifFalse: [ (fibs value: (n - 1)) + (fibs value: (n - 2))]
  ].
  fibs value: 10
] value
") => 89)

(st/eval "String methodAt: #size put: [self at: #length]")
(check (st/eval "'hello' size") => 5)

(st/eval "
Number methodAt: #fib put:
[
  | i |
  i := self.
  (self < 2) ifTrue: [1]
             ifFalse: [ (i - 1) fib + (i - 2) fib]
]")

(check (st/eval "10 fib") => 89)
