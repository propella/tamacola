;;;; SmallerTalk grammar test

(check (parse-collection st/digit "1") => 49)
(check (parse-collection st/number "123") => 123)

(check (parse-collection st/comment "\"comment\"") => 'COMMENT)

(check (parse-collection st/quoted-string "''") => "")
(check (parse-collection st/quoted-string "'hello'") => "hello")
(check (parse-collection st/quoted-string "'''hello'''") => "'hello'")
(check (parse-collection st/symbol "#hello") => ''hello)

(check (parse-collection st/identifier "hello") => "hello")
(check (parse-collection st/reserved "true") => '#t)
(check (parse-collection st/reserved "false") => '#f)
(check (parse-collection st/reserved "self") => '#self)

(check (parse-collection st/unary "x") => 'x)
(check (parse-collection st/unary "1 printString") => '(send 1 printString))
(check (parse-collection st/unary "x printString") => '(send x printString))
(check (parse-collection st/unary "'hello' printString") => '(send "hello" printString))
(check (parse-collection st/unary "(3 + 4) printString") => '(send (+ 3 4) printString))

(check (parse-collection st/unary "1 printString:") => '1)
(check (parse-collection st/unary "1 first second") => '(send (send 1 first) second))

(check (parse-collection st/binary "1") => 1)
(check (parse-collection st/binary "1 + 2") => '(+ 1 2))
(check (parse-collection st/binary "1 + 2 * 3") => '(* (+ 1 2) 3))
(check (parse-collection st/binary "1 hoge + 2 hage") => '(+ (send 1 hoge) (send 2 hage)))

(check (parse-collection st/keyword-part "first: 2") => '("first" . 2))
(check (parse-collection st/keyword-rest "first: 2") => '("first:" . (2)))
(check (parse-collection st/keyword-rest "first: 2 second: 3") => '("first:second:" . (2 3)))

(check (parse-collection st/keyword "1") => 1)
(check (parse-collection st/keyword "1 first: 2") => '(send 1 first: 2))
(check (parse-collection st/keyword "1 first: 2 second: 3") => '(send 1 first:second: 2 3))

(check (parse-collection st/keyword "1 first: 2 second + 3 third: 4")
       => '(send 1 first:third: (+ (send 2 second) 3) 4))

(check (parse-collection st/keyword "(1 first: 2 second + 3 third: 4) fourth")
       => '(send (send 1 first:third: (+ (send 2 second) 3) 4) fourth))

(check (parse-collection st/assign "a := 3 + 4") => '(set! a (+ 3 4)))
(check (parse-collection st/args ":a :b | ") => '(a b))
(check (parse-collection st/tmps "| a b | ") => '(a b))
(check (parse-collection st/exprs "1. 2.") => '(1 2))
(check (parse-collection st/exprs "1") => '(1))
(check (parse-collection st/exprs "x") => '(x))

(check (parse-collection st/block "[]") => '(lambda () (let ())))
(check (parse-collection st/block "[ :a | ]") => '(lambda (a) (let ())))
(check (parse-collection st/block "[ | t | ]") => '(lambda () (let (t))))
(check (parse-collection st/block "[ 1 ]") => '(lambda () (let () 1)))
(check (parse-collection st/block "[ :a | | t | ]") => '(lambda (a) (let (t))))
(check (parse-collection st/block "[ :a | | t | 1 ]") => '(lambda (a) (let (t) 1)))
(check (parse-collection st/block "[ :a | | t | 1. 2 ]") => '(lambda (a) (let (t) 1 2)))
(check (parse-collection st/block "[ :a | | t | 1 unary ]") => '(lambda (a) (let (t) (send 1 unary))))

;; Runtime

(define st/compile (lambda (str) (parse-collection st/_exprs str)))

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
