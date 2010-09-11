;; A simple Chain of Meaning test

(require "boot/peg.k")
(require "boot/peg-grammar.k")
(require "boot/check.k")

(define is-number
  (lambda (*stream* *parser*)
    (if (number? (stream/peek *stream*))
        (begin (stream/set-parser-result
                *parser*
                (stream/next *stream*))
               #t)
        #f)))

(define is-symbol
  (lambda (*stream* *parser*)
    (if (symbol? (stream/peek *stream*))
        (begin (stream/set-parser-result
                *parser*
                (stream/next *stream*))
               #t)
        #f)))

(grammar-load "sexp.g" global)

(check (parse-collection char "+") => #"+")
(check (parse-collection sym "+") => '+)
(check (parse-collection num "1234") => 1234)
(check (parse-collection sexp "hello") => 'hello)
(check (parse-collection sexp "1234") => 1234)
(check (parse-collection sexp "(hello world)") => '(hello world))
(check (parse-collection sexp "(3 4)") => '(3 4))
(check (parse-collection sexp "(+ 3 4)") => '(+ 3 4))

(check (parse-collection inst '((+ 3 4))) => '((pushint 3) (pushint 4) (add)))
(check (parse-collection inst '((f 3 4))) => '((getlex ((ns "") "f")) (pushnull) (pushint 3) (pushint 4) (call 2)))

(check (parse-collection compile "(print 42)")
       =>'((getlex ((ns "") "print"))
            (pushnull)
            (pushint 42)
            (call 1)))

(check (parse-collection inst '((print (+ 3 4))))
       => '((getlex ((ns "") "print"))
            (pushnull)
            (pushint 3)
            (pushint 4)
            (add)
            (call 1)))

(check (parse-collection compile "(print (+ 3 4))")
       =>'((getlex ((ns "") "print"))
            (pushnull)
            (pushint 3)
            (pushint 4)
            (add)
            (call 1)))

                         
