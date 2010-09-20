;; A simple Chain of Meaning test (Mini lisp compiler)

(define is-number
  (lambda (*stream* *parser*)
    (if (number? (peek *stream*))
        (begin (set-parser-result *parser* (next *stream*))
               #t)
        #f)))

(define is-symbol
  (lambda (*stream* *parser*)
    (if (symbol? (peek *stream*))
        (begin (set-parser-result *parser* (next *stream*))
               #t)
        #f)))

(import sexp)

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

(check (parse-collection inst '((print (+ 3 4))))
       => '((getlex ((ns "") "print"))
            (pushnull)
            (pushint 3)
            (pushint 4)
            (add)
            (call 1)))

(check (parse-collection program '((print 42)))
       => '(asm
            (method
             (((signature ((return_type *) (param_type ()) (name "program")
                           (flags 0) (options ()) (param_names ())))
               (code ((getlocal 0)
                      (pushscope)
                      (getlex ((ns "") "print"))
                      (pushnull)
                      (pushint 42)
                      (call 1)
                      (returnvalue))))))
            (script (((init (method 0)) (trait ()))))))
                         
(let* ((asm (parse-collection execute "(print (+ 3 4))"))
       (bytes (call-with-output-bytes
               (lambda (port) (write-asm asm port)))))
  (load-byte-codes bytes '()))
