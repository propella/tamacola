(require "boot/peg.k")
(require "boot/peg-grammar.k")
(require "boot/check.k")

;;;; S expression

(check (parse-collection peg/sexpr "symbol ") => 'symbol)
(check (parse-collection peg/sexpr "symbol-dash ") => 'symbol-dash)
(check (parse-collection peg/sexpr "symbol1number ") => 'symbol1number)
(check (parse-collection peg/sexpr "symbol.dot ") => 'symbol.dot)
(check (parse-collection peg/sexpr "symbol/slash ") => (string->symbol "symbol/slash"))
(check (parse-collection peg/sexpr "/// ") => '///)
(check (parse-collection peg/sexpr "\"string\" ") => "string")
(check (parse-collection peg/sexpr "42 ") => 42)
(check (parse-collection peg/sexpr "-42 ") => -42)
(check (parse-collection peg/sexpr "-2147483648 ") => (- -2147483647 1))
(check (parse-collection peg/sexpr "+42 ") => 42)
(check (parse-collection peg/sexpr "4.2 ") => 4.2)
(check (parse-collection peg/sexpr "-4.2 ") => -4.2)
(check (parse-collection peg/sexpr "+4.2 ") => 4.2)
(check (parse-collection peg/sexpr "#xf ") => 15)
(check (parse-collection peg/sexpr "0xf ") => 15)
(check (parse-collection peg/sexpr "#f ") => '#f)
(check (parse-collection peg/sexpr "#\"A\" ") => 65)
(check (parse-collection peg/sexpr "#\"\\011\" ") => #"\t")
(check (parse-collection peg/sexpr "'quote ") => '(quote quote))
(check (parse-collection peg/sexpr "(1 . 2) ") => '(1 . 2))
(check (parse-collection peg/sexpr "(head . tail) ") => '(head . tail))
(check (parse-collection peg/sexpr "(head tail) ") => '(head tail))
(check (parse-collection peg/sexpr "[] ") => (array))
(check (parse-collection peg/sexpr "[1] ") => (array 1))
(check (parse-collection peg/sexpr "[1 2 3] ") => (array 1 2 3))
(check (parse-collection peg/sexpr "(symbol (\"string\" 42)) ") => '(symbol ("string" 42)))
(check (->list (parse-collection peg/sexprs "first second ")) => '(first second))

(check (parse-collection peg/sexpr "`(define-macro ,name (lambda ,args ,@body))")
       => '(quasiquote (define-macro (unquote name)
                         (lambda (unquote args)
                           (unquote-splicing body)))))

(check (parse-collection peg/_sexpr ";; comment
contents
;; comment
") => 'contents)

(check (parse-collection peg/_sexpr ";; comment
(asm
 (method
  (((signature
     ((return_type *) (param_type ()) (name \"hello\") (flags 0) (options ()) (param_names ())))
    (code
     ((getlocal 0)
      (pushscope)
      (findpropstrict ((package \"\") \"print\"))
      (pushstring \"Hello, World!!\")
      (callproperty ((package \"\") \"print\") 1)
      (returnvoid)))
    )))
 (script (((init (method 0)) (trait ())))))
") => '(asm
 (method
  (((signature
     ((return_type *) (param_type ()) (name "hello") (flags 0) (options ()) (param_names ())))
    (code
     ((getlocal 0)
      (pushscope)
      (findpropstrict ((package "") "print"))
      (pushstring "Hello, World!!")
      (callproperty ((package "") "print") 1)
      (returnvoid)))
    )))
 (script (((init (method 0)) (trait ()))))))

;; literal

(check (parse-collection peg/literal "'symbol") => '(OBJ 'symbol))
(check (parse-collection peg/literal "'( 'symbol )") => '(STRUCT (OBJ 'symbol)))

;; apply

(check (parse-collection peg/apply-arg "arg") => 'arg)
(check (->list (parse-collection peg/apply-args "a b c")) => '(a b c))
(check (parse-collection peg/apply-rule "<rule >") => '(RULE rule))
; (check (parse-collection peg/apply-rule "<rule>") => '(RULE rule)) ;; todo: bug
(check (parse-collection peg/apply-rule "<rule a b c>") => '(RULE rule a b c))

;; class
(check (parse-collection peg/class "[abc]") => "abc")

;; primary

(check (parse-collection peg/primary "\"t e x t\"") => '(SEQ [#"t" #" " #"e" #" " #"x" #" " #"t"]))
(check (parse-collection peg/primary "[abc]") => '(CLASS "abc"))
(check (parse-collection peg/primary "#\"A\"") => '(OBJ #"A"))
(check (parse-collection peg/primary "[\t]") => '(CLASS "\t"))
(check (parse-collection peg/primary ".") => '(ANY))
(check (parse-collection peg/primary "( 'rule )") => '(OBJ 'rule))
(check (parse-collection peg/primary "-> (func a b)") => '(RETURN (func a b)))
(check (parse-collection peg/primary "rule") => '(RULE rule))

;; suffix

(check (parse-collection peg/suffix "rule?") => '(OPT (RULE rule)))
(check (parse-collection peg/suffix "rule+") => '(MANY1 (RULE rule)))
(check (parse-collection peg/suffix "rule*") => '(MANY (RULE rule)))

;; ranged

(check (parse-collection peg/ranged "rule@") => '(SOURCE (RULE rule)))
(check (parse-collection peg/ranged "rule") => '(RULE rule))

;; stringed

(check (parse-collection peg/stringed "rule$#10") => '(NUMBER 10 (RULE rule)))
(check (parse-collection peg/stringed "rule$") => '(STRING (RULE rule)))

;; stored

(check (parse-collection peg/stored "rule :x") => '(STORE x (RULE rule)))

;; prefix

(check (parse-collection peg/prefix "! rule") => '(NOT (RULE rule)))
(check (parse-collection peg/prefix "& rule") => '(PRED (RULE rule)))

;; sequence

(check (parse-collection peg/sequence "rule") => '(RULE rule))
(check (parse-collection peg/sequence "first second third") => '(AND (RULE first) (RULE second) (RULE third)))

;; expression

(check (parse-collection peg/expression "rule") => '(RULE rule))
(check (parse-collection peg/expression "first | second") => '(OR (RULE first) (RULE second)))

;; find-variables

(check (parse-collection peg/find-variables '(() (STORE x (RETURN)))) => '(x))
(check (parse-collection peg/find-variables '((y x) (STORE z (RETURN)))) => '(z y x))
(check (parse-collection peg/find-variables '((y x) (STORE x (RETURN)))) => '(y x))
(check (parse-collection peg/find-variables '((x) (RETURN))) => '(x))
(check (parse-collection peg/find-variables '((x) (SOMETHING))) => '(x))
(check (parse-collection peg/find-variables '((x) (AND (SOMETHING) (STORE y (SOMETHING))))) => '(y x))

;; definition

(check (parse-collection peg/definition "rule = first ")
       => '(define-rule rule () (RULE first)))
(check (parse-collection peg/definition "rule = first | second ")
       => '(define-rule rule () (OR (RULE first) (RULE second))))
(check (parse-collection peg/definition "rule = first:x | second:y ")
       => '(define-rule rule (y x) (OR (STORE x (RULE first)) (STORE y (RULE second)))))
(check (->list (parse-collection peg/definitions "rule1 = exp1 rule2 = exp2"))
       => '((define-rule rule1 () (RULE exp1))
            (define-rule rule2 () (RULE exp2))))

;;;; PEG grammar

(check (parse-collection peg/definition "match-H4 = \"H\" -> 'ok")
       => '(define-rule match-H4 () (AND (OBJ #"H") (RETURN 'ok))))
;; (run-parser "match-H4 = \"H\" -> 'ok ;" peg/definition 't)

;; (check (parse-collection match-H4 "HELLO") => 'ok)
;; (check (parse-collection match-H4 "BYE") => 'FAIL)

(define test-grammar "ANYTHING = . -> 'ok")

(check (parse-collection peg/definition test-grammar) =>
       '(define-rule ANYTHING () (AND (ANY) (RETURN (quote ok)))))

;; (eval (parse-collection peg/definition test-grammar) global)
;; (check (parse-collection ANYTHING "hello") => 'ok)
