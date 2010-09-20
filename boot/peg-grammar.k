(library
 peg
 (define-rule
  syntax-error
  ()
  (RETURN (error "syntax error near: " (UP-TO-END))))
 (define-rule blank () (CLASS " \t"))
 (define-rule
  eol
  ()
  (OR (AND (OBJ 10) (MANY (OBJ 13))) (AND (OBJ 13) (MANY (OBJ 10)))))
 (define-rule comment () (AND (OBJ 59) (MANY (AND (NOT (RULE eol)) (ANY)))))
 (define-rule _ () (MANY (OR (RULE blank) (RULE eol) (RULE comment))))
 (define-rule EQUALS () (AND (OBJ 61) (RULE _)))
 (define-rule BAR () (AND (OBJ 124) (RULE _)))
 (define-rule AMPER () (AND (OBJ 38) (RULE _)))
 (define-rule PLING () (AND (OBJ 33) (RULE _)))
 (define-rule PLUS () (AND (OBJ 43) (RULE _)))
 (define-rule ASTERISK () (AND (OBJ 42) (RULE _)))
 (define-rule QUERY () (AND (OBJ 63) (RULE _)))
 (define-rule COLON () (AND (OBJ 58) (RULE _)))
 (define-rule DOLLARHASH () (AND (SEQ [36 35]) (RULE _)))
 (define-rule DOLLAR () (AND (OBJ 36) (RULE _)))
 (define-rule AT () (AND (OBJ 64) (RULE _)))
 (define-rule DOT () (AND (OBJ 46) (RULE _)))
 (define-rule LPAREN () (AND (OBJ 40) (RULE _)))
 (define-rule RPAREN () (AND (OBJ 41) (RULE _)))
 (define-rule LBRACK () (AND (OBJ 91) (RULE _)))
 (define-rule RBRACK () (AND (OBJ 93) (RULE _)))
 (define-rule LANGLE () (AND (OBJ 60) (RULE _)))
 (define-rule RANGLE () (AND (OBJ 62) (RULE _)))
 (define-rule QUOTE () (AND (OBJ 39) (RULE _)))
 (define-rule TICK () (AND (OBJ 96) (RULE _)))
 (define-rule COMMAAT () (AND (SEQ [44 64]) (RULE _)))
 (define-rule COMMA () (AND (OBJ 44) (RULE _)))
 (define-rule QUOTEDBL () (AND (OBJ 34) (RULE _)))
 (define-rule RARROW () (AND (SEQ [45 62]) (RULE _)))
 (define-rule digit8 () (CLASS "01234567"))
 (define-rule digit10 () (CLASS "0123456789"))
 (define-rule digit16 () (CLASS "0123456789abcdefABCDEF"))
 (define-rule sign () (CLASS "+-"))
 (define-rule
  double
  (n)
  (AND
   (STORE
    n
    (SOURCE
     (AND
      (OPT (RULE sign))
      (MANY1 (RULE digit10))
      (OBJ 46)
      (MANY1 (RULE digit10)))))
   (RULE _)
   (RETURN (string->double (->string n)))))
 (define-rule
  number
  (xs s)
  (OR
   (AND
    (SEQ [35 120])
    (STORE s (OPT (RULE sign)))
    (STORE xs (MANY1 (RULE digit16)))
    (RULE _)
    (RETURN (string->number-radix-sign (->string xs) 16 (->string s))))
   (AND
    (SEQ [48 120])
    (STORE s (OPT (RULE sign)))
    (STORE xs (MANY1 (RULE digit16)))
    (RULE _)
    (RETURN (string->number-radix-sign (->string xs) 16 (->string s))))
   (AND
    (STORE s (OPT (RULE sign)))
    (STORE xs (MANY1 (RULE digit10)))
    (RULE _)
    (RETURN (string->number-radix-sign (->string xs) 10 (->string s))))))
 (define-rule
  letter0
  ()
  (CLASS "-ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz_"))
 (define-rule letter1 () (OR (RULE letter0) (RULE digit10)))
 (define-rule
  identifier
  (xs)
  (AND
   (STORE xs (SOURCE (AND (RULE letter0) (MANY (RULE letter1)))))
   (RULE _)
   (RETURN (intern (->string xs)))))
 (define-rule
  escaped
  (xs)
  (AND
   (OBJ 92)
   (OR
    (AND (OBJ 101) (RETURN 27))
    (AND (OBJ 97) (RETURN 7))
    (AND (OBJ 98) (RETURN 8))
    (AND (OBJ 102) (RETURN 12))
    (AND (OBJ 110) (RETURN 10))
    (AND (OBJ 114) (RETURN 13))
    (AND (OBJ 116) (RETURN 9))
    (AND (OBJ 118) (RETURN 11))
    (AND (OBJ 39) (RETURN 39))
    (AND (OBJ 34) (RETURN 34))
    (AND (OBJ 92) (RETURN 92))
    (AND
     (OBJ 120)
     (STORE xs (SOURCE (AND (RULE digit16) (RULE digit16))))
     (RETURN (string->number-radix (->string xs) 16)))
    (AND
     (OBJ 117)
     (STORE
      xs
      (SOURCE (AND (RULE digit16) (RULE digit16) (RULE digit16) (RULE digit16))))
     (RETURN (string->number-radix (->string xs) 16)))
    (AND
     (STORE xs (SOURCE (AND (RULE digit8) (RULE digit8) (RULE digit8))))
     (RETURN (string->number-radix (->string xs) 8))))))
 (define-rule character () (OR (RULE escaped) (ANY)))
 (define-rule
  character-literal
  (c)
  (AND (SEQ [35 34]) (STORE c (RULE character)) (OBJ 34) (RULE _) (RETURN c)))
 (define-rule
  class
  (s)
  (AND
   (OBJ 91)
   (STORE s (MANY (AND (NOT (OBJ 93)) (RULE character))))
   (RULE RBRACK)
   (RETURN (->string s))))
 (define-rule
  string
  (s)
  (AND
   (OBJ 34)
   (STORE s (MANY (AND (NOT (OBJ 34)) (RULE character))))
   (RULE QUOTEDBL)
   (RETURN (->string s))))
 (define-rule
  symbol0
  ()
  (CLASS
   "-!$#%&*+/<=>?:@ABCDEFGHIJKLMNOPQRSTUVWXYZ\\^_abcdefghijklmnopqrstuvwxyz|~"))
 (define-rule
  symbol1
  ()
  (CLASS
   "-!$#%&*+/<=>?:@ABCDEFGHIJKLMNOPQRSTUVWXYZ\\^_abcdefghijklmnopqrstuvwxyz|~0123456789."))
 (define-rule
  symbol
  (s)
  (AND
   (STORE s (SOURCE (AND (RULE symbol0) (MANY (RULE symbol1)))))
   (RULE _)
   (RETURN (intern (->string s)))))
 (define-rule
  sexpr
  (es e)
  (OR
   (RULE double)
   (RULE number)
   (RULE string)
   (RULE character-literal)
   (RULE symbol)
   (AND (RULE QUOTE) (STORE e (RULE sexpr)) (RETURN (list (quote quote) e)))
   (AND (RULE TICK) (STORE e (RULE sexpr)) (RETURN (list (quote quasiquote) e)))
   (AND
    (RULE COMMAAT)
    (STORE e (RULE sexpr))
    (RETURN (list (quote unquote-splicing) e)))
   (AND (RULE COMMA) (STORE e (RULE sexpr)) (RETURN (list (quote unquote) e)))
   (AND
    (RULE LBRACK)
    (STORE e (MANY (RULE sexpr)))
    (RULE RBRACK)
    (RETURN (->array e)))
   (AND
    (RULE LPAREN)
    (STORE e (MANY (RULE sexpr)))
    (RULE RPAREN)
    (RETURN (->list e)))
   (AND
    (RULE LPAREN)
    (STORE e (MANY (RULE sexpr)))
    (RULE DOT)
    (STORE es (RULE sexpr))
    (RULE RPAREN)
    (RETURN (concat (->list e) es)))))
 (define-rule sexprs () (MANY (RULE sexpr)))
 (define-rule _sexpr () (AND (RULE _) (RULE sexpr)))
 (define-rule
  _sexprs
  ()
  (OR (AND (RULE _) (RULE sexprs) (NOT (ANY))) (RULE syntax-error)))
 (define-rule expression () (ANY))
 (define-rule
  literal
  (e)
  (AND
   (RULE QUOTE)
   (OR
    (AND
     (RULE LPAREN)
     (STORE e (RULE expression))
     (RULE RPAREN)
     (RETURN (quasiquote (STRUCT (unquote e)))))
    (AND
     (STORE e (RULE symbol))
     (RETURN (quasiquote (OBJ (quote (unquote e)))))))))
 (define-rule
  apply-arg
  ()
  (AND (NOT (OBJ 62)) (OR (RULE identifier) (RULE sexpr))))
 (define-rule apply-args () (MANY (RULE apply-arg)))
 (define-rule
  apply-rule
  (a i)
  (AND
   (RULE LANGLE)
   (STORE i (RULE symbol))
   (STORE a (RULE apply-args))
   (RULE RANGLE)
   (RETURN (quasiquote (RULE (unquote i) (unquote-splicing a))))))
 (define-rule
  primary
  (id e)
  (OR
   (AND
    (STORE e (RULE string))
    (RETURN
     (if
      (empty? e)
      (quote t)
      (if
       (singleton? e)
       (quasiquote (OBJ (unquote (first e))))
       (quasiquote (SEQ (unquote (->array e))))))))
   (AND
    (STORE e (RULE character-literal))
    (RETURN (quasiquote (OBJ (unquote e)))))
   (AND (STORE e (RULE class)) (RETURN (quasiquote (CLASS (unquote e)))))
   (AND (RULE DOT) (RETURN (quote (ANY))))
   (AND (RULE LPAREN) (STORE e (RULE expression)) (RULE RPAREN) (RETURN e))
   (AND
    (RULE RARROW)
    (STORE e (RULE sexpr))
    (RETURN (quasiquote (RETURN (unquote e)))))
   (RULE literal)
   (RULE apply-rule)
   (AND
    (STORE id (RULE identifier))
    (NOT (OBJ 61))
    (RETURN (quasiquote (RULE (unquote id)))))))
 (define-rule
  suffix
  (p)
  (AND
   (STORE p (RULE primary))
   (OPT
    (OR
     (AND (RULE QUERY) (STORE p (RETURN (quasiquote (OPT (unquote p))))))
     (AND (RULE PLUS) (STORE p (RETURN (quasiquote (MANY1 (unquote p))))))
     (AND (RULE ASTERISK) (STORE p (RETURN (quasiquote (MANY (unquote p))))))))
   (RETURN p)))
 (define-rule
  ranged
  (s)
  (AND
   (STORE s (RULE suffix))
   (OPT (AND (RULE AT) (STORE s (RETURN (quasiquote (SOURCE (unquote s)))))))
   (RETURN s)))
 (define-rule
  stringed
  (n s)
  (AND
   (STORE s (RULE ranged))
   (OPT
    (OR
     (AND
      (RULE DOLLARHASH)
      (STORE n (RULE number))
      (STORE s (RETURN (quasiquote (NUMBER (unquote n) (unquote s))))))
     (AND (RULE DOLLAR) (STORE s (RETURN (quasiquote (STRING (unquote s))))))))
   (RETURN s)))
 (define-rule
  stored
  (i s)
  (AND
   (STORE s (RULE stringed))
   (OPT
    (AND
     (RULE COLON)
     (STORE i (RULE identifier))
     (STORE s (RETURN (quasiquote (STORE (unquote i) (unquote s)))))))
   (RETURN s)))
 (define-rule
  prefix
  (s)
  (OR
   (AND
    (RULE PLING)
    (STORE s (RULE stored))
    (RETURN (quasiquote (NOT (unquote s)))))
   (AND
    (RULE AMPER)
    (STORE s (RULE stored))
    (RETURN (quasiquote (PRED (unquote s)))))
   (RULE stored)))
 (define-rule
  sequence
  (s)
  (AND
   (STORE s (MANY1 (RULE prefix)))
   (RETURN
    (if
     (empty? s)
     (quote t)
     (if (singleton? s) (first s) (quasiquote (AND (unquote-splicing s))))))))
 (define-rule
  expression
  (tl hd)
  (AND
   (STORE hd (RULE sequence))
   (STORE tl (MANY (AND (RULE BAR) (RULE sequence))))
   (RETURN
    (if (empty? tl) hd (quasiquote (OR (unquote hd) (unquote-splicing tl)))))))
 (define-rule
  find-variables
  (i v)
  (AND
   (STORE v (ANY))
   (STRUCT
    (OR
     (AND
      (OBJ (quote STORE))
      (STORE i (ANY))
      (STORE v (RULE find-variables v (NEXT)))
      (STORE v (RETURN (if (memq? i v) v (cons i v)))))
     (OBJ (quote RETURN))
     (AND (ANY) (MANY (STORE v (RULE find-variables v (NEXT)))))
     (RULE syntax-error)))
   (RETURN v)))
 (define-rule
  definition
  (v e i)
  (AND
   (RULE _)
   (PRED (ANY))
   (OR
    (AND
     (STORE i (RULE identifier))
     (RULE EQUALS)
     (STORE e (RULE expression))
     (STORE v (RULE find-variables (quote ()) e))
     (RETURN (quasiquote (define-rule (unquote i) (unquote v) (unquote e)))))
    (RULE syntax-error))))
 (define-rule definitions () (MANY (RULE definition))))
