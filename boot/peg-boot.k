;; peg-boot.k -- A subset of peg-grammar.g

;; Copyright (c) 2010 Ian Piumarta, Takashi Yamamiya
;; All Rights Reserved

;; Permission is hereby granted, free of charge, to any person obtaining a
;; copy of this software and associated documentation files (the 'Software'),
;; to deal in the Software without restriction, including without limitation
;; the rights to use, copy, modify, merge, publish, distribute, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, provided that the above copyright notice(s) and this
;; permission notice appear in all copies of the Software.  Inclusion of the
;; the above copyright notice(s) and this permission notice in supporting
;; documentation would be appreciated but is not required.

;; THE SOFTWARE IS PROVIDED 'AS IS'.  USE ENTIRELY AT YOUR OWN RISK.

(library peg

(define-rule syntax-error ()    (RETURN (error "syntax error near: " (UP-TO-END))))
(define-rule expression ()      ()) ;; forward

(define-rule blank	()	(CLASS " \t"))
(define-rule eol	()	(OR (AND (OBJ #"\n") (MANY (OBJ #"\r")))
				    (AND (OBJ #"\r") (MANY (OBJ #"\n")))))
(define-rule comment	()	(AND (CLASS ";") (MANY (AND (NOT (RULE eol)) (ANY)))))
(define-rule _		()	(MANY (OR (RULE blank) (RULE eol) (RULE comment))))

(define-rule EQUALS	()	(AND (OBJ #"=") (RULE _)))
(define-rule BAR	()	(AND (OBJ #"|") (RULE _)))
(define-rule AMPER	()	(AND (OBJ #"&") (RULE _)))
(define-rule PLING	()	(AND (OBJ #"!") (RULE _)))
(define-rule PLUS	()	(AND (OBJ #"+") (RULE _)))
(define-rule ASTERISK	()	(AND (OBJ #"*") (RULE _)))
(define-rule QUERY	()	(AND (OBJ #"?") (RULE _)))
(define-rule COLON	()	(AND (OBJ #":") (RULE _)))
(define-rule AT		()	(AND (OBJ #"@") (RULE _)))
(define-rule DOT	()	(AND (OBJ #".") (RULE _)))
(define-rule LPAREN	()	(AND (OBJ #"(") (RULE _)))
(define-rule RPAREN	()	(AND (OBJ #")") (RULE _)))
(define-rule LBRACK	()	(AND (OBJ #"[") (RULE _)))
(define-rule RBRACK	()	(AND (OBJ #"]") (RULE _)))
(define-rule LANGLE	()	(AND (OBJ #"<") (RULE _)))
(define-rule RANGLE	()	(AND (OBJ #">") (RULE _)))
(define-rule QUOTE	()	(AND (OBJ #"'") (RULE _)))
(define-rule TICK	()	(AND (OBJ #"`") (RULE _)))
(define-rule COMMAAT	()	(AND (SEQ [#"," #"@"]) (RULE _)))
(define-rule COMMA	()	(AND (OBJ #",") (RULE _)))
(define-rule QUOTEDBL	()	(AND (OBJ #"\"") (RULE _)))
(define-rule RARROW	()	(AND (SEQ [#"-" #">"]) (RULE _)))

(define-rule digit10	()	(CLASS "0123456789"))
(define-rule number	(n)	(AND (STORE n (NUMBER 10 (MANY1 (RULE digit10))))
				     (RULE _)
				     (RETURN n)))
(define-rule letter0	()	(CLASS "-ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz_"))
(define-rule letter1	()	(OR (RULE letter0) (RULE digit10)))
(define-rule identifier	(i)	(AND (STORE i (STRING (SOURCE (AND (RULE letter0) (MANY (RULE letter1))))))
				     (RULE _)
				     (RETURN (intern i))))

(define-rule escaped	()	(AND (OBJ #"\\")
				     (OR (AND (OBJ #"n")		(RETURN 10))
					 (AND (OBJ #"r")		(RETURN 13))
					 (AND (OBJ #"t")		(RETURN  9))
                                         (ANY))))

(define-rule character	()	(OR (RULE escaped) (ANY)))

(define-rule class	(s)	(AND (OBJ #"[")
				     (STORE s (STRING (MANY (AND (NOT (OBJ #"]")) (RULE character)))))
				     (RULE RBRACK)							(RETURN s)))

(define-rule string	(s)	(AND (OBJ #"\"")
				     (STORE s (STRING (MANY (AND (NOT (OBJ #"\"")) (RULE character)))))
				     (RULE QUOTEDBL)							(RETURN s)))

(define-rule symbol0	()	(CLASS "-!$%&*+/<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ\\^_abcdefghijklmnopqrstuvwxyz|~"))
(define-rule symbol1	()	(OR (RULE symbol0) (RULE digit10)))
(define-rule symbol	(s)	(AND (STORE s (STRING (SOURCE (AND (RULE symbol0) (MANY (RULE symbol1))))))
				     (RULE _)
				     (RETURN (intern s))))

(define-rule sexpr	(e)	(OR (RULE symbol)
				    (RULE string)
				    (RULE number)
				    (AND (RULE QUOTE) 	(STORE e (RULE sexpr))				(RETURN (list 'quote e)))
				    (AND (RULE TICK)  	(STORE e (RULE sexpr))				(RETURN (list 'quasiquote e)))
				    (AND (RULE COMMAAT) (STORE e (RULE sexpr))				(RETURN (list 'unquote-splicing e)))
				    (AND (RULE COMMA)   (STORE e (RULE sexpr))				(RETURN (list 'unquote e)))
				    (AND (RULE LPAREN)  (STORE e (MANY (RULE sexpr))) (RULE RPAREN)	(RETURN (->list e)))
				    ))

(define-rule literal	(e)	(AND (RULE QUOTE)
				     (OR (AND (RULE LPAREN) (STORE e (RULE expression)) (RULE RPAREN)	(RETURN `(STRUCT ,e)))
					 (AND (STORE e (RULE symbol))					(RETURN `(OBJ ',e))))))

(define-rule apply-arg	()	(AND (NOT (OBJ #">")) (OR (RULE identifier) (RULE sexpr))))

(define-rule apply-args	(a i)	(AND (MANY (RULE apply-arg))))

(define-rule apply-rule	(a i)	(AND (RULE LANGLE)
				     (STORE i (RULE symbol))
				     (STORE a (RULE apply-args))
				     (RULE RANGLE)							(RETURN `(RULE ,i ,@a))))

(define-rule primary	(id e)	(OR (AND (STORE e  (RULE string))					(RETURN (if (empty? e) 't
														  (if (singleton? e)
														      `(OBJ ,(first e))
														    `(SEQ ,(->array e))))))
				    (AND (STORE e  (RULE class))					(RETURN `(CLASS ,e)))
				    (AND (RULE DOT)							(RETURN '(ANY)))
				    (AND (RULE LPAREN) (STORE e (RULE expression)) (RULE RPAREN)	(RETURN e))
				    (AND (RULE RARROW) (STORE e (RULE sexpr))				(RETURN `(RETURN ,e)))
				    (RULE literal)
				    (RULE apply-rule)
				    (AND (STORE id (RULE identifier)) (NOT (OBJ #"="))			(RETURN `(RULE ,id)))
				    ))

(define-rule suffix	(p)	(AND (STORE p (RULE primary))
				     (OPT (OR (AND (RULE QUERY)		(STORE p (RETURN `(OPT   ,p))))
					      (AND (RULE PLUS)		(STORE p (RETURN `(MANY1 ,p))))
					      (AND (RULE ASTERISK)	(STORE p (RETURN `(MANY  ,p))))))
				     (RETURN p)))

(define-rule ranged	(s)	(AND (STORE s (RULE suffix))
				     (OPT (AND (RULE AT) (STORE s (RETURN `(SOURCE ,s)))))
				     (RETURN s)))

(define-rule stored	(i s)	(AND (STORE s (RULE ranged))
				     (OPT (AND (RULE COLON)
					       (STORE i (RULE identifier))
					       (STORE s (RETURN `(STORE ,i ,s)))))
				     (RETURN s)))

(define-rule prefix	(s)	(OR (AND (RULE PLING) (STORE s (RULE stored)) (RETURN `(NOT  ,s)))
				    (AND (RULE AMPER) (STORE s (RULE stored)) (RETURN `(PRED ,s)))
				    (RULE stored)))

(define-rule sequence	(s)	(AND (STORE s (MANY1 (RULE prefix)))
				     (RETURN (if (empty? s) 't
					       (if (singleton? s) (first s)
						 `(AND ,@s))))))

(define-rule expression	(tl hd)	(AND (STORE hd (RULE sequence))
				     (STORE tl (MANY (AND (RULE BAR) (RULE sequence))))
				     (RETURN (if (empty? tl) hd `(OR ,hd ,@tl)))))

(define-rule find-variables (i v)
				(AND (STORE v (ANY))
				     (STRUCT (OR (AND (OBJ 'STORE)
						      (STORE i (ANY))
						      (STORE v (RULE find-variables v (NEXT)))
						      (STORE v (RETURN (if (memq? i v) v (cons i v)))))
						 (OBJ 'RETURN)
						 (AND (ANY) (MANY (STORE v (RULE find-variables v (NEXT)))))
						 (RULE syntax-error)))
				     (RETURN v)))

(define-rule definition	(v e i)	(AND (RULE _)
				     (PRED (ANY))
				     (OR (AND (STORE i (RULE identifier)) (RULE EQUALS)
					      (STORE e (RULE expression))
					      (STORE v (RULE find-variables () e))
                                              (RETURN `(define-rule ,i ,v ,e)))
					 (RULE syntax-error))))

(define-rule definitions ()	(MANY (AND (RULE _)
                                           (RULE definition))))

)
