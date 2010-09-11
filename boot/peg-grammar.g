;; -*- fundamental -*-
;; peg-grammar.g -- PEG Grammar

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

syntax-error	= -> (error "syntax error near: " (UP-TO-END))
blank		= [ \t]
eol		= "\n" "\r"* | "\r" "\n"*
comment		= ";" (!eol .)*
_		= (blank | eol | comment)*

EQUALS		= "="		_
BAR		= "|"		_
AMPER		= "&"		_
PLING		= "!"		_
PLUS		= "+"		_
ASTERISK	= "*"		_
QUERY		= "?"		_
COLON		= ":"		_
DOLLARHASH	= "$#"	_
DOLLAR		= "$"		_
AT		= "@"		_
DOT		= "."		_
LPAREN		= "("		_
RPAREN		= ")"		_
LBRACK		= "["		_
RBRACK		= "]"		_
LANGLE		= "<"		_
RANGLE		= ">"		_
QUOTE		= "'"		_
TICK		= "`"		_
COMMAAT		= ",@"	_
COMMA		= ","		_
QUOTEDBL	= "\""	_
RARROW		= "->"	_

digit8		= [01234567]
digit10		= [0123456789]
digit16		= [0123456789abcdefABCDEF]
sign            = [+-]

double          = (sign? digit10+ "." digit10+)@:n _ -> (string->double (->string n))

number		= "#x" sign?:s digit16+:xs _	-> (string->number-radix-sign (->string xs) 16 (->string s))
                | "0x" sign?:s digit16+:xs _	-> (string->number-radix-sign (->string xs) 16 (->string s))
                | sign?:s digit10+:xs _		-> (string->number-radix-sign (->string xs) 10 (->string s))

letter0		= [-ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz_]
letter1		= letter0 | digit10
identifier	= (letter0 letter1*) @:xs _	-> (intern (->string xs))

escaped		= "\\"	   ( "e"	-> 27
			   | "a"	->  7
			   | "b"	->  8
			   | "f"	-> 12
			   | "n"	-> 10
			   | "r"	-> 13
			   | "t"	->  9
			   | "v"	-> 11
			   | "'"	-> 39
			   | "\""	-> 34
			   | "\\"	-> 92
			   | "x" (digit16 digit16) @:xs 		-> (string->number-radix (->string xs) 16)
			   | "u" (digit16 digit16 digit16 digit16) @:xs	-> (string->number-radix (->string xs) 16)
			   | (digit8 digit8 digit8) @:xs 		-> (string->number-radix (->string xs) 8)
			   )
character	= escaped | .

character-literal = "#\"" character:c "\"" _ -> c
class		= "[" (!"]" character)* :s RBRACK -> (->string s)
string		= "\"" (!"\"" character)* :s QUOTEDBL -> (->string s)

symbol0		= [-!$#%&*+/<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ\\^_abcdefghijklmnopqrstuvwxyz|~]
symbol1		= [-!$#%&*+/<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ\\^_abcdefghijklmnopqrstuvwxyz|~0123456789.]
symbol		= ( symbol0 symbol1* )@:s _ -> (intern (->string s))

sexpr		= double
                | number
		| string
                | character-literal
		| symbol
		| QUOTE   sexpr:e			-> (list 'quote e)
		| TICK    sexpr:e			-> (list 'quasiquote e)
		| COMMAAT sexpr:e			-> (list 'unquote-splicing e)
		| COMMA   sexpr:e			-> (list 'unquote e)
		| LBRACK  sexpr*:e RBRACK		-> (->array e)
		| LPAREN  sexpr*:e RPAREN		-> (->list e)
                | LPAREN  sexpr*:e DOT sexpr:es RPAREN	-> (concat (->list e) es)

sexprs          = sexpr*
_sexpr          = _ sexpr
_sexprs         = _ sexprs !.
                | syntax-error

expression      = . ;; forward

literal		= QUOTE ( LPAREN expression:e RPAREN	-> `(STRUCT ,e)
			| symbol:e			-> `(OBJ ',e)
			)

apply-arg	= !">" (identifier | sexpr)
apply-args      = apply-arg*
apply-rule	= LANGLE symbol:i apply-args:a RANGLE	-> `(RULE ,i ,@a)

primary		= string:e				-> (if (empty? e)
                                                             't
                                                             (if (singleton? e)
                                                               `(OBJ ,(first e))
                                                               `(SEQ ,(->array e))))
		| character-literal:e                   -> `(OBJ ,e)
		| class:e				-> `(CLASS ,e)
		| DOT					-> '(ANY)
		| LPAREN expression:e RPAREN		-> e
		| RARROW sexpr:e			-> `(RETURN ,e)
		| literal
		| apply-rule
		| identifier:id	!"="			-> `(RULE ,id)

suffix		= primary:p ( QUERY			-> `(OPT   ,p) :p
			    | PLUS			-> `(MANY1 ,p) :p
			    | ASTERISK			-> `(MANY  ,p) :p
			    )?				-> p

ranged		= suffix:s ( AT -> `(SOURCE ,s) :s )?	-> s

stringed	= ranged:s ( DOLLARHASH number:n -> `(NUMBER ,n ,s) :s
			   | DOLLAR -> `(STRING ,s) :s
			   )?					-> s

stored		= stringed:s ( COLON identifier:i -> `(STORE ,i ,s) :s )? -> s

prefix		= PLING stored:s -> `(NOT  ,s)
		| AMPER stored:s -> `(PRED ,s)
		|       stored
sequence	= prefix+:s -> (if (empty? s) 't (if (singleton? s) (first s) `(AND ,@s)))

expression	= sequence:hd (BAR sequence)*:tl -> (if (empty? tl) hd `(OR ,hd ,@tl))

find-variables	= .:v '( 'STORE .:i <find-variables v (NEXT)>:v -> (if (memq? i v) v (cons i v)):v
		       | 'RETURN
                       | . (<find-variables v (NEXT)>:v)*
		       | syntax-error
		       )				-> v

definition	= _ &. ( identifier:i EQUALS expression:e <find-variables '() e>:v -> `(define-rule ,i ,v ,e)
		       | syntax-error
		       ) ;

definitions     = definition*
