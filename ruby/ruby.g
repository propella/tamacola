;; -*- fundamental -*-

__              = [ \t\r\n]*
___             = [ \t]*
ralpha          = [_abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ]
rdigit10        = [0123456789]

rescaped        = "\\"     ( "e"        -> 27
                           | "a"        ->  7
                           | "b"        ->  8
                           | "f"        -> 12
                           | "n"        -> 10
                           | "r"        -> 13
                           | "t"        ->  9
                           | "v"        -> 11
                           | "'"        -> 39
                           | "\""       -> 34
                           | "\\"       -> 92
                           )

rcharacter      = rescaped | .
rstring         = __ ("\"" (!"\"" rcharacter)*:s "\""
		     |"\'" (!"\"" rcharacter)*:s "\'"
		     |"\`" (!"\"" rcharacter)*:s "\`")                  -> `(string ,(->string s))

rT		= (__ ";" | ___ "\r" | ___ "\n" | ___ "\r\n")
rDEF		= __ "def"
rIF		= __ "if"
rDO		= rT __ "do"   | __ "do"   | rT
rTHEN		= rT __ "then" | __ "then" | rT
;rTHEN		= __ "then"
rELSE		= __ "else"
rELSIF		= __ "elsif"
rWHILE		= __ "while"
rFOR		= __ "for"
rIN		= __ "in"
rCLASS		= __ "class"
rEND		= __ "end"

rnsName         = (ralpha (rdigit10 | ralpha)*) @:x			-> (->string x)
rdouble         = (("+" | "-")? rdigit10+ "." rdigit10+)@:n         	-> (string->double (->string n))
rinteger        = rinteger:n (rdigit10:c -> (- c #"0")):d		-> (+ (* n 10) d)
		| rinteger:n "_"	     	  			-> n
		| rdigit10:c 						-> (- c #"0")

rnumeric        = __ (rdouble | rinteger):x						  -> `(numeric ,x)
ridentifier     = __ (("defined?" | rnsName):x -> (intern x):x) !<ruby-reservedq x>       -> `(ident ,x)

rvarName	= ("$" ridentifier | ("@" ridentifier) | ridentifier)@:x 	 	  -> `(ident ,(intern (->string x)))

rvariable	= __ (rvarName | "self" -> '(ident self))
rsymbol		= __ ":" (rfname | rvarName):x						  -> `(symbol ,(intern x))
rliteral	= rnumeric | rsymbol | rstring
		| __ ("true" -> '(ident true) | "false" -> '(ident false) | "nil"	  -> '(ident null))

rfname		= ridentifier
		| __ (".." | "|" | "^" | "&" | "<=>" | "==" | "===" | "=~" | ">>"
		     | "<<" | ">=" | ">" | "<=" | "<" | "+" | "-" | "*" | "/"
		     | "%" | "**" | "~" | "+@" | "-@" | "[]" | "[]="):op		  -> `(ident ,(intern (->string op)))

rPrimExpr	= __ "::" ridentifier
		| __ "return" __ rcall-args:a						  -> `(return ,@a)
		| __ "yield" __ rcall-args						  -> `(yield ,@a)
		| __ "defined?" __ "(" rarg ")"
		| __ "(" rexpr:a __ ")"							  -> a
		| (rIF rexpr:a rTHEN rcomp-stmt:b -> `(cond (,a ,b)):r)
                              (rELSIF rexpr:a rTHEN rcomp-stmt:b -> `(,@r (,a ,b)):r)*
                              (rELSE rcomp-stmt:b -> `(,@r (#t ,b)):r)? rEND              -> r
		| rWHILE rexpr:a rDO rcomp-stmt:b rEND   	      			  -> `(while ,a ,b)
		| rFOR rlhs+:l rIN rexpr:r rDO rcomp-stmt:s rEND			  -> `(for ,(->list l) ,r ,s)
		| rCLASS ridentifier __ ("<" ridentifier)? rcomp-stmt rEND
		| rDEF rfname:n rargdecl:d rcomp-stmt:s rEND				  -> `(def ,n ,d ,s)
		| __ "super" (__ "(" rcall-args:a ")")?			     (rfblock:v)? -> `(super ,v ,@a)
		| rPrimExpr:r __ "." roperation:s __ "(" rcall-args:a __ ")" (rfblock:v)? -> `(send ,s ,r ,v ,@a)
		| rPrimExpr:r __ "." roperation:s      	  	       	     (rfblock:v)? -> `(send ,r ,s ,v)
		| roperation:a __ "(" rcall-args:b __ ")"	    	     (rfblock:v)? -> `(call ,a ,v ,@b)
		| rPrimExpr __ "::" ridentifier
		| rliteral
		| rvariable

rfblock		= __ "{" ( __ "|" rblock-vars:v __ "|")? rcomp-stmt:s __ "}"		  -> `(block ,v ,s)

rexpr1		= rexpr1:a __ "[" rargs:b __ "]"					  -> `(index ,a ,@b)
		| rPrimExpr

rexpr2		= __ ("+" -> 'identity | "-" -> 'neg
		     | "!" -> 'not | "~" -> 'bitNot):op rexpr2:a			  -> `(,op ,a)
		| rexpr1

rexpr3		= rexpr3:a __ "**" rexpr2:b						  -> `(exp ,a ,b)
		| rexpr2

rexpr4		= rexpr4:a __ ("*" -> '* | "/" -> '/ | "%" -> '%):op rexpr3:b	          -> `(,op ,a ,b)
		| rexpr3

rexpr5		= rexpr5:a __ ("+" -> '+ | "-" -> '-):op rexpr4:b			  -> `(,op ,a ,b)
		| rexpr4

rexpr6		= rexpr6:a __ ("<<" -> '<<| ">>" -> '>>):op rexpr5:b			  -> `(,op ,a ,b)
		| rexpr5

rexpr7		= rexpr7:a __ "&" rexpr6:b					          -> `(logAnd ,a ,b)
		| rexpr6

rexpr8		= rexpr8:a __ ("|" -> 'logOR| "^" -> 'logXor):op rexpr7:b		  -> `(,op ,a ,b)
		| rexpr7

rexpr9		= rexpr9:a __ (">=" -> '>= | ">" -> '>
		  	      | "<=" -> '<= | "<" -> '<):op rexpr8:b			  -> `(,op ,a ,b)
		| rexpr8

rOpExpr10	= "<=>" -> 'comp | "==" -> 'eq | "===" -> 'strict-eq
		| "!=" -> 'not-eq | "=~" -> 'match | "!~" -> 'not-match

rexpr10		= rexpr10:a __ rOpExpr10:op rexpr9:b					  -> `(,op ,a ,b)
		| rexpr9

rexpr11		= rexpr11:a __ "&&" rexpr10:b						  -> `(and ,a ,b)
		| rexpr10

rexpr12		= rexpr12:a __ "||" rexpr11:b						  -> `(or ,a ,b)
		| rexpr11

rexpr13		= rexpr13:a __ ("..." -> 'range-i | ".." -> 'range):op rexpr12:b	  -> `(,op ,a ,b)
		| rexpr12

rexpr14		= rexpr14:a __ "?" rexpr14:b __ ":" rexpr14:c				  -> `(if ,a ,b ,c)
		| rexpr13

rexpr15		= rexpr15:a __ "and" rexpr14:b						  -> `(and ,a ,b)
		| rexpr14

rexpr16		= rexpr16:a __ "or" rexpr15:b						  -> `(or ,a ,b)
		| rexpr15

rexpr17		= rlhs:r __ "=" rexpr17:l						  -> `(assign ,r ,l)
		| rexpr16

rarg		= rexpr17

rargdecl	= __ "(" rarglist:a __ ")"						  -> a
		| rarglist:a rT	       			 				  -> a

rarglist	= (ridentifier:a -> `(,a):a) (__ "," ridentifier:b -> `(,@a ,b):a)*	  -> `(args ,@a)

roperation 	= __ ("defined?" @ :x | (rnsName ("!" | "?"))@ :x | rnsName @:x)   	  -> `(ident ,(intern (->string x)))


rlhs		= rPrimExpr:a __ "[" rarg:b __ "]"			    		  -> `(index-lhs ,a ,b)
		| rPrimExpr:a __ "." ridentifier:b		        		  -> `(prop-lhs ,a ,b)
		| rvariable

rcall-args	= rargs

rargs           = (rarg:a -> `(,a):a) (__ "," rarg:b -> `(,@a ,b):a)*			  -> a
                | __                                                    		  -> '()

rexpr		= rarg
rstmt		= rexpr

rstmts		= (rstmt:a -> `(,a):a) (rT rstmt:b -> `(,@a ,b):a)* rT?			  -> a
		| __ 	      	       			    	    			  -> '()

rcomp-stmt	= rstmts:a								  -> `(compStmt ,@a)

rblock-vars	= rlhs

rprogram	= rcomp-stmt

