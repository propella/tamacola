;; -*- fundamental -*-

__               = [ \t\n\r]*
lalpha          = [_abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ]
ldigit10          = [0123456789]

lescaped        = "\\"     ( "e"        -> 27
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

lcharacter      = lescaped | .
lstring         = "\"" __ (!"\"" lcharacter)*:s "\"" __                 -> `(string ,(->string s))
lnsName         = (lalpha (ldigit10 | lalpha)*) @:x                     -> (->string x)
ldouble          = (("+" | "-")? ldigit10+ "." ldigit10+)@:n __         -> (string->double (->string n))
linteger         = ldigit10+ $#10:v __                                  -> v
lscalar         = (ldouble | linteger):x                                -> `(scalar ,x)
lidentifier     = lnsName:x __                                          -> `(ident ,(intern (->string x)))

lidentifier     = (lnsName:x __ -> (intern x):x) !<forall-reservedq x>  -> `(ident ,x)


lALL            = "%all"    __                                          -> 'all
lSOME           = "%some"   __                                          -> 'some
lSUCH           = "%such"   __                                          -> 'such
lDOINORDER      = "do" __ "in" __ "order" __                            -> 'inorder
lDO             = "do"      __                                          -> 'do
lBY             = "by"      __
lIN             = "in"      __
lDOTDOTDOT      = "..."     __
lDOTDOT         = ".."      __
lIF             = "if"      __
lTHEN           = "then"    __
lELSE           = "else"    __
lNEW            = "new"     __
lFUNCTION       = "function" __
lRETURN         = "return"  __
lCOMMA          = "," __


larg            = lexpr

largs           = larg:a (lCOMMA largs:b)?                              -> `(,a ,@b)
                | __                                                    -> '()

lidentifiers    = lidentifier:a (lCOMMA lidentifiers:b)?                -> `(,a ,@b)
                | __                                                    -> '()

larrayVar       = lidentifier:i ":" __ larrayPart:a                     -> `(,i ,a)
larrayVars      = larrayVar:a (lCOMMA larrayVars:b)?                    -> `(,a ,@b)
                | __                                                    -> '(() ())

larrayElement   = lexpr

ldots           = lDOTDOTDOT -> 'range-i | lDOTDOT -> 'range

larrayPart      = larrayElement:x ldots:key larrayElement:y lBY lscalar:s -> `(,key ,x ,y ,s)
                | larrayElement:x ldots:key larrayElement:y               -> `(,key ,x ,y (scalar 1))
                | larrayElement

larrayParts     = larrayPart:a (lCOMMA larrayParts:b)?                  -> `(,a ,@b)
                | __                                                    -> '()

larrayDecl      = lidentifier:n "[" __ lidentifiers:i "]" __ -> `(array-decl ,n ,@i)

larrayDecls     = larrayDecl:a (lCOMMA larrayDecls:b)?                  -> `(,a ,@b)
                | __                                                    -> '()

lquery          = lSUCH "{" __ lexpr:e "|" __ larrayDecl:n "<" __ lexpr:s "}" __                -> `(such ,s ,e ,n)
                | lALL  "{" __                larrayDecls:n "<" __ lexpr:s "}" __               -> `(all ,s ,(->list n))
                | lSOME "{" __ lexpr:e "|" __ lidentifier+:n "<" __ lexpr:s "}" __              -> `(some ,s ,e ,(->list n))

lprimExpr       = lscalar
                | lstring
                | "(" __ lexpr:e ")" __                                   -> e
                | lFUNCTION (__ -> (trace "fff")) "(" __ lidentifiers:l ")" (__ -> (trace "ggg")) __ lcompound:c (__ -> (trace "hhh"))     -> `(lambda ,(->list l) ,c)
                | lNEW "[" __ largs:l "]" __ -> `(new ,l)
                | "[" __ largs:a "]" __                                   -> `(array ,@a) 
                | lidentifier

lexpr0          = lexpr0:r "." __ lidentifier:m "(" __ ")" __           -> `(send ,r ,m)
                | lexpr0:r "." __ lidentifier:m "(" __ largs:a ")" __   -> `(send ,r ,m ,@a)
                | lexpr0:r "." __ lidentifier:m __                      -> `(prop ,r ,m)
                | lexpr0:r "(" __ largs:a ")" __                        -> `(send null ,r ,@a)
                | lexpr0:r "[" __ larrayVars:ia "]" __                  -> `(index-v ,r ,@ia)
                | lexpr0:r "[" __ largs:a "]" __                        -> `(index-e ,r ,a)
                | lexpr0:r "[" __ larrayPart:a "]" __                   -> `(index ,r ,a)
                | "+" __ lexpr0
                | "-" __ lexpr0:x                                       -> `(neg ,x)
                | lprimExpr

lexpr1          = lexpr1:x ("%%" -> 'modulo | "//" -> '/):op __ lexpr0:u                -> `(int (,op ,x ,u))
                | lexpr1:x ("*" -> '* | "/" -> '/ | "%" -> 'modulo):op __ lexpr0:u      -> `(,op ,x ,u)
                | lexpr0

lexpr2          = lexpr2:x ("+" -> '+ | "-" -> '-):op __ lexpr1:u                       -> `(,op ,x ,u)
                | lexpr1

lexpr3          = lexpr3:x (">=" -> '>= | ">" -> '> | "<=" -> '<= | "<" -> '<):op __ lexpr2:u -> `(,op ,x ,u)
                | lexpr2

lexpr4          = lexpr4:x ("!=" | "~=") __ lexpr3:u                         -> `(not (= ,x ,u))
                | lexpr4:x "=="          __ lexpr3:u                         -> `(= ,x ,u)
                | lexpr3

lexpr5          = lexpr4:x ("&&" __ lexpr4:u -> `(and ,x ,u):x)* -> x

lexpr6          = lexpr5:x ("||" __ lexpr5:u -> `(or ,x ,u):x)* -> x

lexpr7          = lexpr7:x "?" __ lexpr7:y ":" __ lexpr7:z -> `(if ,x ,y ,z)
                | lexpr6

lexpr           = lexpr7

lsemi           = ";" __ 

lstmts          = (lstmt:r -> `(,r):r) (lsemi lstmt:a -> `(,@r ,a):r)* lsemi?           -> r
		| __ lsemi?    	       	      	      	       	        -> '()

lcompound       = "{" __ lstmts:ss "}" __                               -> `(compoundStmt ,@ss)

lstmt           = lIF lexpr:c lTHEN lstmt:t (lELSE lstmt:e)?            -> `(if ,c ,t ,e)
                | lexpr:x "=" !("=") __ lexpr:v                         -> `(assign ,x ,v)
                | lRETURN lexpr:e    					-> `(return ,e)
                | lcompound
                | lquery:q (lDO | lDOINORDER):e lcompound:ss            -> `(query ,e ,q ,ss)
                | lexpr

lstart          = __ lstmts:s __						-> s
