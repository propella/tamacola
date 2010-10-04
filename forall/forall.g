;; -*- fundamental -*-

_              = [ \t\n\r]*
alpha          = [_abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ]
digit10        = [0123456789]

escaped        = "\\"     ( "e"        -> 27
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

character      = escaped | .
string         = "\"" _ (!"\"" character)*:s "\"" _                 -> `(string ,(->string s))
nsName         = (alpha (digit10 | alpha)*) @:x                     -> (->string x)
double         = (("+" | "-")? digit10+ "." digit10+)@:n _          -> (string->double (->string n))
integer        = digit10+ $#10:v _                                  -> v
scalar         = (double | integer):x                               -> `(scalar ,x)
identifier     = nsName:x _                                         -> `(ident ,(intern (->string x)))

identifier     = (nsName:x _ -> (intern x):x) !<forall-reservedq x> -> `(ident ,x)


ALL            = "%all"     _                                       -> 'all
SOME           = "%some"    _                                       -> 'some
SUCH           = "%such"    _                                       -> 'such
DOINORDER      = "do" _ "in" _ "order" _                            -> 'inorder
DO             = "do"       _                                       -> 'do
BY             = "by"       _
IN             = "in"       _
DOTDOTDOT      = "..."      _
DOTDOT         = ".."       _
IF             = "if"       _
THEN           = "then"     _
ELSE           = "else"     _
NEW            = "new"      _
FUNCTION       = "function" _
RETURN         = "return"   _
COMMA          = "," _


arg            = expr

args           = arg:a (COMMA args:b)?                              -> `(,a ,@b)
               | _                                                  -> '()

identifiers    = identifier:a (COMMA identifiers:b)?                -> `(,a ,@b)
               | _                                                  -> '()

arrayVar       = identifier:i ":" _ arrayPart:a                     -> `(,i ,a)
arrayVars      = arrayVar:a (COMMA arrayVars:b)?                    -> `(,a ,@b)
               | _                                                  -> '(() ())

arrayElement   = expr

dots           = DOTDOTDOT -> 'range-i | DOTDOT -> 'range

arrayPart      = arrayElement:x dots:key arrayElement:y BY scalar:s -> `(,key ,x ,y ,s)
               | arrayElement:x dots:key arrayElement:y             -> `(,key ,x ,y (scalar 1))
               | arrayElement

arrayParts     = arrayPart:a (COMMA arrayParts:b)?                  -> `(,a ,@b)
               | _                                                  -> '()

arrayDecl      = identifier:n "[" _ identifiers:i "]" _             -> `(array-decl ,n ,@i)

arrayDecls     = arrayDecl:a (COMMA arrayDecls:b)?                  -> `(,a ,@b)
               | _                                                  -> '()

query          = SUCH "{" _ expr:e "|" _ arrayDecl:n   "<" _ expr:s "}" _  -> `(such ,s ,e ,n)
               | ALL  "{" _              arrayDecls:n  "<" _ expr:s "}" _  -> `(all ,s ,(->list n))
               | SOME "{" _ expr:e "|" _ identifier+:n "<" _ expr:s "}" _  -> `(some ,s ,e ,(->list n))

primExpr       = scalar
               | string
               | "(" _ expr:e ")" _                                 -> e
               | FUNCTION "(" _ identifiers:l ")" _ compound:c      -> `(lambda ,(->list l) ,c)
               | NEW "[" _ args:l "]" _ -> `(new ,l)
               | "[" _ args:a "]" _                                 -> `(array ,@a) 
               | identifier

expr0          = expr0:r "." _ identifier:m "(" _ ")" _             -> `(send ,r ,m)
               | expr0:r "." _ identifier:m "(" _ args:a ")" _      -> `(send ,r ,m ,@a)
               | expr0:r "." _ identifier:m _                       -> `(prop ,r ,m)
               | expr0:r "(" _ args:a ")" _                         -> `(send null ,r ,@a)
               | expr0:r "[" _ arrayVars:ia "]" _                   -> `(index-v ,r ,@ia)
               | expr0:r "[" _ args:a "]" _                         -> `(index-e ,r ,a)
               | expr0:r "[" _ arrayPart:a "]" _                    -> `(index ,r ,a)
               | "+" _ expr0
               | "-" _ expr0:x                                      -> `(neg ,x)
               | primExpr

expr1          = expr1:x ("%%" -> 'modulo | "//" -> '/):op _ expr0:u                -> `(int (,op ,x ,u))
               | expr1:x ("*" -> '* | "/" -> '/ | "%" -> 'modulo):op _ expr0:u      -> `(,op ,x ,u)
               | expr0

expr2          = expr2:x ("+" -> '+ | "-" -> '-):op _ expr1:u                       -> `(,op ,x ,u)
               | expr1

expr3          = expr3:x (">=" -> '>= | ">" -> '> | "<=" -> '<= | "<" -> '<):op _ expr2:u -> `(,op ,x ,u)
               | expr2

expr4          = expr4:x ("!=" | "~=") _ expr3:u                         -> `(not (= ,x ,u))
               | expr4:x "=="          _ expr3:u                         -> `(= ,x ,u)
               | expr3

expr5          = expr5:x "&&" _ expr4:u                                  -> `(and ,x ,u)
	       | expr4

expr6          = expr6:x "||" _ expr5:u                                  -> `(or ,x ,u)
	       | expr5

expr7          = expr7:x "?" _ expr7:y ":" _ expr7:z                     -> `(if ,x ,y ,z)
               | expr6

expr           = expr7

semi           = ";" _ 

stmts          = (stmt:r -> `(,r):r) (semi stmt:a -> `(,@r ,a):r)* semi? -> r
               | _ semi?                                                 -> '()

compound       = "{" _ stmts:ss "}" _                                    -> `(compoundStmt ,@ss)

stmt           = IF expr:c THEN stmt:t (ELSE stmt:e)?                    -> `(if ,c ,t ,e)
               | expr:x "=" !("=") _ expr:v                              -> `(assign ,x ,v)
               | RETURN expr:e                                           -> `(return ,e)
               | compound
               | query:q (DO | DOINORDER):e compound:ss                  -> `(query ,e ,q ,ss)
               | expr

start          = _ stmts:s _                                             -> s
