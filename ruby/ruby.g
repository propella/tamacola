;; -*- fundamental -*-

_              = [ \t\r\n]*
__             = [ \t]*
alpha          = [_abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ]
digit10        = [0123456789]

escaped        = "\\" ("e"        -> 27
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
string         = _ ("\"" (!"\"" character)*:s "\""
                    |"\'" (!"\"" character)*:s "\'"
                    |"\`" (!"\"" character)*:s "\`")                  -> `(string ,(->string s))

T              = (_ ";" | __ "\r" | __ "\n" | __ "\r\n")
DEF            = _ "def"
IF             = _ "if"
DO             = T _ "do"   | _ "do"   | T
THEN           = T _ "then" | _ "then" | T
ELSE           = _ "else"
ELSIF          = _ "elsif"
WHILE          = _ "while"
FOR            = _ "for"
IN             = _ "in"
CLASS          = _ "class"
END            = _ "end"

nsName         = (alpha (digit10 | alpha)*) @:x                 -> (->string x)
double         = (("+" | "-")? digit10+ "." digit10+)@:n        -> (string->double (->string n))
integer        = integer:n (digit10:c -> (- c #"0")):d          -> (+ (* n 10) d)
               | integer:n "_"                                  -> n
               | digit10:c                                      -> (- c #"0")

numeric        = _ (double | integer):x                         -> `(numeric ,x)
identifier     = _ (("defined?" | nsName):x -> (intern x):x) !<ruby-reservedq x>       -> `(ident ,x)

varName        = ("$" identifier | ("@" identifier) | identifier)@:x               -> `(ident ,(intern (->string x)))

variable        = _ (varName | "self" -> '(ident self))
symbol          = _ ":" (fname | varName):x                                        -> `(symbol ,(intern x))
literal         = numeric | symbol | string
                | _ ("true" -> '(ident true) | "false" -> '(ident false) | "nil"   -> '(ident null))

fname           = identifier
                | _ (".." | "|" | "^" | "&" | "<=>" | "==" | "===" | "=~" | ">>"
                     | "<<" | ">=" | ">" | "<=" | "<" | "+" | "-" | "*" | "/"
                     | "%" | "**" | "~" | "+@" | "-@" | "[]" | "[]="):op           -> `(ident ,(intern (->string op)))

primExpr        = _ "::" identifier
                | _ "return" _ call-args:a                                         -> `(return ,@a)
                | _ "yield"  _ call-args                                           -> `(yield ,@a)
                | _ "defined?" _ "(" arg ")"
                | _ "(" expr:a _ ")"                                               -> a
                | (IF expr:a THEN comp-stmt:b -> `(cond (,a ,b)):r)
                              (ELSIF expr:a THEN comp-stmt:b -> `(,@r (,a ,b)):r)*
                              (ELSE comp-stmt:b -> `(,@r (#t ,b)):r)? END          -> r
                | WHILE expr:a DO comp-stmt:b END                                  -> `(while ,a ,b)
                | FOR lhs+:l IN expr:r DO comp-stmt:s END                          -> `(for ,(->list l) ,r ,s)
                | CLASS identifier _ ("<" identifier)? comp-stmt END
                | DEF fname:n argdecl:d comp-stmt:s END                            -> `(def ,n ,d ,s)
                | _ "super" (_ "(" call-args:a ")")?                   (fblock:v)? -> `(super ,v ,@a)
                | primExpr:r _ "." operation:s _ "(" call-args:a _ ")" (fblock:v)? -> `(send ,s ,r ,v ,@a)
                | primExpr:r _ "." operation:s                         (fblock:v)? -> `(send ,r ,s ,v)
                | operation:a _ "(" call-args:b _ ")"                  (fblock:v)? -> `(call ,a ,v ,@b)
                | primExpr _ "::" identifier
                | literal
                | variable

fblock          = _ "{" ( _ "|" block-vars:v _ "|")? comp-stmt:s _ "}"             -> `(block ,v ,s)

expr1           = expr1:a _ "[" args:b _ "]"                                       -> `(index ,a ,@b)
                | primExpr

expr2           = _ ("+" -> 'identity | "-" -> 'neg
                     | "!" -> 'not | "~" -> 'bitNot):op expr2:a                    -> `(,op ,a)
                | expr1

expr3           = expr3:a _ "**" expr2:b                                           -> `(exp ,a ,b)
                | expr2

expr4           = expr4:a _ ("*" -> '* | "/" -> '/ | "%" -> '%):op expr3:b         -> `(,op ,a ,b)
                | expr3

expr5           = expr5:a _ ("+" -> '+ | "-" -> '-):op expr4:b                     -> `(,op ,a ,b)
                | expr4

expr6           = expr6:a _ ("<<" -> '<<| ">>" -> '>>):op expr5:b                  -> `(,op ,a ,b)
                | expr5

expr7           = expr7:a _ "&" expr6:b                                            -> `(logAnd ,a ,b)
                | expr6

expr8           = expr8:a _ ("|" -> 'logOR | "^" -> 'logXor):op expr7:b            -> `(,op ,a ,b)
                | expr7

expr9           = expr9:a _ (">=" -> '>= | ">" -> '>
                             | "<=" -> '<= | "<" -> '<):op expr8:b                 -> `(,op ,a ,b)
                | expr8

opExpr10        = "<=>" -> 'comp | "==" -> 'eq | "===" -> 'strict-eq
                | "!=" -> 'not-eq | "=~" -> 'match | "!~" -> 'not-match

expr10          = expr10:a _ opExpr10:op expr9:b                                   -> `(,op ,a ,b)
                | expr9

expr11          = expr11:a _ "&&" expr10:b                                         -> `(and ,a ,b)
                | expr10

expr12          = expr12:a _ "||" expr11:b                                         -> `(or ,a ,b)
                | expr11

expr13          = expr13:a _ ("..." -> 'range-i | ".." -> 'range):op expr12:b      -> `(,op ,a ,b)
                | expr12

expr14          = expr14:a _ "?" expr14:b _ ":" expr14:c                           -> `(if ,a ,b ,c)
                | expr13

expr15          = expr15:a _ "and" expr14:b                                        -> `(and ,a ,b)
                | expr14

expr16          = expr16:a _ "or" expr15:b                                         -> `(or ,a ,b)
                | expr15

expr17          = lhs:r _ "=" expr17:l                                             -> `(assign ,r ,l)
                | expr16

arg             = expr17

argdecl         = _ "(" arglist:a _ ")"                                            -> a
                | arglist:a T                                                      -> a

arglist         = (identifier:a -> `(,a):a) (_ "," identifier:b -> `(,@a ,b):a)*   -> `(args ,@a)

operation       = _ ("defined?" @ :x | (nsName ("!" | "?"))@ :x | nsName @:x)      -> `(ident ,(intern (->string x)))


lhs             = primExpr:a _ "[" arg:b _ "]"                                     -> `(index-lhs ,a ,b)
                | primExpr:a _ "." identifier:b                                    -> `(prop-lhs ,a ,b)
                | variable

call-args       = args

args           = (arg:a -> `(,a):a) (_ "," arg:b -> `(,@a ,b):a)*                  -> a
                | _                                                                -> '()

expr            = arg
stmt            = expr

stmts           = (stmt:a -> `(,a):a) (T stmt:b -> `(,@a ,b):a)* T?                -> a
                | _                                                                -> '()

comp-stmt       = stmts:a                                                          -> `(compStmt ,@a)

block-vars      = lhs

program         = comp-stmt

