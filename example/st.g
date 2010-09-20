;; -*- fundamental -*-
;; SmallerTalk grammar
;; http://wiki.squeak.org:8080/squeak/uploads/172/standard_v1_9-indexed.pdf

digit          = [01234567890]
binop          = [!%&*+,/<=>?@\~|-]
whitespace     = [ \t\r\n]
number         = digit+ :n _                    -> (string->number (->string n))

comment        = "\"" (!"\"" .)* "\""               -> 'COMMENT
_              = (whitespace | comment)*
					
letter         = [ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz_]
identifier     = letter:x (letter | digit)* :xs           	-> (list->string `(,x ,@xs))

character      = .
string-body    = (!"'" character | "''" )*:xs -> (list->string (->list xs))
quoted-string  = "'" string-body:x "'" _ -> x

symbol         = "#" identifier:x _ -> `(quote ,(string->symbol x))
reference      = identifier:x _ -> (string->symbol x)
reserved       = "true" -> '#t
               | "false" -> '#f
               | "self" -> '#self

unit           = number
               | reserved
               | reference
               | symbol
               | quoted-string
               | block
               | "(" _ expr :x ")" _         -> x

unary          = unit:x unary-rest*:ms          -> (fold-left
                                                     (lambda (r m) `(send ,r ,(string->symbol m)))
                                                     x
                                                     (->list ms))
unary-rest     = identifier:m !":" _            -> m

binary         = unary:x ( binary-rest+ :ms	-> (fold-left
                                                      (lambda (r m) `(,(car m) ,r ,(cadr m)))
                                                      x
                                                      (->list ms))
                         | 			-> x ) 

binary-rest    = binop:op _ unary:x           	-> `(,(string->symbol (char->string op)) ,x)

keyword        = binary:x ( keyword-rest:m        -> `(send ,x ,(string->symbol (car m)) ,@(cdr m))
                        | 			-> x )

keyword-rest   = keyword-part+:xs               -> (fold-right
                                                     (lambda (e result)
                                                       (let ((word (+ (car e) ":"))
                                                             (arg  (cdr e)))
                                                         (cons (+ word (car result))
                                                               (cons arg (cdr result)))))
                                                     (cons "" ())
                                                     (->list xs))

keyword-part   = identifier:m ":" _ binary:x      -> (cons m x)

assign         = identifier:v _ ":=" _ keyword:x  -> `(set! ,(string->symbol v) ,x)

args           = (":" _ identifier:x _ -> (string->symbol x))+:xs "|" _ -> (->list xs)
               | -> ()

tmps           = "|" _ (identifier:x _ -> (string->symbol x))*:xs "|" _ -> (->list xs)
               | -> ()

exprs          = expr:x ("." _ expr)*:xs -> `(,x ,@xs)
               | -> ()

block          = "[" _ args:as tmps:ts exprs:es "]" _ -> `(lambda ,as (let ,ts ,@es))

expr           = assign | keyword

_exprs         = _ exprs
