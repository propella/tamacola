;; -*- fundamental -*-
;; SmallerTalk grammar
;; http://wiki.squeak.org:8080/squeak/uploads/172/standard_v1_9-indexed.pdf

dig            = [01234567890]
num            = dig+ :n __                    -> (string->number (->string n))

space          = [ \t\r\n]
cmt            = "\"" (!"\"" .)* "\""               -> 'COMMENT
__             = (space | cmt)*
					
letter         = [ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz_]
ident     = letter:x (letter | dig)* :xs -> (list->string `(,x ,@xs))

string-body    = (!"'" . | "''" )*:xs -> (list->string (->list xs))
quoted-string  = "'" string-body:x "'" __ -> x

sym            = "#" ident:x __ -> `(quote ,(string->symbol x))
reserved       = "true" __ -> '#t
               | "false" __ -> '#f
               | "self" __ -> '#self
variable       = ident:x __ -> (string->symbol x)

unit           = num
               | reserved
               | variable
               | sym
               | quoted-string
               | block
               | "(" __ stmt :x ")" __         -> x

unary          = unit:x unary-rest*:ms          -> (fold-left
                                                     (lambda (r m) `(send ,r ,(string->symbol m)))
                                                     x
                                                     (->list ms))
unary-rest     = ident:m !":" __            -> m

binary         = unary:x ( binary-rest+ :ms	-> (fold-left
                                                      (lambda (r m) `(,(car m) ,r ,(cadr m)))
                                                      x
                                                      (->list ms))
                         | 			-> x ) 

binop          = [!%&*+,/<=>?@\~|-]
binary-rest    = binop:op __ unary:x           	-> `(,(string->symbol (char->string op)) ,x)

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

keyword-part   = ident:m ":" __ binary:x      -> (cons m x)

assign         = ident:v __ ":=" __ stmt:x  -> `(set! ,(string->symbol v) ,x)

args           = (":" variable)+:xs "|" __ -> (->list xs)
               | -> ()

tmps           = "|" __ variable*:xs "|" __ -> (->list xs)
               | -> ()

stmts          = stmt:x ("." __ stmt)*:xs -> `(,x ,@xs)
               | -> ()

block          = "[" __ args:as tmps:ts stmts:es "]" __ -> `(lambda ,as (let ,ts ,@es))

stmt           = assign | keyword

_stmts         = __ stmts
