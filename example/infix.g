;; -*- fundamental -*-
;; An example of infix operator

digit          = [01234567890]
_              = [ \t]*
number         = digit+ :n _                -> (string->number (->string n))

value          = number
               | #"(" _ sum :x #")" _       -> x

product-rest   = #"*" _ value :x            -> `(* ,x)
               | #"/" _ value :x            -> `(/ ,x)
product        = value :x product-rest* :xs <make-left x (->list xs)>

sum-rest       = #"+" _ product :x          -> `(+ ,x)
               | #"-" _ product :x          -> `(- ,x)
sum            = product :x sum-rest* :xs   <make-left x (->list xs)>

expr           = sum
               | !.                         -> (error "parse error")

;; Make a left associative expression from a sequence of command like
;; 1 and ((+ 2) (- 3) ..) to (- (+ 1 2) 3)

make-left      = .:x is-null                -> x
               | .:x '('( .:op .:y) .*:xs)  <make-left `(,op ,x ,y) (->list xs)>
