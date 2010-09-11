;; -*- fundamental -*-
;; An example of infix operator

idigit          = [01234567890]
___               = [ \t]*
inumber         = idigit+ :n ___                -> (string->number (->string n))

isum            = .                             ; forward

ivalue          = inumber
                | #"(" ___ isum :x #")" ___     -> x

iproduct-rest   = #"*" ___ ivalue :x            -> `(* ,x)
                | #"/" ___ ivalue :x            -> `(/ ,x)
iproduct        = ivalue :x iproduct-rest* :xs  -> (make-left x (->list xs))

isum-rest       = #"+" ___ iproduct :x          -> `(+ ,x)
                | #"-" ___ iproduct :x          -> `(- ,x)
isum            = iproduct :x isum-rest* :xs    -> (make-left x (->list xs))

iexpr           = isum
                | !.                            -> (error "parse error")
