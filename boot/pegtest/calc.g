digit = [01234567890] ;

_ = [ \t]* ;

number = digit+ $#10 :n _ -> n ;

sum = .; # forward

value = number
      | "(" _ sum :x ")" _ -> x ;

product-rest = "*" _ value :x -> `(mul ,x)
             | "/" _ value :x -> `(div ,x) ;
product = value :x product-rest* :xs -> (make-left x (->list xs)) ;

sum-rest = "+" _ product :x -> `(plus ,x)
         | "-" _ product :x -> `(minus ,x) ;
sum = product :x sum-rest* :xs -> (make-left x (->list xs)) ;

expr = sum
     | !. -> (exit 0) ;
