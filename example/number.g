;; number.g -- A number parser

;; Use string->number

digit   = [0123456789]
; number  = digit+
number  = digit+:n               -> (string->number (->string n))

;; Use fold-left

digit1   = [0123456789]:d        -> (- d 48)
number1  = digit1:x digit1*:xs   -> (fold-left
                                      (lambda (n d) (+ (* n 10) d))
                                      x
                                      (->list xs))

;; Use left-recursion

digit2   = [0123456789]:d        -> (- d 48)
number2  = number2:n digit2:d    -> (+ (* n 10) d)
         | digit2
number2s = number2
