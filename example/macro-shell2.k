(print (add "Hello, " "World!"))
(after (print "*Pattern Macro does NOT work*") (print "Pattern Macro works"))

(print (cat "Hello, " "World!"))

(define-form after (a b) b)
(after (print "*Form Macro does NOT work*") (print "Form Macro works"))
