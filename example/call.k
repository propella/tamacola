(define func1
  (lambda (word1 word2)
    (print word1)
    (print word2)))

(define func2
  (lambda ()
    (print "Function call with no argument: ok.")))

(func1 "Function call: " "ok.")
(func2)
