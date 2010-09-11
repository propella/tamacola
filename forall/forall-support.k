(define forall-reserved-words
  '(if then else new function return))

(define forall-reservedq
  (lambda (*stream* *parser*)
     (let ((sym (next *stream*)))
       (if (memq? sym forall-reserved-words)
 	  (begin
 	    (set-parser-result *parser* sym) #t)
 	  #f))))
