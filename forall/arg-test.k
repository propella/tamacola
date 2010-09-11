;;;; Infix grammar test

(define forall-test
  (lambda (sym str result)
    (trace str)
    (check (parse-collection sym str) => result)))

(pretty-print (parse-stream tstart (->stream (cons '(compoundStmt (compoundStmt (ident c)) (decl b) (ident a) (ident b)) '()))))


