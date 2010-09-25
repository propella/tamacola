;;;; A mini lisp compiler

;; Lexical Parser

spaces  = [ \t\r\n]*                    -> 'SPACES
                                        
digit   = [0123456789]
number  = digit+ :n spaces              -> (string->number (->string n))

char    = [+-*/abcdefghijklmnopqrstuvwxyz]
symbol  = char+ :s spaces               -> (intern (->string s))
        
sexp    = symbol
        | number
        | "(" sexp*:e ")"               -> (->list e)
                                        
;; Compiler

arity   = .*:x                          -> (length (->list x))
insts   = inst* :xs                     -> (concatenate (->list xs)) 
                                        
inst    = is-number:x                   -> `((pushint ,x))
        | is-symbol:x                   -> `((getlex ((ns "") ,(symbol->string x))))
        | '( '+ inst:x inst:y )         -> `(,@x ,@y (add))
        | '( '- inst:x inst:y )         -> `(,@x ,@y (subtract))
        | '( '* inst:x inst:y )         -> `(,@x ,@y (multiply))
        | '( '/ inst:x inst:y )         -> `(,@x ,@y (divide))
        | '( inst:f &arity:n insts:a )  -> `(,@f (pushnull) ,@a (call ,n))

program = inst:x  -> `(asm
                       (method (((signature
                                  ((return_type *) (param_type ()) (name "program")
                                   (flags 0) (options ()) (param_names ())))
                                 (code ((getlocal 0)
                                        (pushscope)
                                        ,@x
                                       (returnvalue))))))
                      (script (((init (method 0)) (trait ())))))

;; Driver

execute = sexp:x <program x>
