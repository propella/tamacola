;; -*- fundamental -*-

head             =  .
ident            = '( 'ident .:n )                                      -> n
range            = '( ('range-i | 'range):op trans:s trans:e )          -> `(list ,op ,s ,e)

make-do-list     = .:n .:range                                          -> `(,n ,(caddr range) (+ ,n 1))
                   
trans            = &(.:xxx -> (trace xxx)) '( 'compStmt trans*:ss )     -> `(begin ,@ss)
                 | '( 'def push-scope ident:f
                      '( 'args (ident:n <add-var-to-scope n>)*:a )
                       trans:s pop-scope:v)                             -> `(define ,f
                                                                             (lambda (,@a)
                                                                              (let ,(lset-difference v (->list a)) ,s)))
                 | '( ('cond -> '(cond):r)
                      '( trans:c trans*:ss -> `(,@r (,c ,@ss)):r)*)     -> r
                 | '( 'for '( ident:n <add-var-to-scope n>)
                            range:ran <make-do-list n ran>:l trans:s)   -> `(do (,l) (,(if (= (car ran) 'range-i)
                                                                                           `(> ,n ,(caddr (cdr ran)))
                                                                                           `(>= ,n ,(caddr (cdr ran)))))
                                                                                     ,s)
                 | '( 'while trans:c trans:s)                           -> `(while ,c ,s)
                 | '( 'ident ('true                                     -> '#t
                             |'false                                    -> '#f
                             |'nil                                      -> '()
                             |'self                                     -> #self
                             | .:n (&<var-in-scope n>                   -> n
                                   |&<ruby-isUppercase n>               -> n 
                                   | &(.)                               -> `(send #self ,n)
                                   )))
                 | '( 'numeric .:n)                                     -> n
                 | '( 'index trans:n trans:i)                           -> `(slot-get ,n ,i)
                 | '( 'assign ident:n trans:e <add-var-to-scope n>)     -> `(set! ,n ,e)
                 | '( 'assign '( 'index-lhs trans:l trans:i) trans:e )  -> `(slot-set! ,l ,i ,e)
                 | '( 'call ident:n . trans*:a)                         -> (if (= n 'print)
                                                                               `(trace ,@a)
                                                                               `(,n ,@a))
                 | '( 'send ident:sel trans:rec . trans*:a)             -> (if (= sel 'new)
                                                                               (let ((ll (->list a)))
                                                                                 (if (= (length ll) 2)
                                                                                     `(let ((ret (new ,rec ,(car ll)))
                                                                                            (v ,(cadr ll)))
                                                                                        (do ((i 0 (+ i 1)))
                                                                                          ((>= i ,(car ll)))
                                                                                          (slot-set! ret i v))
                                                                                        ret)
                                                                                     `(new ,rec ,@a)))
                                                                               `(send ,rec ,sel ,@a))
                 | '( head:t trans*:ts )                                -> `(,t ,@ts)
                 | .

start            = trans*:x                                             -> (->list x)

