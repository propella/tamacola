; -*- fundamental -*-

expr = . ; forward

multiname = is-symbol :x                        -> (new-multiname *parser* x) 
slotname  = is-symbol :x                        -> (slot-multiname *parser* x)

arity   = .*:x                                  -> (length (->list x)) 

exprs   = expr* :xs                             -> (concatenate (->list xs)) 

array_exprs = '( exprs:xs )                     -> xs 

body-expr = expr:x                              -> `((pop) ,@x) 

body    = expr:x body-expr*:xs                  -> `(,@x ,@(concatenate (->list xs)))
        |                                       -> '((pushundefined)) 

malform = .:k .:xs                              -> (error "malformed expression:" `(,k ,@xs))

define-global = .:n .:xs                        -> (save-slot *parser* n)
                                                -> `((findproperty ,n)
                                                    ,@xs
                                                    (setproperty ,n)
                                                    (pushundefined))  ; Define returns undefined

make-lambda = .:n .:xs                          -> (env-frame-top *parser*) :frame
                                                -> (enable-activation *parser* frame (->list xs)) :xs
                                                -> (frame-signature n frame) :sig
                                                -> (convert-local (->list xs)) :xs
                                                -> `(,sig (code (,@xs (returnvalue)))
                                                     (trait ())
                                                     (exception ,(load-exceptions *parser*))) :m
                                                -> (frame-pop *parser*)
                                                -> (save-lambda *parser* m) :id

params = '( is-symbol*:xs )                     -> (frame-push *parser* (new-frame () () ()))
                                                -> (push-vars *parser* (->list xs))  ; local variables must saved before the body.

lambda_ = 'lambda params body :xs               <make-lambda "" xs>:id
                                                -> `((newfunction (method ,id)))
        | 'lambda :k .*:xs                      <malform k xs> 

define_lambda = 'define slotname:n '( 'lambda params body :xs )
                                                -> (cadr n):name
                                                   <make-lambda name xs>:id
                                                   <define-global n `((newfunction (method ,id)))>

binding = '( is-symbol:n expr:e )               -> (cons n e) ; initial values are evaluated in outer env (before push-vars).
        | '( is-symbol:n )                      -> (cons n '((pushnull)))
        | is-symbol:n                           -> (cons n '((pushnull))) 

bindings = binding*:bs                          -> (push-vars *parser* (map car (->list bs)))
                                                -> (make-bindings *parser* (->list bs))

let_    = 'let '( bindings ):bs  body:xs        -> (env-frame-top *parser*) :frame
                                                -> (enable-activation *parser* frame xs) :xs
                                                -> (block-pop *parser*)
                                                -> `(,@bs ,@xs)
        | 'let :k .*:xs                         <malform k xs> 

script = body :xs                               -> (push-vars *parser* ()) ;; todo: maybe push-var is needed before body is compiled
                                                -> (env-frame-top *parser*) :frame
                                                -> (frame-signature "" frame) :sig
                                                -> (convert-local (->list xs)) :xs
                                                -> (new-slots
                                                     (map (lambda (n) (slot-multiname *parser* n))
                                                          (frame-slots frame))) :slots
                                                -> `(,sig
                                                    (code ((getlocal 0) (pushscope) ,@xs (returnvalue)))
                                                    (trait ,slots)
                                                    (exception ,(load-exceptions *parser*)))

class-const = '( 'lambda params body :xs )     <make-lambda "" xs>
            | script:scr                        -> (save-lambda *parser* scr)

class_  = 'class slotname:n '( multiname:s ) '( slotname*:ms ) class-const:sid
                                                -> (new-class *parser* n s (->list ms) sid) :id
                                                -> `((getglobalscope)
                                                    (getlex ,s)
                                                    (dup)
                                                    (pushscope)
                                                    (newclass ,id)
                                                    (popscope)
                                                    (initproperty ,n)
                                                    (pushundefined))

;       | 'class:k .*:xs                        <malform k xs> 

expr3   = define_lambda
        | lambda_
        | let_
        | class_

        | 'define slotname:n expr:e             <define-global n e>
        | 'define :k .*:xs                      <malform k xs>
        | 'quote .:x                            -> (q-object x):q
                                                <expr q>
	| 'quasiquote .:x                       -> (qq-object x):q
                                                <expr q>
        | 'if expr:t expr:x expr:y              -> (new-label) :a
                                                -> (new-label) :b
                                                -> `(,@t (iffalse ,a) ,@x (coerce_a) (jump ,b) ,a ,@y (coerce_a) ,b)
        | 'if :k .*:xs                          <malform k xs>
        | 'while expr:t body:xs                 -> (new-label) :a
                                                -> (new-label) :b
                                                -> `((jump ,b)
                                                    ,a
                                                    (label)
                                                    ,@xs
                                                    (pop)
                                                    ,b
                                                    ,@t
                                                    (iftrue ,a)
                                                    (pushundefined))
        | 'set! is-symbol:n expr:xs             -> (make-setter *parser* n):insts
                                                -> `(,@xs ,@insts (pushundefined))
        | 'set! :k .*:xs                        <malform k xs>
        | 'define-macro is-symbol:n .:f         -> (meta-define-macro *parser* n f)
        | 'define-macro :k .*:xs                <malform k xs>
        | 'define-pattern .*:f                  -> (meta-define-pattern *parser* (->list f))
	| 'define-pattern :k .*:xs		<malform k xs>
        | 'assemble .*:xs                       -> (->list xs)

        | '==      expr:x expr:y                -> `(,@x ,@y (equals))
        | '===      expr:x expr:y               -> `(,@x ,@y (strictequals))
        | '<      expr:x expr:y                 -> `(,@x ,@y (lessthan))
        | '<=      expr:x expr:y                -> `(,@x ,@y (lessequals))
        | '>      expr:x expr:y                 -> `(,@x ,@y (greaterthan))
        | '>=      expr:x expr:y                -> `(,@x ,@y (greaterequals))
        | '+      expr:x expr:y                 -> `(,@x ,@y (add))
        | '-      expr:x expr:y                 -> `(,@x ,@y (subtract))
        | '*      expr:x expr:y                 -> `(,@x ,@y (multiply))
        | '/      expr:x expr:y                 -> `(,@x ,@y (divide))
        | 'modulo      expr:x expr:y            -> `(,@x ,@y (modulo))
        | '&      expr:x expr:y                 -> `(,@x ,@y (bitand))
        | '|      expr:x expr:y                 -> `(,@x ,@y (bitor))
        | '<<      expr:x expr:y                -> `(,@x ,@y (lshift))
        | '>>      expr:x expr:y                -> `(,@x ,@y (rshift))
        | 'not    expr:x                        -> `(,@x (not))

        | 'instanceof expr:x expr:t             -> `(,@x ,@t (instanceof))
        | 'istype expr:x multiname:t            -> `(,@x (istype ,t))
        | 'typeof expr:x                        -> `(,@x (typeof))
        | 'constructsuper &arity:n exprs:xs     -> `((getlocal 0) ,@xs (constructsuper ,n) (pushundefined))
        | 'send expr:o multiname:m &arity:n exprs: xs 
                                                -> `(,@o ,@xs (callproperty ,m ,n))

        | 'slot-getq  expr:o multiname:s        -> `(,@o (getproperty ,s))
        | 'slot-get   expr:o expr:k             -> `(,@o ,@k (getproperty (multinamel (ns_set 1))))
        | 'slot-setq! expr:o multiname:s expr:x -> `(,@o ,@x (setproperty ,s) (pushundefined))
        | 'slot-set!  expr:o expr:k      expr:x -> `(,@o ,@k ,@x
                                                    (setproperty (multinamel (ns_set 1)))
                                                    (pushundefined))
        | 'new expr:c &arity:n exprs: xs        -> `(,@c ,@xs (construct ,n))
        | 'applytype expr:c &arity:n exprs: xs  -> `(,@c ,@xs (applytype ,n))

        | 'try-catch multiname:n multiname:type expr:try expr:catch
                                                -> (new-label) :l1
                                                -> (new-label) :l2
                                                -> (new-label) :l3
                                                -> (new-label) :l4
                                                -> `((from ,l1) (to ,l2) (target ,l3)
                                                     (exc_type ,type) (var_name ,n)) :ex
                                                -> (exception-push *parser* ex) :i
                                                -> `(,l1
                                                     ,@try
                                                     ,l2
                                                     (jump ,l4)
                                                     ,l3
                                                     (newcatch ,i)
                                                     (dup)
                                                     (pushscope)
                                                     (swap)
                                                     (setproperty ,n)
                                                     ,@catch
                                                     (popscope)
                                                     ,l4)
        | 'try-catch :k .*:xs                   <malform k xs>
        | 'throw expr:x                         -> `(,@x (throw))
        | 'undefined? multiname:n               -> `((findproperty ,n)
                                                     (getproperty ,n)
                                                     (pushundefined)
                                                     (strictequals))
        | 'return expr:x                        -> `(,@x (returnvalue))
        | 'import is-symbol:x                   -> (push-import *parser* x)
                                                -> '((pushundefined))
        | 'in-imports is-string*:xs             -> (push-nsset *parser* (->list xs))
                                                -> '((pushundefined))
        | 'in-ns is-string:x                    -> (set-compiler-ns *parser* x)
                                                -> '((pushundefined))

        | expr:f &arity:n exprs:a               -> `(,@f (getlocal 0) ,@a (call ,n))

expr2   = '( expr3:xs )                         -> xs
        | '#self                                -> '((getlocal 0))
        | '#undefined                           -> '((pushundefined))
        | '#t                                   -> '((pushtrue))
        | '#f                                   -> '((pushfalse))
        | is-null                               -> '((pushnull))
        | is-long:x                             -> `((pushint ,x))
        | is-double:x                           -> `((pushdouble ,x))
        | is-string:x                           -> `((pushstring ,x))
        | is-array:as                           -> (array->list as):xs
                                                -> (array-length as):n
                                                   <array_exprs xs>:es
                                                -> `(,@es (newarray ,n))
        | is-symbol:x                           -> (make-getter *parser* x)

        | .:x                                   -> (error "unrecognised expression: " x) 

expr    = .:x                                   -> (meta-macroexpand x) :x
                                                <expr2 x> 

start = script:x                                -> (load-lambda *parser*) :ls
                                                -> (load-instances *parser*) :instances
                                                -> (load-classes *parser*) :classes
                                                -> (load-global-slots *parser*) :slots
                                                -> (load-ns-set *parser*) :sets
                                                -> `(asm
                                                    (ns_set ,sets)
                                                    (method (,x ,@ls))
                                                    (instance ,instances)
                                                    (class ,classes)
                                                    (script (((init (method 0)) (trait ,slots))))) 
