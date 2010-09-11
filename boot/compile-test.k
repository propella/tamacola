;; This test script tests ABC compiler with PEG (abc-compile.k and abc-syntax.g)

(require "boot/check.k")
(require "boot/compile.k")

(define test1 (lambda ()

;;;; Construct multiname

(define ctx (compiler))

(check (new-multiname ctx (string->symbol "flash.display/Sprite")) => '((package "flash.display") "Sprite"))
(check (new-multiname ctx (string->symbol "display/Sprite")) => '((package "display") "Sprite"))
(check (new-multiname ctx 'Sprite) => '((ns_set 1) "Sprite"))

(set-compiler-ns ctx "user")

(check (slot-multiname ctx (string->symbol "display/Sprite")) => '((package "display") "Sprite"))
(check (slot-multiname ctx 'Sprite) => '((ns "user") "Sprite"))

;;;; New label

(check (new-label) => 'L1)
(check (new-label) => 'L2)
(check (new-label) => 'L3)

;;;; Compile simple expression

(check (parse-expr syntax/expr2 '(Object)) => '((getlex ((ns_set 1) "Object"))))
(check (parse-expr syntax/expr '(100)) => '((pushint 100)))
(check (parse-expr syntax/expr '(1.1)) => '((pushdouble 1.1)))
(check (parse-expr syntax/expr '("key")) => '((pushstring "key")))
(check (parse-expr syntax/expr '(())) => '((pushnull)))
(check (->list (parse-expr syntax/expr '((assemble (pushint 1))))) => '((pushint 1)))
(check (->list (parse-expr syntax/expr '((assemble (pushint 1) (pushint 2))))) => '((pushint 1) (pushint 2)))
(check (->list (parse-expr syntax/expr '((func (assemble (pushint 7))))))
       => '((getlex ((ns_set 1) "func"))
            (getlocal 0)
            (pushint 7)
            (call 1)))

(check (parse-expr syntax/expr (list (array))) => '((newarray 0)))
(check (parse-expr syntax/expr (list (array 1 2 3))) => '((pushint 1) (pushint 2) (pushint 3) (newarray 3)))

(check (parse-expr syntax/exprs '(1 2.1 "3")) => '((pushint 1)
                                                (pushdouble 2.1)
                                                (pushstring "3")))

(check (parse-expr syntax/body '(1 2.1 "3")) => '((pushint 1) (pop) (pushdouble 2.1) (pop) (pushstring "3")))
(check (parse-expr syntax/body '(1)) => '((pushint 1)))
(check (parse-expr syntax/body '()) => '((pushundefined)))

;;;; Compile slot accessors

(check (parse-expr syntax/expr '((slot-get Object "key")))
       => '((getlex ((ns_set 1) "Object"))
	    (pushstring "key")
	    (getproperty (multinamel (ns_set 1)))))

(check (parse-expr syntax/expr '((slot-set! Object "key" "value")))
       => '((getlex ((ns_set 1) "Object"))
	    (pushstring "key")
	    (pushstring "value")
	    (setproperty (multinamel (ns_set 1)))
	    (pushundefined)))

(check (parse-expr syntax/expr '((slot-getq Object key)))
       => '((getlex ((ns_set 1) "Object"))
	    (getproperty ((ns_set 1) "key"))))

(check (parse-expr syntax/expr '((slot-setq! Object key "value")))
       => '((getlex ((ns_set 1) "Object"))
	    (pushstring "value")
	    (setproperty ((ns_set 1) "key"))
	    (pushundefined)))

(check (parse-expr syntax/expr '((quote 1)))
       => '((pushint 1)))

(check (parse-expr syntax/expr '((quote hello)))
       => '((getlex ((ns_set 1) "intern"))
            (getlocal 0)
            (pushstring "hello")
            (call 1)))

(check (parse-expr syntax/expr '((quote (1 2))))
       => (parse-expr syntax/expr '((cons 1 (cons 2 ())))))

(check (parse-expr syntax/expr '((quote (1 . 2))))
       => (parse-expr syntax/expr '((cons 1 2))))

;;;; Compile local_0

(check (parse-expr syntax/expr '((slot-getq #self key)))
       => '((getlocal 0)
	    (getproperty ((ns_set 1) "key"))))

;;;; Save and load functions

(define ctx (compiler))
(check (save-lambda ctx '*func1*) => 1)
(check (save-lambda ctx '*func2*) => 2)
(check (save-lambda ctx '*func3*) => 3)
(check (load-lambda ctx) => '(*func1* *func2* *func3*))

;;;; Compile time environment
(define ctx (compiler))
(check (compiler-env ctx) => '((() () ())))

(frame-push ctx (new-frame () () ()))
(frame-push ctx (new-frame () () ()))
(check (compiler-env ctx) => '((() () ()) (() () ()) (() () ())))
(frame-pop ctx)
(frame-pop ctx)
(check (compiler-env ctx) => '((() () ())))

;; compile time environment accessors
(set-compiler-env ctx '((((v5 v4) (v4)) ((v3 v2 v1) (v2))) *parent-env*))
(check (env-frame-top ctx) => '(((v5 v4) (v4)) ((v3 v2 v1) (v2))))

(define ctx (compiler))

(block-push ctx (new-block '(c b a) '(b)))
(check (compiler-env ctx) => '(((((c b a) (b))) () ())))

(push-vars ctx '(a b))
(check (compiler-env ctx) => '(((((b a) ()) ((c b a) (b))) () ())))

(define ctx (compiler))
(parse-expr2 ctx syntax/params '((a b c)))
(check (compiler-env ctx) => '(((((c b a) ())) () ()) (() () ())))

(parse-expr2 ctx syntax/params '(()))
(check (compiler-env ctx) => '((((() ())) () ()) ((((c b a) ())) () ()) (() () ())))

;; Mark variables free

(check (block-mark-free '((v3 v2 v1) ()) 'v1) => '((v3 v2 v1) (v1)))
(check (block-mark-free '((v3 v2 v1) (v1)) 'v1) => '((v3 v2 v1) (v1)))
(check (block-mark-free '((v3 v2 v1) (v1)) 'v0) => #f)

(check (*frame-mark-free '(((v3) ())   ((v2) ())   ((v2 v1) ())) 'v2)
                     =>  '(((v3) ())   ((v2) (v2)) ((v2 v1) ()))    )

(check (frame-mark-free '((((v3) ())   ((v2) ())   ((v2 v1) ())) ()   *ex*) 'v3)
                     => '((((v3) (v3)) ((v2) ())   ((v2 v1) ())) (v3) *ex*)    )
(check (frame-mark-free '((((v3) ())   ((v2) ())   ((v2 v1) ())) ()   *ex*) 'v2)
                     => '((((v3) ())   ((v2) (v2)) ((v2 v1) ())) (v2) *ex*)    )
(check (frame-mark-free '((((v3) ())   ((v2) (v2)) ((v2 v1) ())) (v2) *ex*) 'v2)
                     => '((((v3) ())   ((v2) (v2)) ((v2 v1) ())) (v2) *ex*)    )
(check (frame-mark-free '((((v3) ())   ((v2) ())   ((v2 v1) ())) ())        'v0)
                     => '())

;; Variable reference

(check (block-refer '((v3 v2 v1) (v2 v1)) 'v3) => '(local 3))
(check (block-refer '((v3 v2 v1) (v2 v1)) 'v2) => '(local 2))
(check (block-refer '((v3 v2 v1) (v2 v1)) 'v0) => #f)

(check (blocks-depth '()) => 0)
(check (blocks-depth '(((v5 v4) (v5)) ((v3 v2 v1) (v2 v1)))) => 5)
(check (blocks-depth '(((v3 v2 v1) (v2 v1)))) => 3)

(check (frame-refer '((((v5 v4) (v5)) ((v3 v2 v1) (v2 v1))) ()) 'v4) => '(local 4))
(check (frame-refer '((((v5 v4) (v5)) ((v3 v2 v1) (v2 v1))) ()) 'v3) => '(local 3))
(check (frame-refer '((((v5 v4) (v5)) ((v3 v2 v1) (v2 v1))) ()) 'v5) => '(local 5))
(check (frame-refer '((((v5 v4) (v5)) ((v3 v2 v1) (v2 v1))) ()) 'v0) => '(free))

(check (copy-activation '((((v4 v3) (v4 v3))) ()))
       => '((pushstring "v4") (*getlocal v4 2)
            (pushstring "v3") (*getlocal v3 1)
            (newobject 2)
            (pushwith)))

;; signature

(check (signature "f" 3)
       => '(signature ((return_type *) (param_type (* * *)) (name "f")
		       (flags 0) (options ()) (param_names ()))))

(check (frame-signature "f" '((((v1 v2) ())) ()))
       => '(signature ((return_type *) (param_type (* *)) (name "f")
		       (flags 0) (options ()) (param_names ()))))

(check (frame-signature "f" '((((v1 v2) (v1))) (v1)))
       => '(signature ((return_type *) (param_type (* *)) (name "f")
		       (flags 0) (options ()) (param_names ()))))

;;;; Compile get expression

(set-compiler-env ctx '(((((v4 v3) ())) () *ex*) ((((v2 v1) ())) () *ex*)) )

(check (make-getter ctx 'v1) => '((getlex ((ns_set 1) "v1"))))
(check (compiler-env ctx) => '(((((v4 v3) ())) () *ex*) ((((v2 v1) (v1))) (v1) *ex*)))

(check (make-getter ctx 'v3) => '((*getlocal v3 1)))
(check (compiler-env ctx) => '(((((v4 v3) ())) () *ex*) ((((v2 v1) (v1))) (v1) *ex*)))

))
(define test2 (lambda ()


;;;; Make setter expression

(define ctx (compiler))

(check (make-setter-local 'v 7) => '((*setlocal v 7)))

(check (make-setter-property ctx 'v)
       => '((coerce_a)
            (findpropstrict ((ns_set 1) "v"))
            (swap)
            (setproperty ((ns_set 1) "v"))))

(set-compiler-env ctx '(((((v4 v3) ())) () *ex*) ((((v2 v1) ())) () *ex*)) )

(check (make-setter ctx 'v4 '(*something*)) => '((*setlocal v4 2)))

(check (make-setter ctx 'v2 '(*something*))
       => '((coerce_a)
            (findpropstrict ((ns_set 1) "v2"))
            (swap)
	    (setproperty ((ns_set 1) "v2"))))

(check (compiler-env ctx) => '(((((v4 v3) ())) () *ex*) ((((v2 v1) (v2))) (v2) *ex*)))

(check (convert-activation ctx '((((v2 v1) (v2))) ())
			   '((*setlocal v1 1)
                             (*setlocal v2 2)))
       => '((*setlocal v1 1)
            (coerce_a)
	    (findpropstrict ((ns_set 1) "v2"))
	    (swap)
	    (setproperty ((ns_set 1) "v2"))))

(check (convert-local
	'((*setlocal 'b 2)))
       => '((coerce_a)
	    (setlocal 2)))

;;;; Convert accessors

(check (inst-varname '(*getlocal a 1) '(b a)) => 'a)
(check (inst-varname '(*getlocal c 1) '(b a)) => #f)

;;;; Compile let expression

(define ctx (compiler))

(set-compiler-env ctx '(((((c b a) ())) () *ex*)))

(check (make-bindings ctx '()) => '())
(check (make-bindings ctx '((a . (inst)))) => '(inst (*setlocal a 1)))

(check (make-bindings ctx '((a . (instA))
                            (b . (instB0 instB1))
                            (c . (instC0 instC1))))
       => '(instA instB0 instB1 instC0 instC1
                  (*setlocal c 3) (*setlocal b 2) (*setlocal a 1)))

(check (parse-expr syntax/binding '((a 7))) => '(a (pushint 7)))
(check (parse-expr syntax/binding '((a))) => '(a (pushnull)))

(define ctx (compiler))

(check (parse-expr2 ctx syntax/bindings '((a 7))) => '((pushint 7)
                                                    (*setlocal a 1)))

(check (parse-expr2 ctx syntax/bindings '((a a) (b 77))) => '((*getlocal a 1)
                                                      (pushint 77)
                                                      (*setlocal b 3)
                                                      (*setlocal a 2)))

(check (parse-expr2 ctx syntax/bindings '(a b c)) => '((pushnull)
                                               (pushnull)
                                               (pushnull)
                                               (*setlocal c 6)
                                               (*setlocal b 5)
                                               (*setlocal a 4)))

(define ctx (compiler))

(check (parse-expr syntax/expr '((let ((a 7) (b 8)) a)))
       => '((pushint 7) (pushint 8) (*setlocal b 2) (*setlocal a 1) (*getlocal a 1)))

(check (convert-local (parse-expr syntax/expr '((let ((a 7) (b 8)) a))))
       => '((pushint 7) (pushint 8) (coerce_a) (setlocal 2) (coerce_a) (setlocal 1) (getlocal 1)))

(check (convert-local (parse-expr syntax/expr '((let ((a 7))
							(let ((a 8)) a)))))
       => '((pushint 7) (coerce_a) (setlocal 1) (pushint 8) (coerce_a) (setlocal 2) (getlocal 2)))

;; Reuse same variable name in let expression

(check (parse-expr syntax/expr '((let ((a 7)) 
                                (let ((a a)) a))))
       => '((pushint 7) (*setlocal a 1)
            (*getlocal a 1) (*setlocal a 2)
            (*getlocal a 2)))

;; let in bindings

(check (parse-expr syntax/expr '((let ((a (let ((x 1)) x))
                                    (b (let ((x 2)) x)))
                                b)))
       => '((pushint 1) (*setlocal x 1) (*getlocal x 1)
            (pushint 2) (*setlocal x 1) (*getlocal x 1)
            (*setlocal b 2)
            (*setlocal a 1)
            (*getlocal b 2)))

;;;; Compile set expression

(define ctx (compiler))
(check (parse-expr syntax/expr '((set! g 7)))
       => '((pushint 7)
            (coerce_a)
            (findpropstrict ((ns_set 1) "g"))
            (swap)
            (setproperty ((ns_set 1) "g"))
            (pushundefined)))

;;;; Compile lambda expression

;; Compile simplest function

(define ctx (compiler))

(check (parse-expr2 ctx syntax/expr '((lambda () 7)))
       => '((newfunction (method 1))))

(check (load-lambda ctx)
       => '(((signature ((return_type *) (param_type ()) (name "") (flags 0) (options ()) (param_names ())))
	     (code ((pushint 7)
		    (returnvalue)))
	     (trait ())
             (exception ()))))

;; Compile function

(define ctx (compiler))

(check (parse-expr2 ctx syntax/expr '((lambda (a b) b)))
       => '((newfunction (method 1))))
(check (load-lambda ctx)
       => '(((signature ((return_type *) (param_type (* *)) (name "") (flags 0) (options ()) (param_names ())))
	     (code ((getlocal 2) (returnvalue)))
	     (trait ())
             (exception ()))))

;;;; Compile closure

;; Closure in let (assignment of a := 7 is duplicated now)
(define ctx (compiler))

(check (parse-expr2 ctx syntax/expr '((let ((a 7)) (lambda () a))))
       => '((pushint 7)
            (*setlocal a 1)
            (pushstring "a")
            (*getlocal a 1)
            (newobject 1)
            (pushwith)
            (newfunction (method 1))
            (popscope)))

(check (load-lambda ctx)
       => '(((signature ((return_type *) (param_type ()) (name "") (flags 0) (options ()) (param_names ())))
	     (code ((getlex ((ns_set 1) "a")) (returnvalue)))
	     (trait ())
             (exception ()))))

;; Closure with set

(define ctx (compiler))

(check (parse-expr2 ctx syntax/expr '((lambda (f) (set! f 7) (lambda () f)))) 
       => '((newfunction (method 2))))

(check (cadr (load-lambda ctx)) => '((signature ((return_type *) (param_type (*)) (name "") (flags 0) (options ()) (param_names ())))
  (code
   ((pushstring "f")
    (getlocal 1)
    (newobject 1)
    (pushwith)
    (pushint 7)
    (coerce_a)
    (findpropstrict ((ns_set 1) "f"))
    (swap)
    (setproperty ((ns_set 1) "f"))
    (pushundefined)
    (pop)
    (newfunction (method 1))
    (popscope)
    (returnvalue)))
  (trait ())
  (exception ())))

;; slot definition

(check (new-slots '()) => '())

(check (new-slots '(((package "") "foo")))
       => '(((kind slot)
	     (name ((package "") "foo"))
	     (slot_id 0) (type_name *) (vindex 0) (vkind 0) (metadata ()))))

(check (new-slots '(((package "") "foo") ((package "") "bar")))
       => '(((kind slot)
	     (name ((package "") "foo"))
	     (slot_id 0) (type_name *) (vindex 0) (vkind 0) (metadata ()))
	    ((kind slot)
	     (name ((package "") "bar"))
	     (slot_id 0) (type_name *) (vindex 0) (vkind 0) (metadata ()))))

(save-slot ctx '((package "") "foo"))
(save-slot ctx '((package "") "bar"))
(save-slot ctx '((package "") "foo"))
(check (compiler-slotnames ctx) => '(((package "") "bar") ((package "") "foo")))

;; Function application arity

(check (parse-expr syntax/arity '()) => 0)
(check (parse-expr syntax/arity '(a b c)) => 3)

(define ctx (compiler))

(check (parse-expr2 ctx syntax/expr '((lambda (a) (lambda (b) a))))
       => '((newfunction (method 2))))

(check (assq 'code (car (load-lambda ctx)))
       => '(code ((getlex ((ns_set 1) "a")) (returnvalue))))

(check (assq 'code (cadr (load-lambda ctx)))
       => '(code ((pushstring "a")
		  (getlocal 1)
                  (newobject 1)
		  (pushwith)
		  (newfunction (method 1))
                  (popscope)
		  (returnvalue)
		  )))

;;; Body

(check (parse-expr syntax/body '()) => '((pushundefined)))
(check (parse-expr syntax/body '(first))
       => '((getlex ((ns_set 1) "first"))))
(check (parse-expr syntax/body '(first second))
       => '((getlex ((ns_set 1) "first"))
	    (pop)
	    (getlex ((ns_set 1) "second"))))
(check (parse-expr syntax/body '(first second third))
       => '((getlex ((ns_set 1) "first"))
	    (pop)
	    (getlex ((ns_set 1) "second"))
	    (pop)
	    (getlex ((ns_set 1) "third"))))

;;;; Script

(define ctx (compiler))
(check (parse-expr2 ctx syntax/script '())
       => '((signature ((return_type *) (param_type ()) (name "") (flags 0) (options ()) (param_names ())))
	    (code ((getlocal 0) (pushscope) (pushundefined) (returnvalue)))
	    (trait ())
            (exception ())))

(define ctx (compiler))
(check (parse-expr2 ctx syntax/script '((constructsuper)))
       => '((signature ((return_type *) (param_type ()) (name "") (flags 0) (options ()) (param_names ())))
	    (code ((getlocal 0)
		   (pushscope)
		   (getlocal 0)
		   (constructsuper 0)
		   (pushundefined)
		   (returnvalue)))
	    (trait ())
            (exception ())))

;; Closure in let in the initial script

(define ctx (compiler))
(check (parse-expr2 ctx syntax/script '((let ((a 7)) (lambda () a))))
       => '((signature ((return_type *) (param_type ()) (name "") (flags 0) (options ()) (param_names ())))
	    (code ((getlocal 0)
		   (pushscope)
		   (pushint 7)
		   (coerce_a)
		   (setlocal 1)
                   (pushstring "a")
		   (getlocal 1)
                   (newobject 1)
		   (pushwith)
		   (newfunction (method 1))
                   (popscope)
		   (returnvalue)))
	    (trait (((kind slot) (name ((ns "") "a")) (slot_id 0)
		     (type_name *) (vindex 0) (vkind 0) (metadata ()))))
            (exception ())))

;;; Class

(check (parse-expr2 ctx syntax/expr '((class Foo (Bar) (first second))))
       => '((getglobalscope)		; this
	    (getlex ((ns_set 1) "Bar"))	; this, Bar
	    (dup)			; this, Bar, Bar
	    (pushscope)			; this, Bar
	    (newclass 0)		; this, (Foo)
	    (popscope)			; this, (Foo)
	    (initproperty ((ns "") "Foo"))
	    (pushundefined)))

(define ctx (compiler))

(check (new-class ctx '((ns "") "Foo") '((ns "") "Bar") 
		  '(((ns "") "first") ((ns "") "second"))
		  42) => 0)

(check (compiler-instances ctx)
       => '(((name ((ns "") "Foo"))
	     (super_name ((ns "") "Bar"))
	     (flags 0)
	     (interface ())
	     (iinit (method 42))
	     (trait
	      (((kind slot) (name ((ns "") "first")) (slot_id 0) (type_name *) (vindex 0) (vkind 0) (metadata ()))
	       ((kind slot) (name ((ns "") "second")) (slot_id 0) (type_name *) (vindex 0) (vkind 0) (metadata ()))
	       )))))

(check (compiler-classes ctx) => '(((cinit (method 1)) (trait ()))))

(check (car (compiler-lambdas ctx)) ;; method 0 class initializer of Foo
       => '((signature
	      ((return_type *) (param_type ()) (name "*class-initializer*") (flags 0) (options ()) (param_names ())))
	     (code ((returnvoid)))))

(new-class ctx '((ns "") "Baz") '((ns "") "Bar") '() 0)

(check (compiler-class-slots ctx)
       => '(
            ((kind class) (name ((ns "") "Foo")) (slot_id 0) (classi (class 0)) (metadata ()))
            ((kind class) (name ((ns "") "Baz")) (slot_id 0) (classi (class 1)) (metadata ()))))

;; Class with constructor

(define ctx (compiler))

(check (parse-expr2 ctx syntax/expr
                    '((class Foo (Bar) (prop)
                             (lambda (x) (slot-setq! #self prop x)))))
       => '((getglobalscope)		; this
	    (getlex ((ns_set 1) "Bar"))	; this, Bar
	    (dup)			; this, Bar, Bar
	    (pushscope)			; this, Bar
	    (newclass 0)		; this, (Foo)
	    (popscope)			; this, (Foo)
	    (initproperty ((ns "") "Foo"))
	    (pushundefined)))

(check (assq 'code (car (load-lambda ctx)))
       => '(code ((getlocal 0)
                  (getlocal 1)
                  (setproperty ((ns_set 1) "prop"))
                  (pushundefined)
                  (returnvalue))))

(check (compiler-instances ctx)
       => '(((name ((ns "") "Foo"))
	     (super_name ((ns_set 1) "Bar"))
	     (flags 0)
	     (interface ())
	     (iinit (method 1))
	     (trait
	      (((kind slot) (name ((ns "") "prop")) (slot_id 0) (type_name *) (vindex 0) (vkind 0) (metadata ()))
	       )))))

;;;; Namespaces

(define ctx (compiler))

(check (parse-expr2 ctx syntax/expr '((import foo))) => '((pushundefined)))
(check (load-ns-set ctx) => '((ns_set (ns "")) (ns_set (ns "foo") (ns ""))))
(check (compiler-imports-id ctx) => 2)

(check (parse-expr2 ctx syntax/expr '((in-ns "foo"))) => '((pushundefined)))
(check (compiler-ns ctx) => "foo")

(check (parse-expr2 ctx syntax/expr '((in-ns ""))) => '((pushundefined)))
(check (compiler-ns ctx) => "")

;;;; Exceptions

(define ctx (compiler))
(check (exception-push ctx '((from L1) (to L2) (target L3)
                             (exc_type ((ns "") Object)) (var_name ((ns "") "e2"))))
       => 0)
(check (exception-push ctx '((from L4) (to L5) (target L6)
                             (exc_type ((ns "") String)) (var_name ((ns "") "e2"))))
       => 1)
(check (load-exceptions ctx)
       => '(((from L1) (to L2) (target L3)
             (exc_type ((ns "") Object)) (var_name ((ns "") "e2")))
            ((from L4) (to L5) (target L6)
             (exc_type ((ns "") String)) (var_name ((ns "") "e2")))))

;;;; Macro test

(meta-define-macro (compiler) 'double '(lambda (x) `(+ ,(car x) ,(car x))))
(check (meta-macroexpand '(double 1)) => '(+ 1 1))

;; Quote

(check (q-object 1) => 1)
(check (q-object 'symbol) => '(intern "symbol"))
(check (q-object '(1 2)) => '(cons 1 (cons 2 ())))
(check (q-object '(1 . 2)) => '(cons 1 2))
(check (q-object '(1 2 3)) => '(cons 1 (cons 2 (cons 3 ()))))
(check (q-object '((1 second) 3)) => '(cons (cons 1 (cons (intern "second") ())) (cons 3 ())))
))

(test1)
(test2)
