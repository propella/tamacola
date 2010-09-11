;; Frame accessors
(define new-frame (lambda (blocks slots exceptions) (list blocks slots exceptions)))
(define frame-blocks car)
(define frame-slots cadr)
(define frame-exceptions caddr)

;; Block accessors
(define new-block (lambda (local free) (list local free)))
(define block-locals car)
(define block-slots cadr)

;; Compiler context object
(define-structure compiler (result env lambdas instances classes slotnames ns imports))
(define-method     parser-result typeof-compiler     compiler-result)
(define-method set-parser-result typeof-compiler set-compiler-result)

(define compiler
  (lambda ()
    (let ((ctx (new-compiler)))
      (set-compiler-result ctx ())
      (set-compiler-env ctx (cons (new-frame '() '() '()) ()))
      (set-compiler-lambdas ctx ())
      (set-compiler-instances ctx ())
      (set-compiler-classes ctx ())
      (set-compiler-slotnames ctx ())
      (set-compiler-ns ctx "")
      (set-compiler-imports ctx '(("")))
      ctx)))

(define push-instances
  (lambda (ctx instance)
    (set-compiler-instances ctx (cons instance (compiler-instances ctx)))))

(define push-classes
  (lambda (ctx klass)
    (set-compiler-classes ctx (cons klass (compiler-classes ctx)))))

(define push-slotnames
  (lambda (ctx mname)
    (set-compiler-slotnames ctx (cons mname (compiler-slotnames ctx)))))

(define compiler-imports-top
  (lambda (ctx) (car (compiler-imports ctx))))

(define compiler-imports-id
  (lambda (ctx) (length (compiler-imports ctx))))

(define push-nsset
  (lambda (ctx nsset)
    (set-compiler-imports ctx (cons nsset (compiler-imports ctx)))))

(define push-import
  (lambda (ctx sym)
    (let ((new-set (cons (symbol->string sym)
                         (compiler-imports-top ctx))))
      (push-nsset ctx new-set))))

;; Return namespace and localname
(define parse-multiname
  (lambda (sym)
    (let* ((name (symbol->string sym))
	   (i (- (string-length name) 1)))
      (while (and (<= 0 i)
		  (!= (string-ref name i) #"/"))
	     (set! i (- i 1)))
      (if (< i 0)
	  (cons '() name)        		;; namespace is not found
	  (let ((ns (substring name 0 i))	;; namespace is found
		(local (substring name (+ i 1) (string-length name))))
	    (cons `(package ,ns) local))))))

(define new-multiname
  (lambda (ctx sym)
    (let ((pair (parse-multiname sym)))
      (list (if (null? (car pair))
                `(ns_set ,(compiler-imports-id ctx))
                (car pair))
            (cdr pair)))))

(define slot-multiname
  (lambda (ctx sym)
    (let ((pair (parse-multiname sym)))
      (list (if (null? (car pair)) `(ns ,(compiler-ns ctx)) (car pair))
            (cdr pair)))))

;; Push and pop update first argument as a stack operation.

(define-pattern ((push lval rval) (set! lval (cons rval lval))))

(define-pattern ((pop lval) (let ((*stack-top* (car lval)))
                              (set! lval (cdr lval))
                              *stack-top*)))

;; new-label initializes the internal counter only once. And answer
;; another label each time when it is called.

(define new-label
  (let ((label-count 0))
    (lambda ()
      (set! label-count (+ label-count 1))
      (intern (string-append "L" (number->string label-count))))))

;; Save a function and return the method id. Newer function has lager
;; number.  The method id begins with 1 because method 0 is used for
;; the main function.

(define save-lambda
  (lambda (ctx l)
    (set-compiler-lambdas ctx (cons l (compiler-lambdas ctx)))
    (length (compiler-lambdas ctx))))
    
;; Return saved functions. New function is first.

(define load-lambda (lambda (ctx) (reverse (compiler-lambdas ctx))))

;; The Compile Time Environment

;; keeps track of all variable name and helps to decide how variables
;; are implemented in the VM.

;; Terminology

;; - local variable      - variable defined in the the function.
;; - register variable   - local variable && referred by only the function.
;; - activation variable - local variable && referred by other functions.
;; - free variable       - variable defined in the parent environments.

;; local = (... name2 name1 name0) ;; are all variable names.
;; free = (... name2 name1 name0) ;; are free variables.

;; block = (local free) ;; is made by a let
;;    Free variables are subset of local. Locals not included free
;;    are register variables. Activation variables are stored in the
;;    activation record, and register variables are stored in registers.
;;    Variables are stored in reverse order of the souce code.

;; blocks = (... block2 block1 block0)

;; frame = (blocks slots exceptions) ;; is made by a lambda
;;    Each frame consists of nested blocks. Outer block shadows same
;;    variable names as the innter block. e.g. block1 shadows
;;    block0. A frame is represented as a stack frame in the VM.
;;    slots is a set of activation variables included in the blocks.
;;    slots is used to define an activation slot for the function.

;; frames = (... frame2 frame1 frame0) ;; is made by nested lambdas
;;    A function can be nested with functions. Each nested layer is a
;;    frame. Variables refereed by inside frame are activation
;;    variables. Inside frame must access the outside by the scope
;;    stack (runtime environment).

;; Accessors of top frame of the environment
(define env-frame-top (lambda (ctx) (car (compiler-env ctx))))

;; Push and pop a frame in the environment.
(define frame-push
  (lambda (ctx frame)
    (set-compiler-env ctx (cons frame (compiler-env ctx)))))

(define frame-pop
  (lambda (ctx)
    (let ((top (env-frame-top ctx)))
      (set-compiler-env ctx (cdr (compiler-env ctx)))
      top)))

;; Push and pop a block in the top block
(define block-push
  (lambda (ctx block)
    (let ((frame (frame-pop ctx)))
      (frame-push ctx (new-frame (cons block (frame-blocks frame))
                                 (frame-slots frame)
                                 (frame-exceptions frame))))))

(define block-pop 
  (lambda (ctx)
    (let ((frame (frame-pop ctx)))
      (frame-push ctx (new-frame (cdr (frame-blocks frame))
                                 (frame-slots frame)
                                 (frame-exceptions frame))))))

;; Push a new block with variables
(define push-vars
  (lambda (ctx vars)
    (block-push ctx (new-block (reverse vars) ()))))

;; Mark the name as free variable in the block.
;; Return new block, or #f if the name is not found.
(define block-mark-free
  (lambda (block name)
    (and (memq? name (block-locals block))
	 (if (memq? name (block-slots block)) ;; todo: use SRFI-1 set function
	     block
	     (list (block-locals block) (cons name (block-slots block)))))))

(define *frame-mark-free
  (lambda (blocks name)
    (let ((found #f)
          (skipped '()))
      (while (and (pair? blocks) (eq? found #f))
        (set! found (block-mark-free (car blocks) name))
        (set! skipped (cons (car blocks) skipped))
        (set! blocks (cdr blocks)))
      (if found
          (let ((head (reverse (cdr skipped)))
                (tail (cons found blocks)))
            (append head tail))
          '()))))

;; Mark the name as free variable in the frame.
;; Return new frame, or '() if the name is not found.
(define frame-mark-free
  (lambda (frame name)
    (let ((blocks (*frame-mark-free (frame-blocks frame) name)))
      (and blocks
	   (new-frame blocks 
		      (let ((slots (frame-slots frame)))
			(if (memq? name slots)
			    slots
			    (cons name slots)))
                      (frame-exceptions frame))))))

;; Mark the name as free variable in the environment.
;; Return new environment.
(define env-mark-free
  (lambda (env name)
    (and env
	 (let ((found-frame (frame-mark-free (car env) name)))
	   (if found-frame
	       (cons found-frame (cdr env))
	       (cons (car env) (env-mark-free (cdr env) name)))))))

(define blocks-depth
  (lambda (blocks)
    (fold
     (lambda (block sum) (+ (length (block-locals block)) sum))
     0
     blocks)))

;; Return a way to access the variable. (local id) or #f if not found.
(define block-refer
  (lambda (block name)
    (let ((found (memq? name (block-locals block))))
      (and found
	   (list 'local (length found))))))

(define *frame-refer
  (lambda (blocks name)
    (let ((found #f))
      (while (and (pair? blocks) (eq? found #f))
        (set! found (block-refer (car blocks) name))
        (set! blocks (cdr blocks)))
      (if found
          (list 'local (+ (cadr found) (blocks-depth blocks)))
          '(free)))))

;; Return a way to access the variable. (local id) or (free) in the frame
(define frame-refer
  (lambda (frame name)
    (*frame-refer (frame-blocks frame) name)))

;; Return a way to access the variable. Modify the compile environment
;; if a free variable is found in the environment.

(define env-refer
  (lambda (ctx name)
    (let ((ref (frame-refer (env-frame-top ctx) name)))
      (if (equal? ref '(free))
	  (set-compiler-env ctx (env-mark-free (compiler-env ctx) name))
          '*nothing*)
      ref)))

(define make-getter-local (lambda (name id) `((*getlocal ,name ,id))))
(define make-getter-property (lambda (ctx name) `((getlex ,(new-multiname ctx name)))))

;; Return instructions which push value of the variable name into the
;; stack. stack: (-- object)
(define make-getter
  (lambda (ctx name)
    (let ((ref (env-refer ctx name)))
      (if (eq? (car ref) 'local)
         (make-getter-local name (cadr ref))
         (make-getter-property ctx name)))))

(define make-setter-local
  (lambda (name id) `((*setlocal ,name ,id))))

(define make-setter-property
  (lambda (ctx name)
    (let ((multiname (new-multiname ctx name)))
      `((coerce_a)
        (findpropstrict ,multiname)
	(swap)
	(setproperty ,multiname)))))

;; Return instructions which set value of the top stack into the
;; variable. stack: (object --)
(define make-setter
  (lambda (ctx name)
    (let ((ref (env-refer ctx name)))
      (if (eq? (car ref) 'local)
	  (make-setter-local name (cadr ref))
	  (make-setter-property ctx name)))))

;; Make instructions for bindings
;; pairs is ((name0 . insts0) (name1 . insts1) ...)

(define make-bindings
  (lambda (ctx pairs)
    (let ((setters (map
                     (lambda (pair) (make-setter ctx (car pair)))
                     pairs))
           (values (map cdr pairs)))
      (let ((setters* (concatenate (reverse setters)))
            (values* (concatenate values)))
      `(,@values* ,@setters*)))))

;; Return instructions to copy local variable to activation record.

(define copy-activation
  (lambda (frame)
    (let* ((slots (block-slots (car (frame-blocks frame))))
           (defs (append-map
                  (lambda (name)
                    (let* ((found (frame-refer frame name))
                           (id (cadr found)))
                      `((pushstring ,(symbol->string name))
                        ,@(make-getter-local name id))))
                    slots)))
      (if slots
          `(,@defs
             (newobject ,(length slots))
             (pushwith))
          ()))))

;; Return var name if names includes ints's operand, or #f

(define inst-varname
  (lambda (inst names)
    (let ((found (memq? (cadr inst) names)))
      (and found (car found)))))

;; Convert pseudo inst to activation reference

(define convert-activation
  (lambda (ctx frame insts)
    (let ((slots (block-slots (car (frame-blocks frame)))))
      (if slots
	  (append-map
	   (lambda (inst)
	     (cond
	      ((and (eq? (car inst) '*getlocal)
		    (inst-varname inst slots))
	       (make-getter-property ctx (inst-varname inst slots)))
	      ((and (eq? (car inst) '*setlocal)
		    (inst-varname inst slots))
	       (make-setter-property ctx (inst-varname inst slots)))
	      ('else (list inst))))
	   insts)
	  insts))))
  
;; Convert pseudo inst to register reference (*getlocal -> getlocal)

(define convert-local
  (lambda (insts)
    (append-map
     (lambda (inst)
       (cond
        ((symbol? inst) (list inst))
	((eq? (car inst) '*getlocal) `((getlocal ,(caddr inst))))
	((eq? (car inst) '*setlocal) `((coerce_a)
                                        (setlocal ,(caddr inst))))
	('else (list inst))))
     insts)))

;; Rename local variable to slot access, and add code for copy those values.
(define enable-activation
  (lambda (ctx frame insts)
    (let ((pushscope (copy-activation frame))
	  (xs (convert-activation ctx frame insts)))
      (if (pair? pushscope)
          `(,@pushscope ,@xs (popscope))
          xs))))

;;;; Exception

;; Push a new exception and return the exception id (zero base).
(define exception-push
  (lambda (ctx exception)
    (let* ((frame (frame-pop ctx))
           (exceptions (cons exception (frame-exceptions frame))))
      (frame-push ctx (new-frame (frame-blocks frame)
                                 (frame-slots frame)
                                 exceptions))
      (- (length exceptions) 1))))

;; Return popped exceptions in the frame.
(define load-exceptions
  (lambda (ctx)
    (reverse (frame-exceptions (env-frame-top ctx)))))

;; New-slots receives multi-names and return slot definitions.

(define new-slots
  (lambda (names)
    (map
     (lambda (name)
       `((kind slot)
         (name ,name)
         (slot_id 0) (type_name *) (vindex 0) (vkind 0) (metadata ())))
     names)))

;; Make a new signature

(define signature-param-type
  (lambda (nparam)
    (if (> nparam 0)
	(cons '* (signature-param-type (- nparam 1)))
	'())))

(define signature
  (lambda (name nparam)
    (let ((pt (signature-param-type nparam)))
    `(signature ((return_type *) (param_type ,pt) (name ,name) (flags 0) (options ()) (param_names ()))))))

(define frame-signature
  (lambda (name frame)
    (let ((nparam (length (block-locals (car (frame-blocks frame))))))
      (signature name nparam))))

;; Class definition

(define load-instances (lambda (ctx) (reverse (compiler-instances ctx))))
(define load-classes   (lambda (ctx) (reverse (compiler-classes ctx))))
(define load-slots     (lambda (ctx) (new-slots (compiler-slotnames ctx))))

(define save-slot
  (lambda (ctx mname)
    (or (member mname (compiler-slotnames ctx))
        (push-slotnames ctx mname))))

(define new-class-init
  (lambda (ctx)
    (save-lambda
     ctx
     `(,(signature "*class-initializer*" 0 0)
       (code ((returnvoid)))))))

;; Return class ID

(define new-class
  (lambda (ctx name super elements constructor)
    (let* ((slots (new-slots elements))
	   (instance `((name ,name)
		       (super_name ,super)
		       (flags 0)
		       (interface ())
		       (iinit (method ,constructor))
		       (trait ,slots)))
	   (class `((cinit (method ,(new-class-init ctx))) (trait ()))))
      (push-instances ctx instance)
      (push-classes ctx class)
      (- (length (compiler-classes ctx)) 1))))

(define compiler-class-slots
  (lambda (ctx)
    (let ((instances (compiler-instances ctx)))
      (map-with-index
       (lambda (instance i)
         `((kind class) ,(car instance) (slot_id 0) (classi (class ,i)) (metadata ())))
       (reverse instances)))))

;; return slots and class slats
(define load-global-slots
  (lambda (ctx)
    (append (new-slots (compiler-slotnames ctx))
            (compiler-class-slots ctx))))

;;; Namespaces

(define load-ns-set
  (lambda (ctx)
    (reverse (map (lambda (set)
                    `(ns_set ,@(map (lambda (name) `(ns ,name)) set)))
                  (compiler-imports ctx)))))

;;;; Simple Quote

(define q-object
  (lambda (obj)
    (cond
     ((pair? obj) `(cons ,(q-object (car obj)) ,(q-object (cdr obj))))
     ((symbol? obj) `(intern ,(symbol->string obj)))
     ('else obj))))

;;;; Macros

;; macro dictionary
(define *macro* (make-hasheq))

(define set-macro-at
  (lambda (key expander) (hash-set! *macro* key expander)))

(define macro-at
  (lambda (key) (hash-ref *macro* key #f)))

;; When *IS-RUNTIME-MACRO-COMPILE-MODE* is #f, the compiler generates
;; macro code for the interactive shell. And the interactive shell can
;; read macro definition from generated .abc file.
(define *IS-RUNTIME-MACRO-COMPILE-MODE* #f)

(define set-macros-setup
  (lambda (ctx name expander)
    (if *IS-RUNTIME-MACRO-COMPILE-MODE*
        (let ((xs `((set-macro-at ',name ,expander))))
          (if (syntax/expr (->stream xs) ctx)
              (parser-result ctx)
              (error "syntax error:" xs)))
        '((pushundefined)))))

;; Pattern matching macro
(define meta-define-pattern
  (lambda (ctx cases)
    (let ((name (car (caar cases))))
      (set-macro-at name cases)
      (set-macros-setup ctx name (list 'quote cases)))))

;; Traditional macro
(define meta-define-macro
  (lambda (ctx name expander)
    (set-macro-at name (eval expander global))
    (set-macros-setup ctx name expander)))

(define meta-macroexpand
  (lambda (form)
    (if (pair? form)
	(let ((found (macro-at (car form))))
	  (if found
              (if (pair? found)
                  (meta-macroexpand (pattern-apply found form))
                  (meta-macroexpand (found (cdr form))))
	      form))
	form)))

;; Match by predicate

(define-pattern ((TEST-PREDICATE func)
  (and (func (peek *stream*))
       (begin (set-parser-result *parser* (next *stream*))
              #t))))

(define-rule is-long () (TEST-PREDICATE long?))
(define-rule is-double () (TEST-PREDICATE double?))
(define-rule is-symbol () (TEST-PREDICATE symbol?))
(define-rule is-string () (TEST-PREDICATE string?))
(define-rule is-array () (TEST-PREDICATE array?))
(define-rule is-null () (TEST-PREDICATE null?))
(define-rule is-pair () (TEST-PREDICATE pair?))

(define match-assign
  (lambda (formals vals)
    (if (pair? formals)
        (cons 
         (list (car formals) (list 'car vals))
         (match-assign (cdr formals) (list 'cdr vals)))
        (if (null? formals)
            ()
            (list (list formals vals))))))

(define parse-expr2
  (lambda (ctx rule collection)
    (if (rule (->stream collection) ctx)
        (parser-result ctx)
        'FAIL)))

(define parse-expr
  (lambda (rule collection)
    (parse-expr2 (compiler) rule collection)))
