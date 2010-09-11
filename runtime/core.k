;; Macro definition
;;
;; Macors are processed before compilation so you have to include this
;; file each time you build a binary using those macros. You can't
;; compile it separate abc file.

(define-pattern ((begin . e)            (let () . e)))

(define-pattern ((and)                  #t)
                ((and x)                x)
                ((and x . xs)           (let ((**cond** x))
                                          (if **cond**
                                              (and . xs)
                                              **cond**))))

(define-pattern ((or)                   #f)
                ((or x)                 x)
                ((or x . xs)            (let ((**cond** x))
                                          (if **cond**
                                              **cond**
                                              (or . xs)))))

(define-pattern
  ((cond)                               ())
  ((cond (c0 . b0) . rest)  	        (if c0 (begin . b0) (cond . rest))))

(define-pattern
  ((let* () . body)                     (let () . body))
  ((let* (bindings . rest) . body)      (let (bindings) (let* rest . body))))

;;;; Constructor

(define-pattern ((list)                 ())
                ((list x . xs)          (cons x (list . xs))))

(define-pattern ((bytes . xs)           (list->bytes (list . xs))))
(define-pattern ((array . xs)           (list->array (list . xs))))

;;;; Traditional Macro

(define-macro define-form
  (lambda (exp)
    (let ((name (car exp))
          (args (cadr exp))
          (body (caddr exp)))
      `(define-macro ,name
         (lambda (**exp**)
           (let ,(match-assign args '**exp**) ,body))))))

(define-form macroexpand-1 (expr)
  (list 'quote (meta-macroexpand expr)))

;;;; Structure

(define-form define-structure (name fields)
  (let ((type (concat-symbol 'typeof- name))
        (getters (map
                  (lambda (field)
                    `(define ,(concat-symbol name (concat-symbol '- field))
                       (lambda (obj) (slot-getq obj ,field))))
                  fields))
        (setters (map
                  (lambda (field)
                    `(define ,(concat-symbol 'set- (concat-symbol name (concat-symbol '- field)))
                       (lambda (obj v) (slot-setq! obj ,field v))))
                  fields)))
    `(begin
       (class ,type (Object) ,fields)
       (define ,(concat-symbol 'new- name)
         (lambda () (new ,type)))
       ,@getters
       ,@setters
;; todo
       (slot-setq! (slot-getq ,type prototype) toString
                   (lambda () (+ (+ "<" ,(symbol->string name)) ">")))
       (define-method type-fields ,type (lambda () ',fields))
       (define-method type-name ,type (lambda (self) ',name))
       )))

;; Single dispatch method definition
;;
;; Define-generic declare a generic function name. It corresponds
;; set-array-fill in cola. The first argument in function refers the
;; receiver. Conventionally "self" is used. The default function
;; (lambda) must be a lambda exression, because it needs the arg
;; names. A method is stored as a property named *m*-name.

(define-form define-generic (name args . default-body)
  (let ((default `(lambda ,args ,@default-body))
        (method-name (concat-symbol '*m*- name)))
    `(begin (define ,name
              (lambda ,args
                (if (=== () ,(car args))
                    (send *null* ,method-name ,@args)
                    (send ,(car args) ,method-name ,@args))))
            (define-method ,name Object ,default))))

;; Define-method defines a method. It corresponds set-array-fill in cola.

(define-form define-method (name type func)
  (let ((method-name (concat-symbol '*m*- name)))
    `(slot-setq! (slot-getq ,type prototype) ,method-name ,func)))

(define-generic type-name (obj) (string->symbol (typeof obj)))
(define-generic type-fields (obj) ())

;; typeof-null : pseudo type.
;; Because ActionScript can't dispatch null value. typeof-null is used for null.
(define-structure null ())
(define *null* (new-null))

;; Makeshift : Ignore namespace expression like ns/expr
(define-pattern ((in ns . expr) (begin . expr)))

;;;; Error
;; cola/flash doesn't have variable arguments yet. So it is defined as a macro.

(define-pattern
  ((error str . rest)
   (let ((err (new Error)))
     (trace str . rest)
     (slot-setq! err message str)
     (throw err))))

;;;; Optimization

(define-pattern ((cons h t) (new typeof-pair h t)))
(define-pattern ((car lst) (slot-getq lst head)))
(define-pattern ((cdr lst) (slot-getq lst tail)))
(define-pattern ((pair? v) (instanceof v typeof-pair)))
(define-pattern ((bytes? v) (instanceof v flash.utils/ByteArray)))
(define-pattern ((array? v) (if v (instanceof v Array) #f)))
(define-pattern ((string-ref str i) (send str charCodeAt i)))

;;;; More control structures

(define-form do (viss test . cs)
  (let ((process '()))
    (set! process
	  (lambda (l inits steps)
	    (if (null? l)
		(cons inits steps)
		(let ((v (car l)))
		  (process (cdr l)
			   `(,@inits ,(list (car v) (cadr v)))
			   (if (= 3 (length v))
			       `(,@steps (set! ,(car v) ,(caddr v)))
			       steps))))))
    (let ((vs (process viss '() '())))
      `(let (,@(car vs))
	 (while (not ,(car test))
		,@cs
		,@(cdr vs))
	 ,@(cdr test)))))

;(check (macroexpand-1
; (do ((i 0 (+ i 1))
;      (j 0 (+ j 2)))
;    ((= i 5) (* i j))
;  (trace i j))) => '(let ((i 0) (j 0)) (while (not (= i 5)) (trace i j) (set! i (+ i 1)) (set! j (+ j 2))) (* i j)))

;; Namespaces
;;
;; This syntax is borrowed from R6RS. But it uses name.space.path
;; instead of (name space path) to specify namespaces.
;; e.g:
;; (library my.useful.library
;;   (import some.lib)
;;   ....)

(define-form library (name . rest)
  (let ((ns (if (null? name) "" (symbol->string name))))
    `(let ()
       (in-ns ,ns)
       (in-imports "" ,ns)
       ,@rest
       (in-ns "")
       (in-imports ""))))
