;;;; Basic Standard library
;;;; This library set defines least necessary functions for stream and writer
;;;; 
;;;; NOTE: Semantics of equality of cola
;;;; == Identically equal. Same as eq? in scheme
;;;; =  Same value or same contents. Same as equal?
;;;; There is no equivalent operator as eqv? in cola.

(require "builtin-types.k")

;;;; Predicates

(define nil?		(lambda (obj) (= (typeof obj) typeof-undefined)))
(define long?		(lambda (obj) (= (typeof obj) typeof-long)))
(define string?		(lambda (obj) (= (typeof obj) typeof-string)))
(define symbol?		(lambda (obj) (= (typeof obj) typeof-symbol)))
(define double?		(lambda (obj) (= (typeof obj) typeof-double)))
(define pointer?	(lambda (obj) (= (typeof obj) typeof-pointer)))
(define array?		(lambda (obj) (= (typeof obj) typeof-array))) ; vector?

;;;; Systam calls

(define printf		(extern "printf"	'(-> int (* char) ...)))
(define snprintf	(extern "snprintf"	'(-> int (* char) int (* char) ...)))
(define abort		(extern "abort"		'(-> void void)))
(define strtoll		(extern "strtoll"	'(-> int (* char) int int)))
(define strtod		(extern "strtod"	'(-> double (* char) (* char))))

;;;; List utilities

(define set-car	(lambda (expr obj)	(and (pair? expr) (set-tuple-at expr 0 obj))))
(define set-cdr	(lambda (expr obj)	(and (pair? expr) (set-tuple-at expr 1 obj))))

(define caar	(lambda (expr) (car (car expr))))
(define cadr	(lambda (expr) (car (cdr expr))))
(define cdar	(lambda (expr) (cdr (car expr))))
(define cddr	(lambda (expr) (cdr (cdr expr))))

(define caddr	(lambda (expr) (car (cdr (cdr expr)))))
(define cdddr	(lambda (expr) (cdr (cdr (cdr expr)))))
(define cadddr	(lambda (expr) (car (cdr (cdr (cdr expr))))))

(define concat	(lambda (a b)
		  (if (pair? a)
		      (cons (car a) (concat (cdr a) b))
		    b)))

(define ->list (lambda (obj) obj))

;;;; Conversion

(define double->string
  (lambda (d)
    (let ((buf (new typeof-string 32)))
      (snprintf (addrof buf) 32 "%g" d)
      buf)))

(define symbol->string ;; [r6rs]
  (lambda (s)
    (let ((buf (new typeof-string (+ (strlen s) 1))))
      (memcpy (addrof buf) (addrof s) (strlen s))
      (poke (+ (addrof buf) (strlen s)) 0)
      buf)))

;;;; Scheme compatible functions

(define #f ()) ;; [r6rs]
(define #t 't) ;; [r6rs]
(define eq? ==) ;; [r6rs]
(define null? nil?) ;; [r6rs]
(define string-length strlen) ;; [r6rs]
(define string->symbol intern) ;; [r6rs]
(define max (lambda (a b) (if (> a b) a b))) ;; [r6rs(-)]
(define min (lambda (a b) (if (< a b) a b))) ;; [r6rs(-)]
(define negative? (lambda (n) (< n 0))) ;; [r6rs]
(define positive? (lambda (n) (> n 0))) ;; [r6rs]
(define zero? (lambda (n) (= n 0))) ;; [r6rs]
(define number? long?) ;; [r6rs]
(define set! (form (lambda (compiler head var val) (list 'set var val)))) ;; [r6rs]

;;;; Numbers

;; Strtoll convert a string to long long value, and then it is convert to long.
;; Therefore 0x80000000 is converted to -2147483648

(define string->number-radix
  (lambda (str radix) (strtoll str 0 radix)))

(define string->number-radix-sign
  (lambda (str radix sign) (string->number-radix (concat-string sign str) radix)))

(define string->number ;; [r6rs(-)]
  (lambda (str) (string->number-radix str 10)))

(define string->double
  (lambda (str) (strtod str (convert '(* char)  0))))

;;;; List enumeration
(define fold ()) ;; forward
(define length ;; [r6rs]
  (lambda (lst) (fold (lambda (_ n) (+ n 1)) 0 lst)))

(define map
  (lambda (fn list)
    (if (pair? list)
	(let ((head (fn (car list))) ;; head should be apply first if you care side effect!
	      (tail (map fn (cdr list))))
	  (cons head tail))
	list)))

(define map2
  (lambda (fn list1 list2)
    (if (pair? list1)
	(let ((head (fn (car list1) (car list2)))
	      (tail (map2 fn (cdr list1) (cdr list2))))
	  (cons head tail))
	list1)))

(define map-with-index-from
  (lambda (fn lst i)
    (if (pair? lst)
	(cons (fn (car lst) i)
	      (map-with-index-from fn (cdr lst) (+ i 1)))
        '())))

(define map-with-index
  (lambda (fn lst) (map-with-index-from fn lst 0)))

(define for-each
  (lambda (f lst)
    (while (pair? lst)
           (f (car lst))
           (set! lst (cdr lst)))))

(define for-each2
  (lambda (f lst1 lst2)
    (while (pair? lst1)
           (f (car lst1) (car lst2))
           (set! lst1 (cdr lst1))
           (set! lst2 (cdr lst2)))))

; (define for-each map)

;; srfi-1 style fold

(define fold ;; [srfi-1(-)]
  (lambda (f z lst)
    (while (pair? lst)
           (set! z (f (car lst) z))
           (set! lst (cdr lst)))
    z))

(define fold2
  (lambda (f z lst1 lst2)
    (if (pair? lst1)
	(fold2 f (f (car lst1) (car lst2) z) (cdr lst1) (cdr lst2))
	 z)))

(define reverse ;; [r6rs]
  (lambda (list) (fold cons '() list)))

(define member ;; [r6rs-lib]
  (lambda (key lst)
    (let ((found #f))
      (while (and (== found #f) (pair? lst))
             (if (= key (car lst))
                 (set! found lst)
                 (set! lst (cdr lst))))
      found)))

(define delete ;; [srfi-1] todo: use remq (r6rs-lib) instead?
  (lambda (v lst)
    (and (pair? lst)
         (let ((head (car lst)) (tail (cdr lst)))
           (if (= v head)
               tail
               (cons head (delete v tail)))))))

(define fold-right ;; [r6rs-lib(-)]
  (lambda (func z lst)
    (if (pair? lst)
	(func (car lst) (fold-right func z (cdr lst)))
	z)))

(define filter ;; [r6rs-lib]
  (lambda (pred lst)
    (fold-right (lambda (x xs) (if (pred x) (cons x xs) xs)) () lst)))

(define find
  (lambda (proc lst)
    (if lst
	(if (proc (car lst))
	    (car lst)
	    (find proc (cdr lst)))
	())))

(define list-ref
  (lambda (lst pos)
    (if lst
	(if (= pos 0)
	    (car lst)
	    (list-ref (cdr lst) (- pos 1)))
	(error "list-ref: index " pos " too large for " lst))))


(define take
  (lambda (lst i)
    (if (or (== i 0) (null? lst))
        ()
        (cons (car lst) (take (cdr lst) (- i 1))))))

(define drop
  (lambda (lst i)
    (if (or (== i 0) (null? lst))
        lst
        (drop (cdr lst) (- i 1)))))

(define assoc ;; [r6rs-lib]
  (lambda (obj list)
    (and (pair? list)
	 (if (= obj (car (car list)))
	     (car list)
	     (assoc obj (cdr list))))))

(define assq ;; [r6rs-lib]
  (lambda (obj lst)
    (let ((found #f))
      (while (and (== found #f) (pair? lst))
	     (if (== obj (car (car lst)))
		 (set! found (car lst))
		 (set! lst (cdr lst))))
      found)))

(define append ;; [r6rs-lib]
  (lambda ls (fold-right concat '() ls)))

(define append-map ;; [srfi-1]
  (lambda (fn list) (apply append (map fn list))))

(define concatenate ;; [srfi-1]
  (lambda (list-of-list) (apply append list-of-list)))

;; (define take-while ;; [srfi-1]
;;   (lambda (proc xs)
;;     (if (null? xs) '()
;;         (if (proc (car xs)) (cons (car xs) (take-while proc (cdr xs)))
;;             '()))))

(define drop-while ;; [srfi-1]
  (lambda (proc xs)
    (if (pair? xs)
        (if (proc (car xs)) (drop-while proc (cdr xs))
            xs)
        '())))

(define iota ;; [srfi-1(-)]
  (let ((*iota))
    (set! *iota (lambda (count i)
		 (if (<= i count)
		     (cons i (*iota count (+ i 1)))
		     '())))
    (lambda (count) (*iota count 1))))

(define build-list
  (lambda (n proc) (map proc (iota n))))

;;;; Array

(define array->list
  (lambda (arr)
    (let ((i (array-length arr))
          (lst ()))
      (while (< 0 i)
             (set! i (- i 1))
             (set! lst (cons (array-at arr i) lst)))
      lst)))
             
(define list->array
  (lambda (l)
    (let ((lst l)
          (arr (array)))
      (while (pair? lst)
             (array-insert-at arr (array-length arr) (car lst))
             (set! lst (cdr lst)))
      arr)))

;;;; Symbol

(define concat-symbol
  (lambda args
    (intern
     (apply concat-string
      (map (lambda (sym) (symbol->string sym)) args)))))

;;;; Form

(define qq-object
  (lambda (x)
    (if (pair? x)
        (let ((head (car x))
              (tail (cdr x)))
          (if (eq? head 'unquote)
              (car tail)
              (if (and (pair? head)
                       (eq? (car head) 'unquote-splicing))
                  (list 'concat (list '->list (cadr head)) (qq-object tail))
                  (list 'cons (qq-object head) (qq-object tail)))))
        (list 'quote x))))

(define quasiquote
  (form (lambda (c f expr) (qq-object expr))))

(define define-form
  (form
    (lambda (compiler head name args . body)
      `(define ,name (form (lambda ,(concat '(*compiler* *name*) args) ,@body))))))

;; equal? is needed because Cola's = is a bit strict than Scheme's equal?
;; todo: Cola's = could be a bug.
(define equal?
  (lambda (a b)
    (or (= a b)
	(and (pair? a)
             (pair? b)
             (and (equal? (car a) (car b))
                  (equal? (cdr a) (cdr b))))
        (and (array? a)
             (array? b)
             (equal? (array->list a) (array->list b))))))

;; srfi-78 Lightweight testing function
;; todo: use namespace

(define *check-write* println)

(define check
  (form (lambda (compiler _ expr => expected)
          `(let ((expr ,expr)
                 (expected ,expected))
             (if (equal? expr expected)
                 ()
                 (check-error ',expr expr expected))))))

(define check-error
  (lambda (expr result expected)
    (*check-write* expr)
    (printf "=> ")
    (*check-write* result)
    (printf "  ; *** failed ***\n")
    (printf "  ; expected result: ")
    (*check-write* expected)))

;;;; Basic forms

(require "boot/pattern.k")

;; (define-pattern ((name pattern) template)
;;                 ((name pattern) template) ...)
;;
;; Define a pattern based macro. The first car of pattern (key) is
;; used as a macro name. Other names are just ignored.

(define define-pattern
  (form
    (lambda (compiler head . cases)
      `(define ,(caar cases)
         (form (lambda ,(cons '*compiler* 'form)
                 (pattern-apply ',cases form)))))))

(define-pattern ((not e)           (== () e)))
(define-pattern ((begin . e)       (let () . e)))

(define-pattern
  ((cond)                  ())
  ((cond (c0 . b0) . rest) (if c0 (begin . b0) (cond . rest))))

(define-pattern
  ((let* () . body)                (let () . body))
  ((let* (bindings . rest) . body) (let (bindings) (let* rest . body))))

;;;; Structure

(define doprint (array))
(define type-fields (array))
(define type-name (array))
(set-array-fill type-fields (lambda (obj) ()))
(set-array-fill type-name (lambda (obj) (typeof obj))) ;; todo: type-name should return a symbol instead of number

(define make-getters
  (lambda (name offset fields)
    (map-with-index
      (lambda (field index)
	`(define ,(concat-symbol name '- field)
	   (lambda (obj) (,(concat-symbol 'tuple-at) obj ,index))))
      fields)))

(define make-setters
  (lambda (name offset fields)
    (map-with-index
      (lambda (field index)
	`(define ,(concat-symbol 'set- name '- field)
	   (lambda (obj value) (,(concat-symbol 'set-tuple-at) obj ,index value))))
      fields)))

(define-form define-structure (name fields . parents)
  (let ((size (length fields))
	(type (new-type)))
    `(let ()
       (define ,(concat-symbol 'typeof- name) ,type)
       (define ,(concat-symbol 'sizeof- name) ,size)
       (define ,(concat-symbol 'new-    name) (lambda () (new ,type ,size)))
       (set-array-at doprint ,type (lambda (self) (princ "<" ',name ">")))
       (set-array-at type-fields ,type (lambda (self) ',fields))
       (set-array-at type-name ,type (lambda (self) ',name))
       ,@(make-getters name 0 fields)
       ,@(make-setters name 0 fields)
       )))

;;;; Muitimethod todo: make it compatible to tamacola

(define-pattern
  ((define-generic name args . default-body)
   (define name
     (let ((**obj** (array)))
       (set-array-fill **obj** (lambda args . default-body))
       **obj**))))

(define-form define-method (name type func)
  `(let ()
     (define ,(concat-symbol name '- type) ,func)
     (set-array-at ,name ,type ,(concat-symbol name '- type))))

;;;; Namespace (copied from test.k)

(define local-namespace
  (lambda (env)
    (while (and (pair? env) (!= env (variable-value (cdar env))))
      (set! env (cdr env)))
    env))

(define parent-namespace
  (lambda (env)
    (local-namespace (cdr env))))

(define-form current-namespace () `',(compiler-environment *compiler*))

(define namespace
  (lambda (parent name)
    (or name (set! name '<>))
    (let ((var (new typeof-variable)))
      (set-variable-name  var name)
      (set-variable-class var 'global)
      (set-variable-free  var 't)
      (set-variable-value var (cons (cons name var) parent)))))

(define namespace-do
  (lambda (ns fn)
    (let ((result ())
	  (start  (cdr ns)))
      (while (and start (!= '*namespace* (caar start)))
        (set! result (apply fn start))
	(set! start (cdr start)))
      result)))

(define-form library (name . rest)
  (let ((name (if (null? name) 'global name)))
    `(in ,name ,@rest)))

;;;; Byte Strings

(define-structure bytes (bits size))

(define make-bytes
  (lambda (length)
    (let ((self (new-bytes)))
      (set-bytes-bits self (new typeof-string length))
      (set-bytes-size self length)
      self)))

(define bytes-length (lambda (v) (bytes-size v)))

(define bytes-ref
  (lambda (v i)
    (if (and (<= 0 i) (< i (bytes-size v)))
	(_peek (+ (addrof (bytes-bits v)) i))
	(error "Out of bounds"))))

(define bytes-set!
  (lambda (v i oct)
    (if (and (<= 0 i) (< i (bytes-size v)))
	(poke (+ (addrof (bytes-bits v)) i) oct)
	(error "Out of bounds"))))

(define bytes
  (lambda elements
    (let ((dst (make-bytes (length elements))))
      (map-with-index (lambda (e i) (bytes-set! dst i e)) elements)
      dst)))

(define bytes->list
  (lambda (v)
    (let ((i (bytes-length v))
	  (ls '()))
      (while (< 0 i)
	     (set! i (- i 1))
	     (set! ls (cons (bytes-ref v i) ls)))
      ls)))

(define bytes->string
  (lambda (bs)
    (let ((str (new typeof-string (+ (bytes-length bs) 1))))
      (memcpy (addrof str) (addrof (bytes-bits bs)) (bytes-length bs))
      str)))

(define string->bytes
  (lambda (str)
    (let ((bs (make-bytes (strlen str))))
      (memcpy (addrof (bytes-bits bs)) (addrof str) (strlen str))
      bs)))

(define double->bytes
  (lambda (d)
    (let ((bs (make-bytes (sizeof d))))
      (memcpy (addrof (bytes-bits bs)) (addrof d) (sizeof d))
      bs)))

(define bytes->double
  (lambda (bs)
    (let ((d (new typeof-double 1)))
      (memcpy (addrof d) (addrof (bytes-bits bs)) sizeof-double)
      d)))

(define list->bytes
  (lambda (ls) (apply bytes ls)))

(define list->string
  (lambda (ls)
    (bytes->string (list->bytes ls))))

(define string->list
  (lambda (str)
    (bytes->list (string->bytes str))))

;; Destructive byte-array copy.
;; (byte-array-copy! dest dest-start src [ src-start src-end ])
(define bytes-copy!
  (lambda (dest dest-start src . option)
    (if (< dest-start 0) (error "Out of bounds"))
    (let ((src-start (if option (car option) 0))
	  (src-end (if option (cadr option) (bytes-length src))))
      (let ((src-addr  (+ (addrof (bytes-bits src)) src-start))
	    (dest-addr (+ (addrof (bytes-bits dest)) dest-start))
	    (src-length  (- src-end src-start))
	    (dest-length (- (bytes-length dest) dest-start)))
	(memcpy dest-addr src-addr (min src-length dest-length))
	dest))))

;; Return a new byte string copied from str, the length is end - start.
;; (subbytes str start end)
(define subbytes
  (lambda (str start end)
    (let ((dest (make-bytes (- end start))))
      (bytes-copy! dest 0 str start end))))

(define substring
  (lambda (str start end)
    (bytes->string (subbytes (string->bytes str) start end))))

;; Return a copy of the string witout extension.
(define trim-extension
  (lambda (string)
    (let ((reversed (drop-while
                     (lambda (c) (not (eq? c #".")))
                     (reverse (string->list string)))))
      (list->string
       (reverse (if (pair? reversed) (cdr reversed) reversed))))))

;; makeshift about dictionary

(define make-immutable-hash
  (lambda (ass) (list ass)))

(define make-hasheq
  (lambda () '(())))

(define hash-ref
  (lambda (dict key . default)
    (let ((found (assq key (car dict))))
      (if found
          (cdr found)
          (if (pair? default)
              (car default)
              (error "no value found for key:" key))))))

(define hash-set!
  (lambda (dict key v)
    (let ((found (assq key (car dict))))
      (if found
	  (set-cdr found v)
	  (set-car dict (cons (cons key v) (car dict)))))
    v))

;; macro debug function

(define macroexpand-1
  (form
   (lambda (compiler _ expr)
     `(apply (form-function ,(car expr)) (cons '*compiler* ',expr)))))

(define trace println)
