;; Standard library for Cola/Flash runtime

(define to-string
  (lambda (obj) (send obj toString)))

;;;; Symbol

(class <symbol> (Object) (name)) ;; todo: namespace
(define *symbol-table* (new Object))

(define symbol? (lambda (v) (instanceof v <symbol>)))

(define intern
  (lambda (str)
    (let ((renamed (+ "_" str))) ;; mangled to avoid predefined property name like toString
      (let ((symbol (slot-get *symbol-table* renamed)))
        (if symbol
            symbol
            (let ((new-symbol (new <symbol>)))
              (slot-setq! new-symbol name str)
              (slot-set! *symbol-table* renamed new-symbol)
              new-symbol))))))

(define symbol->string ;; [r6rs]
  (lambda (sym) (slot-getq sym name)))

(define string->symbol intern) ;; [r6rs]

(define concat-symbol ;; [cola]
  (lambda (a b) (string->symbol (+ (symbol->string a) (symbol->string b)))))

(slot-setq! (slot-getq <symbol> prototype) toString
	    (lambda () (symbol->string #self)))

;;;; Pair list

(class typeof-pair (Object) (head tail)
       (lambda (h t)
         (slot-setq! #self head h)
         (slot-setq! #self tail t)))

(define cons
  (lambda (a b) (new typeof-pair a b)))

(slot-setq! (slot-getq typeof-pair prototype) toString
	    (lambda () (pair->string #self)))

(define pair? (lambda (v) (instanceof v typeof-pair)))

(define pair->string-cdr
  (lambda (v)
    (if (not v)
	""
	(if (pair? v)
	    (+ " "
	       (+ (car v) (pair->string-cdr (cdr v))))
	    (+ " . " (send v toString))))))

(define pair->string
  (lambda (v)
    (+ "("
       (+ (car v)
	  (+ (pair->string-cdr (cdr v))
	     ")")))))

(define car (lambda (p) (slot-getq p head)))
(define cdr (lambda (p) (slot-getq p tail)))

;; (define set-car! (lambda (p x) (slot-setq! p head x)))
;; (define set-cdr! (lambda (p x) (slot-setq! p tail x)))
;; (define set-car set-car!)
;; (define set-cdr set-cdr!) ; [cola]

(define caar	(lambda (expr) (car (car expr))))
(define cadr	(lambda (expr) (car (cdr expr))))
(define cdar	(lambda (expr) (cdr (car expr))))
(define cddr	(lambda (expr) (cdr (cdr expr))))

(define caddr	(lambda (expr) (car (cdr (cdr expr)))))
(define cdddr	(lambda (expr) (cdr (cdr (cdr expr)))))
(define cadddr	(lambda (expr) (car (cdr (cdr (cdr expr))))))

;;;; Enumeration

(define concat (lambda (l r) (fold-right cons r l)))

(define length
  (lambda (lst) (fold (lambda (_ n) (+ n 1)) 0 lst)))

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
  (lambda (list) (fold cons () list)))

(define member ;; [r6rs-lib]
  (lambda (key lst)
    (let ((found #f))
      (while (and (not found) (pair? lst))
             (if (equal? key (car lst))
                 (set! found lst)
                 (set! lst (cdr lst))))
      found)))

(define delete ;; [srfi-1] todo: use remq (r6rs-lib) instead?
  (lambda (v lst)
    (if (pair? lst)
         (let ((head (car lst)) (tail (cdr lst)))
           (if (= v head)
               tail
               (cons head (delete v tail))))
         ())))

(define fold-right ;; [r6rs-lib(-)]
  (lambda (fn z lst)
    (let ((reversed (reverse lst)))
      (fold fn z reversed))))

;; (define fold-left ;; [r6rs-lib(-)]
;;   (lambda (f z lst)
;;     (if (pair? lst)
;;         (fold-left f (f z (car lst)) (cdr lst))
;;         z)))

(define fold-left ;; [r6rs-lib(-)]
  (lambda (f z lst)
    (fold (lambda (l r) (f r l)) z lst)))

;; Make sure lst is evaluated left to right.
(define map
  (lambda (fn lst)
    (reverse (fold (lambda (x xs) (cons (fn x) xs)) () lst))))

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

(define filter ;; [r6rs-lib]
  (lambda (pred lst)
    (fold-right (lambda (x xs) (if (pred x) (cons x xs) xs)) () lst)))

(define null? (lambda (obj) (=== () obj))) ;; [r6rs]

(define for-each
  (lambda (fn lst)
    (while (pair? lst)
           (fn (car lst))
           (set! lst (cdr lst)))))

(define for-each2
  (lambda (f lst1 lst2)
    (while (pair? lst1)
           (f (car lst1) (car lst2))
           (set! lst1 (cdr lst1))
           (set! lst2 (cdr lst2)))))

(define list-ref ;; [r6rs]
  (lambda (lst pos)
    (if lst
	(if (=== pos 0)
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

(define assoc ;; [r6rs-lib]
  (lambda (obj list)
    (let ((found #f)
	  (current list))
      (while (and (not found) (pair? current))
	     (if (= obj (car (car current)))
		 (set! found (car current))
		 (set! current (cdr current))))
      found)))

(define assq ;; [r6rs-lib]
  (lambda (obj list)
    (let ((found #f)
	  (current list))
      (while (and (not found) (pair? current))
	     (if (=== obj (car (car current)))
		 (set! found (car current))
		 (set! current (cdr current))))
      found)))

(define append concat) ;; [r6rs-lib(-)]

(define concatenate ;; [srfi-1]
  (lambda (list-of-list) (fold-right concat () list-of-list)))

(define append-map ;; [srfi-1]
  (lambda (fn list) (concatenate (map fn list))))

(define memq
  (lambda (key lst)
    (let ((found #f))
      (while (and (pair? lst) (not found))
	 (if (=== key (car lst))
	     (set! found lst)
             (set! lst (cdr lst))))
      found)))

(define memq? memq)

(define find
  (lambda (proc lst)
    (if (pair? lst)
	(if (proc (car lst))
	    (car lst)
	    (find proc (cdr lst)))
	#f)))

;;; Array : see also (array ...) macro in core.k

(define array?
  (lambda (v) (if v (instanceof v Array) #f)))

(define array-length
  (lambda (arr) (slot-getq arr length)))

(define array-append
  (lambda (arr value) (send arr push value)))

(define array->string
  (lambda (arr) (send (slot-getq String fromCharCode) apply String arr)))

(define array-at ;; todo: need test
  (lambda (arr i) (slot-get arr i)))

(define set-array-at ;; todo: need test
  (lambda (arr i v) (slot-set! arr i v)))

;;; Function

(define apply ;; [r6rs(-)]
  (lambda (func args)
    (send func apply () (list->array args))))

;;; String

(define string? ;; [r6rs]
  (lambda (v) (instanceof v String)))

(define char->string ;; todo: rename it to string (r6rs) later.
  (lambda (c) (send String fromCharCode c)))

(define string-length ;; [r6rs]
  (lambda (str) (slot-getq str length)))

(define string-ref ;; [r6rs]
  (lambda (str i) (send str charCodeAt i)))

(define substring ;; [r6rs-lib]
  (lambda (str begin end)
    (send str substring begin end)))

(define string-append ;; [r6rs-lib(-)] todo: why not just +?
  (lambda (a b) (+ a b)))

(define string-join
  (lambda (lst delimiter) (send (list->array lst) join delimiter)))

;; Return a copy of the string witout extension.
(define trim-extension
  (lambda (string)
    (let ((reversed (drop-while
                     (lambda (c) (not (eq? c #".")))
                     (reverse (string->list string)))))
      (list->string
       (reverse (if (pair? reversed) (cdr reversed) reversed))))))

(define swap-extension
  (lambda (path extension)
    (string-append (trim-extension path) extension)))

(define string->list ;; [r6rs]
  (lambda (str)
    (let ((result ())
          (i (- (string-length str) 1)))
      (while (<= 0 i)
             (set! result (cons (string-ref str i) result))
             (set! i (- i 1)))
      result)))

(define list->array
  (lambda (lst)
    (let ((array (new Array))
          (i 0))
      (for-each (lambda (e)
                  (slot-set! array i e)
                  (set! i (+ i 1)))
                lst)
      array)))

(define array->list
  (lambda (arr)
    (let ((i (array-length arr))
          (lst ()))
      (while (< 0 i)
             (set! i (- i 1))
             (set! lst (cons (array-at arr i) lst)))
      lst)))

(define list->string
  (lambda (lst) (array->string (list->array lst))))

(define double->string to-string)
(define string->double parseFloat)
(define string->number Number)

(define string->number-radix
  (lambda (str radix) (int (parseInt str radix)))) ;; coerce signed int

(define string->number-radix-sign
  (lambda (str radix sign) (int (parseInt (+ sign str) radix)))) ;; coerce signed int

(define integer->string-base
  (lambda (base n) (send n toString base)))

(define ends-with
  (lambda (str postfix)
    (eq? (send str lastIndexOf postfix) (- (string-length str) (string-length postfix)))))

;;;; Byte Array -- wrapper functions to flash.utils.ByteArray
;;;; See bytes macro in core.k

(define bytes? (lambda (v) (instanceof v flash.utils/ByteArray))) ;; [plt]

(define make-bytes ;; [plt(-)]
  (lambda (size)
    (let ((bs (new flash.utils/ByteArray)))
      (slot-setq! bs length size)
      bs)))

(define bytes-ref array-at) ;; [plt]

(define bytes-length ;; [plt]
  (lambda (bs) (slot-getq bs length)))

(define list->bytes ;; [plt]
  (lambda (lst)
    (let ((bs (make-bytes 0)))
      (for-each
       (lambda (e) (send bs writeByte e))
       lst)
      bs)))

(define bytes->list ;; [plt]
  (lambda (bs)
    (let ((lst ())
          (i (bytes-length bs)))
      (while (< 0 i)
             (set! i (- i 1))
             (set! lst (cons (bytes-ref bs i) lst)))
      lst)))

(define string->bytes
  (lambda (str)
    (let ((bs (make-bytes 0)))
      (send bs writeUTFBytes str)
      bs)))

(define bytes->string
  (lambda (bs)
    (slot-setq! bs position 0)
    (send bs readUTFBytes (bytes-length bs))))

(define double->bytes
  (lambda (d)
    (let ((bs (make-bytes 0)))
      (slot-setq! bs endian (slot-getq flash.utils/Endian LITTLE_ENDIAN))
      (send bs writeDouble d)
      bs)))

(define bytes->double
  (lambda (bs)
    (slot-setq! bs endian (slot-getq flash.utils/Endian LITTLE_ENDIAN))
    (slot-setq! bs position 0)
    (send bs readDouble)))

(define real->floating-point-bytes double->bytes val) ;; [plt(-)]
(define floating-point-bytes->real bytes->double bstr) ;; [plt(-)]

;;;; Primitive functions

(define equal?
  (lambda (a b)
    (or (and (== a b) (=== (typeof a) (typeof b)))
	(and (pair? a)
             (pair? b)
             (and (equal? (car a) (car b))
                  (equal? (cdr a) (cdr b))))
        (and (array? a)
             (array? b)
             (equal? (array->list a) (array->list b)))
        (and (bytes? a)
             (bytes? b)
             (equal? (bytes->list a) (bytes->list b))))))

(define = equal?) ;; [cola]
(define != (lambda (a b) (not (= a b))))
(define else 't)
(define eq? (lambda (a b) (=== a b))) ;; [r6fs]

(define number? (lambda (n) (istype n Number))) ;; [r6rs]
(define long? (lambda (n) (istype n int)))
(define double? (lambda (n) (and (not (long? n)) (istype n Number))))
(define negative? (lambda (n) (< n 0))) ;; [r6rs]
(define positive? (lambda (n) (> n 0))) ;; [r6rs]
(define zero? (lambda (n) (=== n 0))) ;; [r6rs]

(define max (lambda (a b) (if (> a b) a b))) ;; [r6rs(-)]
(define min (lambda (a b) (if (< a b) a b))) ;; [r6rs(-)]
(define bitwise-and (lambda (a b) (& a b))) ;; [r6rs]
(define bitwise-ior (lambda (a b) (| a b))) ;; [r6rs]
(define add1 (lambda (n) (+ n 1))) ;; [plt]
(define number->string to-string)
(define + (lambda (a b) (+ a b)))
(define - (lambda (a b) (- a b)))
(define * (lambda (a b) (* a b)))
(define / (lambda (a b) (/ a b)))
(define log (slot-getq Math log)) ;; [r5rs]

(define arithmetic-shift
  (lambda (n m)
    (if (> m 0)
	(<< n m)
	(>> n (- 0 m)))))

;;;; Unit test
;;;; todo: Use macro. see ../boot/std.k

(define => ()) ; dummy definition for delimiter

(define %check-write
  (lambda (result expected)
    (trace "*FAILED*" " result: " result " expected: " expected)
    'FAIL))

(define check
  (lambda (result => expect)
    (if (equal? result expect)
	"ok"
	(%check-write result expect))))

;;;; Hash Map [plt]
;;;; This emulate hash map with an ActionScript object.
;;;; NOTE: The key is converted to String implicitly

(define-structure hash-map ())

(define make-hasheq new-hash-map)

(define make-immutable-hash ;; It is not immutable at all though.
  (lambda (ass)
    (let ((dict (make-hasheq)))
      (for-each (lambda (e) (slot-set! dict (car e) (cdr e))) ass)
      dict)))

(define hash-ref
  (lambda (dict key default)
    (let ((found (slot-get dict key)))
      (if (eq? found #undefined)
          (if (eq? default #undefined)
              (error "no value found for key: " key)
              default)
          found))))

(define hash-set!
  (lambda (dict key v)
    (slot-set! dict key v)
    v))

(define identity (lambda (x) x))

;; a convenience function to measure the runtime.
(define runtime
  (lambda (l)
    (let ((s (getTimer)))
      (l)
      (- (getTimer) s))))


;; hand written define-generic

(define ->list
  (lambda (obj)
    (if (null? obj)
        obj
        (send obj *m*-->list obj))))

(define-method ->list Object (lambda (obj) (error "cannot convert " obj " to list")))

; (define-method ->list typeof-undefined identity)
(define-method ->list typeof-pair      identity)

; makeshift for namespace
(define in (lambda (ns name) name))

; dummy
(define require (lambda (name)))

