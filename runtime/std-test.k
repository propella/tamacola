;; Macroexpand

(check (macroexpand-1 (and 1 2 3)) => '(let ((**cond** 1)) (if **cond** (and 2 3) **cond**)))
(check (macroexpand-1 (in namespace exp1 exp2)) => '(let () exp1 exp2))

;; Boolean values
;; NOTE: the condition is converted to Boolean type by ecmascript's convention.

(check (if #f 1 2) => 2)
(check (if () 1 2) => 2)
(check (if #undefined 1 2) => 2)
(check (if 0 1 2) => 2)
(check (if "" 1 2) => 2)

(check (and) => #t)
(check (and 1) => 1)
(check (and () 1) => ())
(check (and 1 2 3) => 3)
(check (and 1 2 () 4) => ())
(check (and 1 2 3 4) => 4)

(check (or) => #f)
(check (or 7) => 7)
(check (or () 1) => 1)
(check (or () #f 3) => 3)

(check (let* ((a 1)
              (b (* a 2))
              (c (* b 2))) c) => 4)

;;;; let

(check (let (x y z)
         (set! x 1)
         (set! y 2)
         (set! z 3)
         (list x y z)) => (list 1 2 3))

(check (let ((a (let ((a 1)) (+ a 1)))
             (b (let ((a 10)) (+ a 1))))
         a) => 2)

;;;; Structure

(check (type-name "hello") => 'string)
(check (type-fields "hello") => ())

(define-structure triple (first second third))
(let ((t (new-triple)))
  (set-triple-first t "hello")
  (check (triple-first t) => "hello")
  (check (instanceof t typeof-triple) => #t)
  (check (send t toString) => "<triple>")
  (check (type-name t) => 'triple)
  (check (type-fields t) => '(first second third))
  )

(define-structure dog ())
(define-structure cat (favorite))

(define-generic say (self some) (+ some ", hola"))
(define-method say typeof-dog (lambda (self some) (+ some ", bowwow")))
(define-method say typeof-cat (lambda (self some) (+ (+ (cat-favorite self) some) ", meow")))

(check (say 0 "apple") => "apple, hola")

(check (say (new-dog) "bone") => "bone, bowwow")
(let ((cat (new-cat)))
  (set-cat-favorite cat "fish")
  (check (say cat "cake") => "fishcake, meow"))

;;;; Symbol

(check (=== (intern "hello") (intern "hello")) => #t)
(check (=== (intern "hello") (intern "hell_")) => #f)
(check (=== (list->string (list 65 66 67)) "ABC") => #t)
(check (=== (intern (list->string (list 65 66 67))) (intern "ABC")) => #t)
(check (symbol->string (string->symbol "hello")) => "hello")
(check (send (intern "hello") toString) => "hello")
(check (concat-symbol 'hello 'world) => 'helloworld)
(check (symbol->string (intern "toString")) => "toString")

;;;; Numbers

(check (string->number-radix "1" 10) => 1)
(check (string->number-radix "999" 10) => 999)
(check (string->number-radix "fff" 16) => 4095)
(check (string->number-radix-sign "7fffffff" 16 "") => 2147483647)
(check (string->number-radix-sign "7fffffff" 16 "-") => -2147483647)
(check (string->number-radix-sign "80000000" 16 "") => -2147483648)
(check (string->number-radix-sign "80000000" 16 "-") => -2147483648) ;; strange?
(check (string->double "3.14") => 3.14)

;;;; List

(check (pair? '(1)) => #t)
(check (pair? ()) => #f)

(check (pair->string (cons 1 2)) =>  "(1 . 2)")

(check (list 1 2 3) => (cons 1 (cons 2 (cons 3 ()))))

(check (concat (list 1 2 3) (list)) => (list 1 2 3))
(check (concat (list) (list 4 5)) => (list 4 5))
(check (concat (list 1 2 3) (list 4 5)) => (list 1 2 3 4 5))

(check (length (list 1 2 3)) => 3)

(check (memq 2 '(1 2 3 4 5)) => '(2 3 4 5))
(check (memq 10 '(1 2 3 4 5)) => #f)

;;;; List enumeration

(check (list 5 4 3 2 1) => (reverse (list 1 2 3 4 5)))

(check (member 1 (list 1 2 3 4 5)) => (list 1 2 3 4 5))
(check (member 5 (list 1 2 3 4 5)) => (list 5))
(check (member 0 (list 1 2 3 4 5)) => #f)

(check (map (lambda (e) (* e 2)) (list 1 2 3 4)) => (list 2 4 6 8))

(check (map-with-index
        (lambda (e i) (+ e i)) (list 1 2 3 4)) => (list 1 3 5 7))

(check (take '(1 2 3 4 5) 0) => '())
(check (take '(1 2 3 4 5) 3) => '(1 2 3))
(check (take '(1 2 3 4 5) 6) => '(1 2 3 4 5))

(check (drop '(1 2 3 4 5) 0) => '(1 2 3 4 5))
(check (drop '(1 2 3 4 5) 3) => '(4 5))
(check (drop '(1 2 3 4 5) 6) => '())

(check (iota 5) => '(1 2 3 4 5))
(check (build-list 5 (lambda (x) (* x 2))) => '(2 4 6 8 10))

(check (find negative? '(3 1 -4 1 5 9)) => -4)
(check (find negative? '(3 1 5 1 5 9)) => #f)

(check (delete 3 '(1 2 3 4)) => '(1 2 4))
(check (delete 3 '()) => '())

(check (fold-right cons '() '(1 2 3 4 5)) => '(1 2 3 4 5))

(check (filter (lambda (x) (== (& x 1) 0)) (list 3 1 4 5 9 2 6)) => (list 4 2 6))

(check (assoc 2 '((1 . a) (2 . b) (3 . c))) => '(2 . b))
(check (assoc 2 '()) => #f)
(check (assq 2 '((1 . a) (2 . b) (3 . c))) => '(2 . b))
(check (assq 2 '()) => #f)

;(check (append '(1) '(2 3) '(4 5 6)) => '(1 2 3 4 5 6))

(check (append-map
	(lambda (x) (list x (+ x 1)))
	'(1 3 5 7)) => '(1 2 3 4 5 6 7 8))

(check (append-map
	(lambda (x) (list x x))
	'(1 2 3 4)) => '(1 1 2 2 3 3 4 4))

(check (concatenate '((1) (2 3) (4 5 6))) => '(1 2 3 4 5 6))

;;;; String

(check (string-length "Hello, World!") => 13)
(check (string-ref "ABC" 1) => #"B")
(check (string->list "ABC") => (list 65 66 67))
(check (string->list "ABC") => (list #"A" #"B" #"C"))
(check (list->string (list #"A" #"B" #"C")) => "ABC")
(check (string-join (list "Hello," "World!" "more!!") " ") => "Hello, World! more!!")

(check (number->string 42) => "42")
(check (integer->string-base 10 42) => "42")
(check (integer->string-base 16 42) => "2a")
(check (integer->string-base 8 42) => "52")
(check (integer->string-base 2 42) => "101010")
(check (integer->string-base 16 0xffff) => "ffff")
(check (integer->string-base 16 0x7fffffff) => "7fffffff")
(check (number->string 0xffffffff) => "-1")
(check (number->string 3.14) => "3.14")

(check (trim-extension "file.txt") => "file")
(check (trim-extension "file") => "")
(check (trim-extension ".file") => "")
(check (trim-extension "../file.txt") => "../file")

(check (ends-with "hello.txt" ".txt") => #t)
(check (ends-with "hello.txt.gz" ".txt") => #f)
(check (ends-with "hello world" ".txt") => #f)

;;;; Array

(check (array->list (array 1 2 3)) => '(1 2 3))
(check [1 2 3] => [1 2 3])
(check (array-at [1 2 3] 1) => 2)
(let ((a [1 2 3]))
  (set-array-at a 1 "hello")
  (check a => [1 "hello" 3]))

(check (list->array (list 1 2 3)) => [1 2 3])
(check (array->list [1 2 3]) => '(1 2 3))

;;;; Function

(check (apply cons '(1 2)) => (cons 1 2))

;;;; Quote

(check 'hello => (intern "hello"))
(check '1 => 1)
(check '1.2 => 1.2)
(check '() => ())
(check '(1 2) => (list 1 2))
(check '(1 . 2) => (cons 1 2))
(check '("hello" "world") => (list "hello" "world"))
(check '((1) (2 3)) => (list (list 1) (list 2 3)))
(check '(hello world) => (list 'hello 'world))

;;;; Quasi Quote

(check (let ((value "value")
             (group (list 1 2 3))
             (other 7)
             (num 1))
         value)
       => "value")

(check (let ((value "value")
             (group '(1 2 3)))
         `(symbol ,value ,@group (inside list)))
       => '(symbol "value" 1 2 3 (inside list)))

;;;; Tiny functions

(check (number? 1) => #t)
(check (number? "hello") => #f)

(check (long? 1) => #t)
(check (long? 1.1) => #f)
(check (long? "hello") => #f)

(check (double? 1) => #f)
(check (double? 1.1) => #t)
(check (double? "hello") => #f)

(check (long? 1) => #t)
(check (long? 1.0) => #t) ; this is too tricky!!!
(check (long? "hello") => #f)

;;;; Byte Array

(check (bytes-length (make-bytes 0)) => 0)
(check (bytes->list (bytes 1 2 3 4 5)) => '(1 2 3 4 5))
(check (list->bytes '(1 2 3 4 5)) => (bytes 1 2 3 4 5))

(check (bytes->string (bytes 65 66 67 68 69)) => "ABCDE" )
(check (string->bytes "ABCDE") => (bytes 65 66 67 68 69))

(check (double->bytes 0.1) => (bytes #x9a #x99 #x99 #x99 #x99 #x99 #xb9 #x3f))
(check (bytes->double (bytes #x9a #x99 #x99 #x99 #x99 #x99 #xb9 #x3f)) => 0.1)
