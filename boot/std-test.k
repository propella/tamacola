(require "boot/std.k")

;;;; Equality

(check (equal? 1 1) => #t)
(check (equal? '(1 2 3) '(1 2 3)) => #t)
(check (equal? [1 2 3] (list->array '(1 2 3))) => #t)
(check (equal? (list (list->array '(1 2 3))) '([1 2 3])) => #t)

;;;; Numbers

(check (string->number-radix "1" 10) => 1)
(check (string->number-radix "999" 10) => 999)
(check (string->number-radix "fff" 16) => 4095)
(check (string->number-radix-sign "7fffffff" 16 "") => 2147483647)
(check (string->number-radix-sign "7fffffff" 16 "-") => -2147483647)
(check (string->number-radix-sign "80000000" 16 "") => -2147483648)
(check (string->number-radix-sign "80000000" 16 "-") => -2147483648) ;; strange?
(check (string->double "3.14") => 3.14)

;;;; List enumeration

(check (list 5 4 3 2 1) => (reverse (list 1 2 3 4 5)))

(check (filter (lambda (x) (== (& x 1) 0)) (list 3 1 4 5 9 2 6)) => (list 4 2 6))

(check (map (lambda (e) (* e 2)) (list 1 2 3 4)) => (list 2 4 6 8))

(check (map-with-index
        (lambda (e i) (+ e i)) (list 1 2 3 4)) => (list 1 3 5 7))

(check (iota 5) => '(1 2 3 4 5))
(check (build-list 5 (lambda (x) (* x 2))) => '(2 4 6 8 10))

(check (find negative? '(3 1 -4 1 5 9)) => -4)
(check (find negative? '(3 1 5 1 5 9)) => #f)

(check (delete 3 '(1 2 3 4)) => '(1 2 4))
(check (delete 3 '()) => '())

(check (fold-right cons '() '(1 2 3 4 5)) => '(1 2 3 4 5))
(check (assoc 2 '((1 . a) (2 . b) (3 . c))) => '(2 . b))
(check (assq 2 '((1 . a) (2 . b) (3 . c))) => '(2 . b))

(check (append '(1) '(2 3) '(4 5 6)) => '(1 2 3 4 5 6))

(check (append-map
	(lambda (x) (list x (+ x 1)))
	'(1 3 5 7)) => '(1 2 3 4 5 6 7 8))

(check (append-map
	(lambda (x) (list x x))
	'(1 2 3 4)) => '(1 1 2 2 3 3 4 4))

(check (concatenate '((1) (2 3) (4 5 6))) => '(1 2 3 4 5 6))

;;;; Array

(check (array->list (array 1 2 3)) => '(1 2 3))
(check (array 1 2 3) => [1 2 3])
(check (array->list (array 1 2 3 4 5 6 7)) => '(1 2 3 4 5 6 7))
(check (array 1 2 3 4 5 6 7) => [1 2 3 4 5 6 7])
(check (array->list (list->array '(1 2 3 4 5 6 7))) => '(1 2 3 4 5 6 7))
(check (list->array (array->list [1 2 3])) => [1 2 3]) ;todo: bug???

;;;; Byte Strings

(assert (= '(#x9a #x99 #x99 #x99 #x99 #x99 #xb9 #x3f) (bytes->list (double->bytes 0.1))))

(assert (= 0.1 (bytes->double (bytes #x9a #x99 #x99 #x99 #x99 #x99 #xb9 #x3f))))

(let ((b (make-bytes 5)))
  (assert (= '(0 0 0 0 0) (bytes->list b)))
  (bytes-set! b 0 65)
  (assert (= '(65 0 0 0 0) (bytes->list b)))
  (bytes-set! b 1 66)
  (bytes-set! b 2 67)
  (assert (= "ABC" (bytes->string b)))
  (let ((b2 (make-bytes 6)))
    (bytes-copy! b2 1 b)
    (assert (= '(0 65 66 67 0 0) (bytes->list b2))))
  (let ((b3 (make-bytes 3)))
    (bytes-copy! b3 1 b)
    (assert (= '(0 65 66) (bytes->list b3))))
)

(assert (= '(0 1 2 3 4) (bytes->list (bytes 0 1 2 3 4))))
(assert (= "ABCDE" (bytes->string (bytes 65 66 67 68 69))))
(assert (= '(1 2) (bytes->list (subbytes (bytes 0 1 2 3 4) 1 3))))
(assert (= '(104 101 108 108 111) (bytes->list (string->bytes "hello"))))

;;;; Misc

(check (substring "Apple" 1 3) => "pp")
(check (substring "Apple" 0 3) => "App")
(check (substring "Apple" 0 0) => "")
(check (substring "Apple" 0 5) => "Apple")
(check (substring "Apple" 4 5) => "e")
(check (substring "Apple" 5 5) => "")

(check (trim-extension "file.txt") => "file")
(check (trim-extension "file") => "")
(check (trim-extension ".file") => "")
(check (trim-extension "../file.txt") => "../file")

(check (list->bytes '(1 2 3 4 5)) => (bytes 1 2 3 4 5))
(check (bytes->list (bytes 1 2 3 4 5)) => '(1 2 3 4 5))

(check (list->string '(65 66 67 68 69)) => "ABCDE")
(check (string->list "ABCDE") => '(65 66 67 68 69))

;; (check (string-copy! "hello" 1 "ELL") => "hELLo")
;; (check (string-copy! "hello" 4 "O WORLD") => "hellO WORLD") ; overrun
;; (check (string-copy! "hello" 0 "01234" 1 3) => "12llo")

;; Macro

(define my-swap!
  (form (lambda (compiler name a b)
          `(let ((temp ,a))
             (set! ,a ,b)
             (set! ,b temp)))))
          
(let ((a 1) (b 2))
  (my-swap! a b)
  (check (cons a b) => '(2 . 1)))

(assert (= (macroexpand-1 (my-swap! x y)) '(let ((temp x)) (set! x y) (set! y temp))))

;;; Macroexpand-1

;; ;; todo: some very wrong bug in check?
;; (check (macroexpand-1
;;           (let* ((a 1) (b a)) (print a b)))
;;        => '(let ((a 1)) (let* ((b a)) (print a b))))

(assert (= (macroexpand-1
           (let* ((a 1) (b a)) (print a b)))
          '(let ((a 1)) (let* ((b a)) (print a b)))))

(check (not 1) => ())
(check (not ()) => 't)

(check (let ((x 1)) (begin (set! x 2) x)) => 2)

;;;; quasiquote

(check (qq-object 1) => '(quote 1))
(check (qq-object '(1 2)) => '(cons (quote 1) (cons (quote 2) (quote ()))))
(check (qq-object '(1 . 2)) => '(cons (quote 1) (quote 2)))
(check (qq-object '((unquote a) (unquote b))) => '(cons a (cons b (quote ()))))
(check (qq-object '((unquote a) . (unquote b))) => '(cons a b))
(check (qq-object '((unquote-splicing lst) 1 2))
       => '(concat (->list lst) (cons (quote 1) (cons (quote 2) (quote ())))))

(check (qq-object '(1 (unquote a) 2)) => '(cons (quote 1)
                                                (cons a
                                                      (cons (quote 2)
                                                            (quote ())))))

(check (quote `(symbol ,value ,@group (inside list)))
       => '(quasiquote (symbol
                        (unquote value)
                        (unquote-splicing group)
                        (inside list))))

(check (qq-object '(symbol
                        (unquote value)
                        (unquote-splicing group)
                        (inside list)))
       => '(cons (quote symbol)
                 (cons value
                       (concat (->list group)
                               (cons (cons (quote inside)
                                           (cons (quote list) (quote ())))
                                     (quote ()))))))

(check (let ((value "value")
             (group '(1 2 3)))
         `(symbol ,value ,@group (inside list)))
       => '(symbol "value" 1 2 3 (inside list)))

;;;; Structure

(define-structure triple (first second third))
(let ((t (new-triple)))
  (set-triple-first t "hello")
  (check (triple-first t) => "hello")
  (check (typeof t) => typeof-triple)
;  (check (send t toString) => "<triple>")
  (check (type-name t) => 'triple)
  (check (type-fields t) => '(first second third))
)

(define-structure dog ())
(define-structure cat (favorite))

(define-generic say (self some) (concat-string some ", hola"))

(define-method say typeof-dog
  (lambda (self some) (concat-string some ", bowwow")))

(define-method say typeof-cat
  (lambda (self some) (concat-string (concat-string (cat-favorite self) some) ", meow")))

(check (say 0 "apple") => "apple, hola")

(check (say (new-dog) "bone") => "bone, bowwow")
(let ((cat (new-cat)))
  (set-cat-favorite cat "fish")
  (check (say cat "cake") => "fishcake, meow"))

;; Namespaces

(define lib1 (namespace global 'lib1))
(define lib2 (namespace global 'lib2))

(library lib1
  (define hello "lib1/hello"))

(library lib2
  (define hello "lib2/hello"))

(check lib1/hello => "lib1/hello")
(check lib2/hello => "lib2/hello")
