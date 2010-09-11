(require "boot/peg.k")
(require "boot/peg-grammar.k")
(require "boot/check.k")

;;;; With raw macro

;; Match a symbol

(define-rule SYMBOL1 () (OBJ 'hello))
(check (parse-collection SYMBOL1 '(hello world)) => 'hello)

;; Match by predicate (function)

(define test-predicate
  (lambda (func)
    (lambda (*stream* *parser*)
      (and (func (stream/peek *stream*))
	   (stream/set-parser-result *parser* (stream/next *stream*))))))

(check (parse-collection (test-predicate long?) '(1 2)) => 1)
(check (parse-collection (test-predicate long?) '(hello world)) => 'FAIL)

;; Match by predicate (form)

(define-form TEST-PREDICATE (func)
  `(if (,func (stream/peek *stream*))
	(begin (stream/set-parser-result *parser* (stream/next *stream*))
	       't)
	'()))

(define-rule IS-LONG () (TEST-PREDICATE long?))
(define-rule IS-STRING () (TEST-PREDICATE string?))
(define-rule IS-NIL () (TEST-PREDICATE nil?))

(check (parse-collection IS-LONG '(1 2)) => 1)
(check (parse-collection IS-LONG '(hello world)) => 'FAIL)
(check (parse-collection IS-STRING '("hello" world)) => "hello")
(check (parse-collection IS-NIL '(() world)) => '())
(check (parse-collection IS-NIL '(world)) => 'FAIL)

;; Match many

(define list-many
  (lambda (*stream* *parser*) ; define-rule
    (let ((token (stream/peek *stream*))) ; STRUCT - PEEK
      (and (or (pair? token) (nil? token)) ; STRUCT
           (let (_result_) ; STRUCT
             (let ((*stream* (->stream token))) ; STRUCT
               (set! _result_ ; MANY - GROUP
                    (let ((_g_ (stream/token-group-new))) ; MANY - GROUP
                      (and
                       (begin (while (APPEND (ANY))) 't) ; MANY
                       (begin (set-parser-result *parser* _g_) 't))))) ; GROUP - RETURN
             (and
              _result_ ; STRUCT
              (begin (stream/next *stream*) 't))))))) ; STRUCT - NEXT

(check (->list (parse-collection list-many '((1 2 3)))) => '(1 2 3))
(check (->list (parse-collection list-many '(()))) => '())

;;;; With PEG grammar

(grammar-load "struct.g" global)

(check (parse-collection SYMBOL '(hello world)) => 'hello)
(check (parse-collection SYMBOL '(bye world)) => 'FAIL)

;; Match any list

(check (->list (parse-collection LIST-MANY '((1 2 3)))) => '(1 2 3))
(check (->list (parse-collection LIST-MANY '((1 2)))) => '(1 2))
(check (->list (parse-collection LIST-MANY '((1)))) => '(1))
(check (->list (parse-collection LIST-MANY '(()))) => '())

;; Match struct

(check (parse-collection NESTED '(((foo)))) => '((foo)))
(check (parse-collection NESTED '((foo))) => '(foo))
(check (parse-collection NESTED '(foo)) => 'foo)

;; Rule with arguments

(check (->list (parse-collection MYRULE '((first second) third))) => '(first second))
