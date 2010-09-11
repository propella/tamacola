;;; Test for PEG helper functions except grammar itself

(require "boot/peg.k")
(require "boot/check.k")

(define src "hello")
(define dst ())
(string-do src
 (lambda (c) (set! dst (cons c dst))))
(check dst => '(111 108 108 101 104))

(check (digit-value #"0") => 0)
(check (digit-value #"1") => 1)
(check (digit-value #"a") => 10)
(check (digit-value #"F") => 15)
(check (digit-value #"?") => 0)

;;;; Stream and token

;; token-group simple usage
(in stream
(let ((group (token-group-new)))
  (token-group-append group #"A")
  (token-group-append group #"B")
  (token-group-append group #"C")
  (check (->list group) => '(#"A" #"B" #"C"))
  (check (->string group) => "ABC")
  )
    
;; token-group with token-stream
(let ((token-st (open-string-input-port "ABCDE")))
  (next token-st) ;; skip A
  (let ((start (position token-st)))
    (next token-st) ;; skip B
    (next token-st) ;; skip C
    (next token-st) ;; skip D
    (let ((g (token-stream-group-from token-st start)))
      (next token-st) ;; skip E
      (check (->list g) => '(#"B" #"C" #"D")))
    ))
)

;;;; Parser in function

;; A parser function accepts *stream* and *parser* (result) as arguments.
;; If a rule is matched, the function returns true value.
;; If a rule is matched, the result is saved to (parser-result *parser*).
;; If a rule is not matched, the function returns '().

;; Return 'Hello for anything

(define hello (lambda (s p) (set-parser-result p "Hello") #t))
(check (parse-collection hello "Any String") => "Hello")

;; Match A then return 'ok
(define match-A
  (lambda (*stream* *parser*)
    (if (= #"A" (peek *stream*))
	(begin (next *stream*)
               (set-parser-result *parser* "ok")
               #t)
	())))

(check (parse-collection match-A "Apple") => "ok")
(check (parse-collection match-A "Orange") => 'FAIL)

;; Match B then return 'ok (with define-form and define-rule)
(define-pattern ((MATCH-B)
                 (if (= #"B" (peek *stream*))
                     (begin (next *stream*)
                            (set-parser-result *parser* "ok")
                            #t)
                     ())))

(define-rule match-B () (MATCH-B))

(check (parse-collection match-B "Boy") => "ok")
(check (parse-collection match-B "Girl") => 'FAIL)

;; Match C by predefined function. ;; todo: this braeks with define-structure
(define match-C (lambda (s p) (match-char s p #"C")))

(check (parse-collection match-C "Cat") => #"C")
(check (parse-collection match-C "Dog") => 'FAIL)

;;;; Parser in define-rule and define-form

(define-pattern ((MATCH-CHAR c) (match-char *stream* *parser* c)))

(define-rule match-d () (MATCH-CHAR #"D"))
(check (parse-collection match-d "Dog") => #"D")
(check (parse-collection match-d "Cat") => 'FAIL)

;;;; Parser in define-rule and predefined rules
;;;; define-rule binds *parser* and *stream*

;; Return 'Hello for anything.
(define-rule hello () (RETURN "Hello"))
(check (parse-collection hello "Any String") => "Hello")

;; . : Match anything
(define-rule anything () (ANY))
(check (parse-collection anything "Anything") => #"A")

;; char = "H" : Match a character
(define-rule one-char () (OBJ #"H"))
(check (parse-collection one-char "HELLO") => #"H")
(check (parse-collection one-char "BYE") => 'FAIL)

;; char = "HE" : Match a character
(define-rule two-chars () (SEQ [#"H" #"E"]))
(check (parse-collection two-chars "HELLO") => #"E")
(check (parse-collection two-chars "BYE") => 'FAIL)

;; Not
(define-rule not-test () (AND (OBJ #"A") (NOT (OBJ #"B"))))
(check (parse-collection not-test "AB") => 'FAIL)
(check (parse-collection not-test "AA") => #"A")

;; char = 'H' -> 'ok : Match a character and return something
(define-rule one-char-return () (AND (OBJ #"H")
				     (RETURN "ok")))
(check (parse-collection one-char-return "HELLO") => "ok")
(check (parse-collection one-char-return "hello") => 'FAIL)

;; digit = [0123456789] : Match one of characters by function
(check (parse-collection (match-class "0123456789") "123") => #"1")

;; digit = [0123456789] : Match one of characters
(define-rule digit () (CLASS "0123456789"))
(check (parse-collection digit "123") => #"1")

;; many = [A]* : Return many characters
(define-rule many () (MANY (CLASS "A")))
(check (->list (parse-collection many "AABBCC")) => (list #"A" #"A"))

;; opt = digit | many : Return matched first choice
(define-rule opt () (OR (RULE digit)
			(RULE many)))
(check (->list (parse-collection opt "AABBCC")) => (list #"A" #"A"))
(check (parse-collection opt "123") => #"1")

;; text = [helo]* $ : Return a text
(define-rule text () (STRING (MANY (CLASS "helo"))))
(check (parse-collection text "hello") => "hello")

;; number = digit $ $#10 : Convert to a number
(define-rule my-number () (NUMBER 10 (STRING (MANY1 (RULE digit)))))
(check (parse-collection my-number "42") => 42)

;; triplet = (digit digit digit) @ : Return a list
(define-rule triplet () (SOURCE (AND (RULE digit) (RULE digit) (RULE digit))))
(check (->list (parse-collection triplet "123")) => '(49 50 51))

;; makelist = [abc] :x -> `(head ,x)
(define-rule makelist (x) (AND 
			   (STORE x (CLASS "abc"))
			   (RETURN (list 'head x))))
(check (parse-collection makelist "a") => '(head #"a"))

;; sym = [abc]* @$:i -> (intern i) :  Return a symbol

(define-rule sym (i) (AND (STORE i (STRING (SOURCE (MANY (CLASS "abc")))))
			  (RETURN (intern i))))
(check (parse-collection sym "aabbcc") => 'aabbcc)

;;;; List

(define-rule SYMBOL () (AND (OBJ (quote hello)) (RETURN (quote hello))))
(define-rule LIST-MANY () (STRUCT (MANY (ANY))))
(define-rule NESTED (e) (OR (AND (OBJ (quote foo)) (RETURN (quote foo))) (AND (STRUCT (STORE e (RULE NESTED))) (RETURN (list e)))))
;(define-rule MYRULE (e) (AND (STORE e (ANY)) (AND (PUSH e) (RULE LIST-MANY))))
(define-rule MYRULE (e) (AND (STORE e (ANY)) (RULE LIST-MANY e)))

(check (parse-collection SYMBOL '(hello world)) => 'hello)
(check (parse-collection SYMBOL '(bye world)) => 'FAIL)

;; Match any list

(check (->list (parse-collection LIST-MANY '((1 2 3)))) => '(1 2 3))
(check (->list (parse-collection LIST-MANY '((1 2)))) => '(1 2))
(check (->list (parse-collection LIST-MANY '((1)))) => '(1))
(check (->list (parse-collection LIST-MANY '(()))) => '())

;; Nested struct

(check (parse-collection NESTED '(((foo)))) => '((foo)))
(check (parse-collection NESTED '((foo))) => '(foo))
(check (parse-collection NESTED '(foo)) => 'foo)

;; Rule with arguments

(check (->list (parse-collection MYRULE '((first second) third))) => '(first second))
(check (parse-collection MYRULE '(zero third)) => 'FAIL)
