;; A calculator interactive demo

;;;; GUI

(define *stage* ())
(define *input* ())
(define *output* ())

(define display
  (lambda (str)
    (send *output* appendText str)
    (slot-setq! *output* scrollV (slot-getq *output* maxScrollV))))
    
(define newline
  (lambda ()
    (display "\n")))

(define listener
  (lambda (e)
    (if (== (slot-getq e keyCode) 13)
	(let (dummy)
	  (run (slot-getq *input* text))
	  (slot-setq! *input* text ""))
	())))

(class calcFlash (flash.display/Sprite) (format)
       (constructsuper)
       (set! *stage* #self)

       (set! format (new flash.text/TextFormat))
       (slot-setq! format size 24)

       (set! *output* (new flash.text/TextField))
       (slot-setq! *output* x 10)
       (slot-setq! *output* y 10)
       (slot-setq! *output* width 280)
       (slot-setq! *output* height 330)
       (slot-setq! *output* border 1)
       (slot-setq! *output* multiline 1)
       (slot-setq! *output* defaultTextFormat format)

       (set! *input* (new flash.text/TextField))
       (slot-setq! *input* type (slot-getq flash.text/TextFieldType INPUT))
       (slot-setq! *input* x 10)
       (slot-setq! *input* y 350)
       (slot-setq! *input* width 280)
       (slot-setq! *input* height 40)
       (slot-setq! *input* border 1)
       (slot-setq! *input* defaultTextFormat format)
       (slot-setq! *input* text "(3 + 4) * 6")

       (send #self addChild *output*)
       (send #self addChild *input*)
       (send (slot-getq #self parent) addEventListener "keyDown" listener)
)

;;;; A parser accepts a string.
;;;; Return (result . rest-of-string) or () if fail

;; Always success with the value

(define success
  (lambda (v)
    (lambda (line) (cons v line))))

(check ((success "a") "xxx") => (cons "a" "xxx"))

;; (char "a") matches if the first character is "a".

(define char
  (lambda (char)
    (lambda (line)
      (if (< (string-length line) 1) ()
	  (if (equal? (substring line 0 1) char)
	      (cons char (substring line 1 (string-length line)))
	      ())))))

(check ((char "a") "a__") => (cons "a" "__"))

;; Combine two parsers sequentially.
;; next is a function where the argument is result of the first
;; parser, and the value is a parser.

(define match-seq
  (lambda (parser next)
    (lambda (line)
      (let ((result (parser line)))
	(if (null? result)
	    ()
	    ((next (car result)) (cdr result)))))))

(check ((match-seq (char "a") (lambda (x) (success "ok"))) "abc") => (cons "ok" "bc"))

;; Ordered choice combinator

(define match-or
  (lambda (p1 p2)
    (lambda (line)
      (let ((result (p1 line)))
	(if (null? result)
	    (p2 line)
	    result)))))

(check ((match-or (char "a") (match-or (char "b") (char "c"))) "a__") => (cons "a" "__"))
(check ((match-or (char "a") (match-or (char "b") (char "c"))) "c__") => (cons "c" "__"))
(check ((match-or (char "a") (match-or (char "b") (char "c"))) "d__") => ())

;; Repeat a parser. Results are processed in foldl fashion.
;; func's first argument is result of the last iteration (initial value is z).
;; func's second argument is this result of the parser.

(define match-foldl
  (lambda (parser z func)
    (match-or (match-seq parser (lambda (x) (match-foldl parser (func z x) func)))
	      (success z))))

(check ((match-foldl (char "a") () cons) "aaa__") => (cons (cons (cons (cons () "a") "a") "a") "__"))

;; Repeat a parser.
;; It returns a list of all results.

(define match-many
  (lambda (p)
    (match-or (match-seq p
      (lambda (x) (match-seq (match-many p)
        (lambda (xs) (success (cons x xs))))))
	  (success ()))))

(check ((match-many (char "a")) "aaa__") => (cons (cons "a" (cons "a" (cons "a" ()))) "__"))

;; Combine two parsers. Only last result is returned.

(define match-and
  (lambda (p1 p2) (match-seq p1 (lambda (v) p2))))

(check ((match-and (char "a") (match-and (char "b") (char "c"))) "abc__") => (cons "c" "__"))

;; Match end of string

(define eof
  (lambda (line)
    (if (< 0 (string-length line)) () (cons () ""))))

(check ((match-and (char "a") eof) "a") => (cons () ""))

;; Simple tokens

(define *digit
  (match-or (char "0")
  (match-or (char "1")
  (match-or (char "2")
  (match-or (char "3")
  (match-or (char "4")
  (match-or (char "5")
  (match-or (char "6")
  (match-or (char "7")
  (match-or (char "8")
	    (char "9")))))))))))

(define digit
  (match-seq *digit (lambda (v) (success (string->number v)))))

(check (digit "777__") => (cons 7 "77__"))
(check (digit "abc__") => ())

(define number
  (match-seq digit
    (lambda (z) (match-foldl digit z
      (lambda (l r) (+ (* l 10) r))))))

(check (number "1234567__") => (cons 1234567 "__"))

(define space
  (match-or (char " ")
  (match-or (char "\t")
  (match-or (char "\n")
	    (char "\r")))))

(check (space " __") => (cons " " "__"))

;; (token parser) matches parser trailing whilte spaces.

(define token
  (lambda (parser)
    (match-seq parser
	       (lambda (x)
		 (match-and (match-many space)
			    (success x))))))

(define tchar (lambda (c) (token (char c))))

(check ((tchar "a" ) "a   __") => (cons "a" "__"))

;;;; Calculator

(define expr (lambda (line) (add line)))
(define calc (match-seq expr (lambda (x) (match-and eof (success x)))))

(define prim
  (match-or (match-and (tchar "(")
		       (match-seq expr
				  (lambda (x) (match-and (tchar ")") (success x)))))
	    (token number)))

(define mul
  (match-seq prim (lambda (z)
		    (match-foldl
		     (match-or
		      (match-seq (match-and (tchar "*") prim) (lambda (n) (success n)))
		      (match-seq (match-and (tchar "/") prim) (lambda (n) (success (/ 1 n)))))
		     z
		     (lambda (l r) (* l r))))))

(define add
  (match-seq mul (lambda (z)
		   (match-foldl
		    (match-or
		     (match-seq (match-and (tchar "+") mul) (lambda (n) (success n)))
		     (match-seq (match-and (tchar "-") mul) (lambda (n) (success (- 0 n)))))
		    z
		    (lambda (l r) (+ l r))))))

(check (expr "3 + 4") => (cons 7 ""))
(check (expr "3 + 4 + 5") => (cons 12 ""))
(check (expr "4 - 3") => (cons 1 ""))
(check (expr "3 * 4") => (cons 12 ""))
(check (expr "10 / 2") => (cons 5 ""))
(check (expr "3 + 4 * 5") => (cons 23 ""))
(check (expr "(3 + 4) * 6") => (cons 42 ""))
(check (expr "3 ") => (cons 3 ""))
(check (expr "(3)") => (cons 3 ""))

(define run
  (lambda (str)
    (let ((result (calc str)))
      (display str)
      (display " =\n")
      (if (null? result)
	  (display "Syntax error.")
	  (display (car result)))
      (newline))))
