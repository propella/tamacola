(define ->array (lambda (obj) (list->array (->list obj))))

(define-generic ->string (obj) (error "cannot convert " obj " to string"))

(define-method ->string String identity)
(define-method ->string <symbol> symbol->string)

(define-generic empty? (obj) (error "empty?: " obj " is not a collection"))

(define-method empty? typeof-null	(lambda (obj) #t))
(define-method empty? String      	(lambda (obj) (=== 0 (string-length obj))))
(define-method empty? typeof-pair 	(lambda (obj)))

(define-generic singleton? (obj) (error "singleton?: " obj " is not a collection"))

(define-method singleton? typeof-null 	(lambda (obj)))
(define-method singleton? String      	(lambda (obj) (=== 1 (string-length obj))))
(define-method singleton? typeof-pair   (lambda (obj) (null? (cdr obj))))

(define-generic first (obj) (error "first: " obj " is not a collection"))

(define-method first typeof-null	(lambda (obj)	(error "this list is empty")))
(define-method first String   	        (lambda (obj)	(string-ref obj 0)))
(define-method first typeof-pair      	(lambda (obj)	(car obj)))
(define-method ->list String 		string->list)

(define string-do
  (lambda (string function)
    (let ((i 0)
          (length (string-length string)))
      (while (< i length)
	(function (string-ref string i))
        (set! i (+ i 1))))))

;; (string-do "ABCDEF" print)

(define digit-value
  (lambda (c)
    (if (and (<= #"0" c ) (<= c #"9"))
	(- c #"0")
      (if (and (<= #"a" c ) (<= c #"z"))
	  (+ 10 (- c #"a"))
	(if (and (<= #"A" c ) (<= c #"Z"))
	    (+ 10 (- c #"A"))
            0)))))

;;;; List-stream

(define-structure list-stream (position))

(define list-stream-on
  (lambda (pair)
    (let ((s (new-list-stream)))
      (set-list-stream-position s pair)
      s)))

(define-method ->stream typeof-pair list-stream-on)
(define-method ->stream typeof-null list-stream-on)

(define-method peek typeof-list-stream
  (lambda (self)
    (if (list-stream-position self)
        (car (list-stream-position self))
        *end*)))

(define-method next typeof-list-stream
  (lambda (self)
    (if (list-stream-position self)
        (let ((head (list-stream-position self)))
          (set-list-stream-position self (cdr head))
          (car head))
        *end*)))

(define-method at-end? typeof-list-stream
  (lambda (self) (null? (list-stream-position self))))

(define-method position typeof-list-stream
  (lambda (self) (list-stream-position self)))

(define-method set-position typeof-list-stream
  (lambda (self position) (set-list-stream-position self position)))

(define-method up-to-end typeof-list-stream position) ;; [cola]

;;;; Token-group : token-group is used to keep contents between two
;;;; positions in a stream.

(define-structure token-group (body))

(define-method empty? typeof-token-group        (lambda (obj) (empty? (->list obj))))
(define-method singleton? typeof-token-group    (lambda (obj) (singleton? (->list obj))))
(define-method first typeof-token-group         (lambda (obj) (first (->list obj))))

(slot-setq! (slot-getq typeof-token-group prototype) toString
            (lambda () (+ "<token-string " (+ (String (->list #self)) ">"))))

(define token-group-new
  (lambda ()
    (let ((group (new-token-group)))
      (set-token-group-body group ())
      group)))

(define token-stream-group-from
  (lambda (stream start)
    (let ((group (token-group-new))
          (end (position stream)))
      (set-position stream start)
      (while (!= end (position stream))
             (token-group-append group (next stream)))
      group)))

(define token-group-append
  (lambda (self value)
    (set-token-group-body self (cons value (token-group-body self)))))

(define-method ->list typeof-token-group
  (lambda (self value)
    (reverse (token-group-body self))))

(define-method ->string typeof-token-group
  (lambda (self) (list->string (->list self))))

;;;; PEG Parser

(define-structure simple-parser (result))
(define-generic     parser-result (self)   (self " is not a parser"))
(define-generic set-parser-result (self x) (self " is not a parser"))

(define-method     parser-result typeof-simple-parser     simple-parser-result)
(define-method set-parser-result typeof-simple-parser set-simple-parser-result)

(define match-char
  (lambda (self parser c)
    (if (=== c (lookahead-char self))
        (begin (set-parser-result parser (get-char self))
               #t)
        #f)))

(define match-class
  (lambda (chars)
    (lambda (*stream* *parser*)
      (if (>= (send chars indexOf (send String fromCharCode (peek *stream*))) 0)
          (begin (set-parser-result *parser* (next *stream*))
                  #t)
            #f))))

;;;; Peg parser driver

;; Grammar-load read a grammar from the file, and define the parser.

;; (define grammar-load
;;   (lambda (file-name ns)
;;     (let ((source (stream/file-stream-open file-name "r")))
;;       (let ((*stream* (stream/token-stream-with-source source))
;; 	    (*parser* (new-simple-parser)))
;; 	(while (definition *stream* *parser*)
;; 	       (let ((result (parser-result *parser*)))
;; 		 (eval result ns))))
;;       (stream/file-stream-close source))))

(define parse-stream
  (lambda (rule *stream*)
    (let ((*parser* (new-simple-parser)))
      (if (rule *stream* *parser*)
	  (parser-result *parser*)
	  'FAIL))))

(define parse-collection
  (lambda (rule collection) (parse-stream rule (->stream collection))))

(define read ;; [r6rs-lib(-)]
  (lambda (input) (parse-stream peg/_sexpr input)))

;; Returns multiple expressions as long as available
(define read*
  (lambda (input) (parse-stream peg/_sexprs input)))

;;;; Memorization and left-recursion support.
;;;; To use these features, compiled the .k file generated from .g file
;;;; with peg-memo-macro.k.  See the infix.g example.
;;;; A few caveats:
;;;; - It only supports direct left recursion.
;;;; - to test such a rule, infix-test.k has a test line like this:
;;;;    (parse-collection in-start "1-2*3+4")
;;;;   The in-start rule is just calling the in-a rule but you need it to trigger the code in memo-RULE.
;;;;   If you just write the test:
;;;;    (parse-collection in-a "1-2*3+4")
;;;;   in-a function is called like a normal function without setting up the data structure around it and returns wrong result.
;;;;   Perhaps the right thing is to put a call to RULE or memo-RULE in parse-stream.

(define parser-memo
  (lambda (s p n)
    (let ((m (slot-get p (position s))))
      (if (eq? #undefined m)
      	  m
	  (slot-get m n)))))

(define parser-advance   
  (lambda (s p m)
    (set-position s (slot-get m 2))))

(define set-parser-memo
  (lambda (s p pos n v)
    (let ((dict (slot-get p pos)))
      (if (eq? #undefined dict)
	  (begin
	    (set! dict (new Object))
	    (slot-set! p pos dict))
	  '())
      (slot-set! dict n v)
      v)))

(class Failer (Object) (used)
       (slot-setq! #self used #f))

(define failer-used
  (lambda (obj)
    (slot-setq! obj used #t)
    #f))

(define memo-RULE
  (lambda (*stream* *parser* name func)
    (let ((memoRec (parser-memo *stream* *parser* name))
	  (ans #f)
	  (left #f))
      (if (eq? #undefined memoRec)
	  (let ((pos (position *stream*))
		 (failer (new Failer)))
	    (set! memoRec (set-parser-memo *stream* *parser* pos name failer))
	    (set! ans (func *stream* *parser*))
	    (set! memoRec (set-parser-memo *stream* *parser* pos name
					   (new Array ans (parser-result *parser*) (position *stream*))))
	    (if (slot-getq failer used)
		(let ((keepGoing #t)
		      (lastPos pos))
		  (set! left #t)
		  (while keepGoing
			 (begin
			   (set-position *stream* pos)
			   (set! ans (func *stream* *parser*))
			   (if (or (not ans) (<= (position *stream*) lastPos))
			       (set! keepGoing #f)
			       (begin 
				 (set! lastPos (position *stream*))
				 (set! memoRec (set-parser-memo *stream* *parser* pos name (new Array #t (parser-result *parser*) (position *stream*)))))))))
		'()))
	  '())
	  (if (instanceof memoRec Failer)
	      (failer-used memoRec)
	      (if (slot-get memoRec 0)
		(begin (parser-advance *stream* *parser* memoRec) (set-parser-result *parser* (slot-get memoRec 1)) #t)
		#f)))))

