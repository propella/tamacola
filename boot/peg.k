;; peg.k -- PEG helper functions

;; Copyright (c) 2010 Ian Piumarta, Takashi Yamamiya
;; All Rights Reserved

;; Permission is hereby granted, free of charge, to any person obtaining a
;; copy of this software and associated documentation files (the 'Software'),
;; to deal in the Software without restriction, including without limitation
;; the rights to use, copy, modify, merge, publish, distribute, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, provided that the above copyright notice(s) and this
;; permission notice appear in all copies of the Software.  Inclusion of the
;; the above copyright notice(s) and this permission notice in supporting
;; documentation would be appreciated but is not required.

;; THE SOFTWARE IS PROVIDED 'AS IS'.  USE ENTIRELY AT YOUR OWN RISK.

(require "boot/stream.k")

(define identity (lambda (x) x))

(define-generic ->list (obj)	(error "cannot convert " obj " to list"))
(define-generic ->array (obj)	(error "cannot convert " obj " to array"))
(define-generic ->string (obj)	(error "cannot convert " obj " to string"))
(define-generic ->stream (obj)	(error "cannot convert " obj " to stream"))

(define-generic empty? (obj) (error "empty?: " obj " is not a collection"))
(define-generic singleton? (obj) (error "singleton?: " obj " is not a collection"))

(define-generic first (obj) (error "first: " obj " is not a collection"))

(define-method ->list typeof-undefined identity)
(define-method ->list typeof-pair      identity)

(define-method ->string typeof-string identity)

(define-method empty? typeof-undefined (lambda (obj) 't))
(define-method empty? typeof-string    (lambda (obj) (= 0 (strlen obj))))
(define-method empty? typeof-pair      (lambda (obj)))

(define-method singleton? typeof-undefined (lambda (obj)))
(define-method singleton? typeof-string    (lambda (obj) (= 1 (strlen obj))))
(define-method singleton? typeof-pair      (lambda (obj) (nil? (cdr obj))))

(define-method first typeof-undefined (lambda (obj)	(error "this list is empty")))
(define-method first typeof-string    (lambda (obj)	(_peek (addrof obj))))
(define-method first typeof-pair      (lambda (obj)	(car obj)))

;; todo: makeshift. A byte stream (input-byte) is used for string string.

(define string-stream-with-string
  (lambda (str) (open-input-byte (string->bytes str))))

(define open-string-input-port string-stream-with-string) ;; [r6rs]
(define-method ->stream typeof-string string-stream-with-string)

;;;;;;;

(define-pattern ((incr lval) (set! lval (+ lval 1))))
(define-pattern ((decr lval) (set! lval (- lval 1))))

(define array-append
  (lambda (array obj)
    (array-insert-at array (array-length array) obj)))

(define array-append-all
  (lambda (array objs)
    (while (pair? objs)
      (array-insert-at array (array-length array) (car objs))
      (set! objs (cdr objs)))))

(define array-append-bytes
  (lambda (array obj)
    (let ((ptr (addrof obj)))
      (let ((len (strlen ptr))
	    (idx 0))
	(while (< idx len)
	  (array-append array (_peek (+ ptr idx)))
	  (incr idx))
	array))))

(define array->string
  (lambda (array)
    (let ((len (array-length array)))
      (let ((string (new typeof-string (+ len 1))))
	(let ((ptr (addrof string))
	      (idx 0))
	  (while (< idx len)
	    (poke (+ ptr idx) (array-at array idx))
	    (incr idx))
	  (poke (+ ptr idx) 0))
	string))))

(define string->array
  (lambda (string)
    (let ((arr (array)))
      (let ((ptr (addrof string)))
	(while (!= 0 (_peek ptr))
	  (array-append arr (_peek ptr))
	  (incr ptr)))
      arr)))

(define-method ->array typeof-string string->array)

(define string-do
  (lambda (string function)
    (let ((ptr (addrof string))
	  (c))
      (while (!= 0 (set! c (_peek ptr)))
	(function c)
	(incr ptr)))))

(define digit-value
  (lambda (c)
    (if (and (<= #"0" c ) (<= c #"9"))
	(- c #"0")
      (if (and (<= #"a" c ) (<= c #"z"))
	  (+ 10 (- c #"a"))
	(if (and (<= #"A" c ) (<= c #"Z"))
	    (+ 10 (- c #"A"))
            0)))))

;;;;;;;;;

  (define-structure list-stream (position))

  (define list-stream-on
    (lambda (pair)
      (let ((s (new-list-stream)))
	(set! (list-stream-position s) pair)
	s)))

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
    (lambda (self) (== () (list-stream-position self))))

  (define-method position typeof-list-stream
    (lambda (self) (list-stream-position self)))

  (define-method set-position typeof-list-stream
    (lambda (self n) (set-list-stream-position self n)))

(define-method ->stream typeof-pair list-stream-on)
(define-method ->stream typeof-undefined list-stream-on)

(define-structure token-group (body))

(define-method empty? typeof-token-group        (lambda (obj) (empty? (->list obj))))
(define-method singleton? typeof-token-group    (lambda (obj) (singleton? (->list obj))))
(define-method first typeof-token-group         (lambda (obj) (first (->list obj))))

(define token-group-new
  (lambda ()
    (let ((group (new-token-group)))
      (set-token-group-body group ())
      group)))

(define token-group-append
  (lambda (self value)
    (set-token-group-body self (cons value (token-group-body self)))))

(define token-stream-group-from
  (lambda (stream start)
    (let ((group (token-group-new))
          (end (position stream)))
      (set-position stream start)
      (while (!= end (position stream))
             (token-group-append group (next stream)))
      group)))

(define-method ->list typeof-token-group
  (lambda (self value)
    (reverse (token-group-body self))))

(define-method ->array typeof-token-group
  (lambda (self value)
    (list->array (->list self))))

(define-method ->string typeof-token-group
  (lambda (self) (list->string (->list self))))

;;;; PEG Parser

(define-generic     parser-result (self)   (self " is not a parser"))
(define-generic set-parser-result (self x) (self " is not a parser"))

(define-structure simple-parser (result))
(define-method     parser-result typeof-simple-parser     simple-parser-result)
(define-method set-parser-result typeof-simple-parser set-simple-parser-result)

(define match-char
  (lambda (self *parser* c)
    (if (= c (peek self))
        (set-parser-result *parser* (next self)))))

(define match-class
  (lambda (chars)
    (let ((class (array))
          (ptr (addrof chars))
          (c 0))
      (while (!= 0 (set! c (_peek ptr)))
        (set-array-at class c 't)
        (incr ptr))
      (lambda (s p)
        (let ((_c_ (peek s)))
          (and (!= _c_ *end*)
               (array-at class _c_)
               (set-parser-result p (next s))))))))

(define-pattern ((UP-TO-END)    (->string (up-to-end *stream*))))
(define-pattern ((RETURN value) (begin (set-parser-result *parser* value) #t)))
(define-pattern ((ATEND?)       (at-end? *stream*)))
(define-pattern ((PEEK)	        (peek *stream*)))
(define-pattern ((NEXT)	        (next *stream*)))
(define-pattern ((RESULT)       (parser-result *parser*)))
(define-pattern ((SAVE . exprs) (let ((_p_ (position *stream*))) . exprs)))
(define-pattern ((FAIL)	        (begin (set-position *stream* _p_) ())))
(define-pattern ((SUCCEED)      (begin (set-position *stream* _p_) #t)))
(define-pattern ((GROUP expr)   (let ((_g_ (token-group-new))) (and expr (RETURN _g_)))))
(define-pattern ((APPEND expr)  (and expr (begin (token-group-append _g_ (parser-result *parser*)) #t))))
(define-pattern ((ANY)	        (and (not (ATEND?)) (RETURN (NEXT)))))
(define-pattern ((OBJ expr)     (and (= expr (PEEK)) (RETURN (NEXT)))))

(define-pattern ((SEQ expr)     (SAVE (let ((_a_ expr))
                                        (let ((_i_ 0)
                                              (_l_ (array-length _a_)))
                                          (while (and (< _i_ _l_) (= (array-at _a_ _i_) (PEEK)))
                                                 (RETURN (NEXT))
                                                 (incr _i_))
                                          (or (= _i_ _l_) (FAIL)))))))

(define-pattern ((CLASS chars)	((match-class chars) *stream* *parser*)))

(define-pattern ((PRED expr)    (SAVE (if expr (SUCCEED) #f))))
(define-pattern ((NOT expr)     (SAVE (if expr (FAIL) #t))))
(define-pattern ((AND . exprs)  (SAVE (or (and . exprs) (FAIL)))))
(define-pattern ((OR . exprs)   (or . exprs)))
(define-pattern ((STRUCT expr)  (let ((token (PEEK)))
                                  (and (or (pair? token) (nil? token))
                                       (let (_result_)
                                         (let ((*stream* (->stream token)))
                                           (set! _result_ expr))
                                         (and _result_ (begin (NEXT) #t)))))))
(define-pattern ((OPT expr)     (GROUP (begin (APPEND expr) #t))))
(define-pattern ((MANY expr)    (GROUP (begin (while (APPEND expr)) #t))))
(define-pattern ((MANY1 expr)   (GROUP (and (APPEND expr) (begin (while (APPEND expr)) #t)))))
(define-pattern
  ((RULE name)        (name *stream* *parser*))
  ((RULE name . args) (let ((*stream* (list-stream-on (list . args))))
                        (name *stream* *parser*))))

(define-pattern ((STORE id expr) (and expr (begin (set! id (RESULT)) #t))))
(define-pattern ((SOURCE expr)  (SAVE (and expr (RETURN (token-stream-group-from *stream* _p_))))))
(define-pattern ((STRING expr)  (and expr (RETURN (->string (RESULT))))))
(define-pattern ((NUMBER r expr) (and expr (RETURN (string->number-radix (->string (RESULT)) r)))))

(define-pattern ((define-rule id vars expr)
  (define id (lambda (*stream* *parser*) (let vars expr)))))

;;;; Peg parser driver
(define peg (namespace global 'peg))
(in peg
  (define definition (lambda () (error "peg/definition is not implemented yet"))) ; forward
  (define definitions (lambda () (error "peg/definitions is not implemented yet"))) ; forward
)
;; Grammar-load read a grammar from the file, and define the parser.

(define grammar-load
  (lambda (file-name ns)
    (let ((path (or (find-load-file-name file-name)
		    (error "Grammar not found: " file-name))))
      (let ((*stream* (file-stream-open path "r"))
            (*parser* (new-simple-parser)))
          (while (peg/definition *stream* *parser*)
                 (let ((result (parser-result *parser*)))
                   ;;                 (println result)
                   (eval result ns)))
        (file-stream-close *stream*)))))

(define parse-stream
  (lambda (rule *stream*)
    (let ((*parser* (new-simple-parser)))
      (if (rule *stream* *parser*)
	  (parser-result *parser*)
	  'FAIL))))

(define parse-collection
  (lambda (rule collection) (parse-stream rule (->stream collection))))

(define run-parser
  (lambda (grammar rule doit)
    (let ((*stream* (->stream grammar))
	  (*parser* (new-simple-parser)))
      (while (rule *stream* *parser*)
	(let ((result (parser-result *parser*)))
	  (if doit (eval result global)))))))
