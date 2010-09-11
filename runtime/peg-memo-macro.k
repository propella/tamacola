(define-form RULE (name . args)
  (if args
      `(let ((*stream* (list-stream-on (list . ,args))))
	 (,name *stream* *parser*))
      `(memo-RULE *stream* *parser* (quote ,name) ,name)))
