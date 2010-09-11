(define ruby-reserved-words
  '(BEGIN class ensure  nil self when END def false not super while alias defined? for or then yield and do if redo true begin else in rescue undef break elsif module retry unless case end next return until))

(define ruby-reservedq
  (lambda (*stream* *parser*)
     (let ((sym (next *stream*)))
       (if (memq? sym ruby-reserved-words)
 	  (begin
 	    (set-parser-result *parser* sym) #t)
 	  #f))))

(define ruby-isUppercase
  (lambda (*stream* *parser*)
     (let* ((sym (next *stream*))
     	    (c (send (->string sym) charCodeAt 0)))
       (if (and (<= #"A" c) (<= c #"Z"))
 	  (begin
 	    (set-parser-result *parser* sym) #t)
 	  #f))))

(define ssp
  (lambda (*stream* *parser*)
    (trace "pos(stream)=" (position *stream*))
    #t))

;(define ruby-reservedq
;  (lambda (sym)
;      (trace "sym" sym)
;      (memq? sym ruby-reserved-words)))

(define *parser-stack*
  '(()))

(define var-in-scope
  (lambda (*stream* *parser*)
    (let ((sym (next *stream*)))
      (if (memq? sym (car *parser-stack*))
	  (begin
	    (set-parser-result *parser* sym) #t)
	  #f))))

(define push-scope
  (lambda (*stream* *parser*)
    (set! *parser-stack* (cons '() *parser-stack*))
    #t))

(define pop-scope
  (lambda (*stream *parser*)
    (set-parser-result *parser* (car *parser-stack*))
    (set! *parser-stack* (cdr *parser-stack*))
    #t))

(define add-var-to-scope
  (lambda (*stream* *parser*)
    (let ((sym (next *stream*))
          (head (car *parser-stack*)))
      (if (memq? sym head)
          '()
          (begin
            (set! head (cons sym head))
	    (set! *parser-stack* (cons head (cdr *parser-stack*)))))
      (set-parser-result *parser* sym))
    #t))

(define lset-difference
  (lambda (long short)
    (let ((ret '()))
      (map (lambda (e) (if (memq? e short) '() (set! ret (cons e ret)))) (reverse long))
      ret)))

;;(trace (modulo-list '(a b c d) '(b d)))
;; (push-scope)
;; (add-var-to-scope 'a)
;; (add-var-to-scope 'b)
;; (trace *parser-stack*)
;; (pop-scope)
;; (trace *parser-stack*)
;; (push-scope)
;; (add-var-to-scope 'a)
;; (add-var-to-scope 'b)
;; (trace *parser-stack*)
;; (push-scope)
;; (add-var-to-scope 'a)
;; (trace *parser-stack*)
;; (add-var-to-scope 'c)
;; (trace *parser-stack*)
