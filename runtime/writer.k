;;;; PLT-scheme style port and simple object printer

;(require "boot/stream.k")

(define-generic write-on (self out) (error "write-on is not defined in " self))

(define-method write-on typeof-null
  (lambda (v out) (next-put-all out "()")))

(define-method write-on Boolean
  (lambda (v out) (next-put-all out (if v "#t" "#f"))))

(define-method write-on <symbol>
  (lambda (v out) (next-put-all out (to-string v))))

(define-method write-on Number
  (lambda (v out) (next-put-all out (to-string v))))

(define-method write-on Error
  (lambda (v out)
    (next-put out #"{")
    (next-put-all out (String v))
    (next-put out #"}")))

(define-method write-on String
  (lambda (v out)
    (next-put out #"\"")
    (let ((i 0)
	  (length (string-length v)))
      (while (< i length)
	     (let ((c (string-ref v i)))
	       (cond
		((=== c 7)  (next-put-all out "\\a"))
		((=== c 8)  (next-put-all out "\\b"))
		((=== c 9)  (next-put-all out "\\t"))
		((=== c 10) (next-put-all out "\\n"))
		((=== c 11) (next-put-all out "\\v"))
		((=== c 12) (next-put-all out "\\f"))
		((=== c 13) (next-put-all out "\\r"))
		((=== c 27) (next-put-all out "\\e"))
		((=== c 34) (next-put-all out "\\\""))
		((=== c 92) (next-put-all out "\\\\"))
;		((and (<= #" "  c) (<= c #"~")) (next-put out c))
		((<= #" "  c) (next-put out c))
		('else (write-string-quoted-hex-on c out))))
	     (set! i (+ i 1))))
    (next-put out #"\"")))

(define write-string-quoted-hex-on
  (lambda (char out)
    (cond
     ((< char 16) (next-put-all out "\\x0"))
     ('else       (next-put-all out "\\x")))
    (next-put-all out (integer->string-base 16 char))))

(define write-pair-cdr-on
  (lambda (v out)
    (cond
     ((null? v) 'skip)
     ((pair? v)
      (begin
	(next-put out #" ")
	(write-on (car v) out)
	(write-pair-cdr-on (cdr v) out)))
     ('else
      (begin
	(next-put-all out " . ")
	(write-on v out))))))

(define-method write-on typeof-pair
  (lambda (v out)
    (next-put out #"(")
    (write-on (car v) out)
    (write-pair-cdr-on (cdr v) out)
    (next-put out #")")))

(define-method write-on Array
  (lambda (v out)
    (next-put out #"[")
    (let ((i 0)
          (size (array-length v)))
      (while (< i size)
             (write-on (array-at v i) out)
             (and (< i (- size 1))
                  (next-put out #" "))
             (set! i (+ i 1))))
    (next-put out #"]")))

(define-method write-on Object
  (lambda (v out)
    (let ((fields (type-fields v)))
      (next-put out #"{")
      (next-put-all out (symbol->string (type-name v)))
      (next-put-all out ":")
      (let ((lst (map (lambda (field) (slot-get v field)) fields)))
        (write-pair-cdr-on lst out))
      (next-put out #"}"))))

(define write ;; [r6rs-lib(-)]
  (lambda (v)
    (display (call-with-output-string
            (lambda (out) (write-on v out))))))
