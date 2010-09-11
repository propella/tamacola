;;;; PLT-scheme style port and simple object printer

(require "boot/stream.k")

(define for-each-index ;; todo: place better place, maybe std.k
  (lambda (func list)
    (let ((i 0))
      (while (pair? list)
             (func (car list) i)
             (set! i (+ i 1))
             (set! list (cdr list))))))

;;; Simple Printer

(define write) ;; forward

(define *write-number-long-base
  (lambda (i base out)
    (if (> i 0)
	(let ((next (/ i base)))
	  (*write-number-long-base next base out)
	  (let ((digit (- i (* next base))))
	    (byte-port-put out (if (< digit 10) (+ 48 digit) (+ 87 digit))))))))

(define write-number-long-base
  (lambda (i base out)
    (cond
     ((= i 0) (byte-port-put out #"0"))
     ((< i 0) (let ()
		  (byte-port-put out #"-")
		  (*write-number-long-base (- 0 i) base out)))
     ('else (*write-number-long-base i base out)))))

(define write-number-long
  (lambda (v out)
    (write-number-long-base v 10 out)))

(define write-string-quoted-hex ()) ; forward

(define write-string-quoted
  (lambda (v out)
    (byte-port-put out #"\"")
    (let ((i 0)
	  (length (string-length v)))
      (while (< i length)
	     (let ((c (string-ref v i)))
	       (cond
		((= c 7)  (write-string "\\a" out))
		((= c 8)  (write-string "\\b" out))
		((= c 9)  (write-string "\\t" out))
		((= c 10) (write-string "\\n" out))
		((= c 11) (write-string "\\v" out))
		((= c 12) (write-string "\\f" out))
		((= c 13) (write-string "\\r" out))
		((= c 27) (write-string "\\e" out))
		((= c 39) (write-string "\\'" out))
		((= c 34) (write-string "\\\"" out))
		((= c 92) (write-string "\\\\" out))
		((and (<= #" "  c) (<= c #"~")) (byte-port-put out c))
		('else (write-string-quoted-hex c out))))
	     (set! i (+ i 1))))
    (byte-port-put out #"\"")))

(define write-string-quoted-hex
  (lambda (char out)
    (cond
     ((< char 16) (write-string "\\x0" out))
     ('else       (write-string "\\x"out)))
    (write-number-long-base char 16 out)))

(define write-pair-cdr
  (lambda (v out)
    (cond
     ((not v) 'skip)
     ((pair? v)
      (let ()
	(byte-port-put out #" ")
	(write (car v) out)
	(write-pair-cdr (cdr v) out)))
     ('else
      (let ()
	(write-string " . " out)
	(write v out))))))

(define write-pair
  (lambda (v out)
    (byte-port-put out #"(")
    (write (car v) out)
    (write-pair-cdr (cdr v) out)
    (byte-port-put out #")")))

(define write-array
  (lambda (v out)
    (byte-port-put out #"[")
    (let ((i 0)
          (size (array-length v)))
      (while (< i size)
             (write (array-at v i) out)
             (and (< i (- size 1))
                  (byte-port-put out #" "))
             (set! i (+ i 1))))
    (byte-port-put out #"]")))

(define write-any
  (lambda (v out)
    (let ((type (typeof v))
          (fields (type-fields v)))
      (byte-port-put out #"{")
      (write-string (symbol->string (type-name v)) out)
      (write-string ": " out)
      (for-each-index
       (lambda (key i)
         (write-string (symbol->string key) out)
         (byte-port-put out #"=")
         (write (tuple-at v i) out)
         (and (< i (- (length fields) 1))
              (byte-port-put out #",")))
       fields)
      (byte-port-put out #"}")
)))

(define write-address
  (lambda (v out)
    (write-string "0x" out)
    (if (< v 0)
	(let ((h (^ (>> (^ v 0xffffffff) 16) 0xffff)) ; 16-31 bits
	      (l (& v 0xffff))) ; 0-15 bits
	  (write-number-long-base h 16 out)
	  (write-number-long-base l 16 out))
	(write-number-long-base v 16 out))))

(define write-pointer
  (lambda (v out)
    (write-string "*todo*" out)
;;     (write-string "#<" out)
;;     (write-address (pointer-address v) out)
;;     (byte-port-put out #"=")
;;     (write-string (oop@ v 4) out)
;;     (write (oop@ v 8) out)
;;     (byte-port-put out #">"))
))

(define write-stdout
  (lambda (v)
    (printf "%s" (call-with-output-string
		  (lambda (out) (write v out))))))

(define write
  (lambda (v . option)
    (if (not option)
	(write-stdout v)
	(let ((out (car option)))
	  (cond
	   ((not v) (write-string "()" out))
;	   ((= v *globals*) (write-string "*globals*" out))
	   ((pair? v) (write-pair v out))
	   ((symbol? v) (write-string (symbol->string v) out))
	   ((string? v) (write-string-quoted v out))
	   ((long? v) (write-number-long v out))
	   ((double? v) (write-string (double->string v) out))
	   ((pointer? v) (write-pointer v out))
	   ((array? v) (write-array v out))
	   ('else (write-any v out)))
	  v))))

;; Convert (long or float) to string
;; (number->string z [base])
(define number->string
  (lambda (z . option)
    (if (double? z)
	(let ()
	  (if option (error "float only be printed in base 10"))
	  (double->string z))
	(let ((base (if (car option) (car option) 10)))
	  (call-with-output-string
	   (lambda (out) (write-number-long-base z base out)))))))
