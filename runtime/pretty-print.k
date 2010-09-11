;;; A PLT-Scheme style pretty printer
;;;
;;; Usage
;;; (pretty-print-on object out) : Print an object to output
;;; (pretty-print object) : Print an object
;;; (set-pretty-print-columns number) : Set column number
;;; (pretty-print-columns) : Get column number
;;;
;;; Example: (pretty-print (function-body load))

(define *indent* 1)
(define *pretty-print-columns* 80)

;;; Pretty Pprinter

(define set-pretty-print-columns
  (lambda (n)
    (set! *pretty-print-columns* n)
    *pretty-print-columns*))

(define pretty-print-columns
  (lambda () *pretty-print-columns*))

(define-generic *pretty-print-on (self indent out) (write-on self out))
(define-method *pretty-print-on typeof-null (lambda (self indent out) (write-on self out)))
(define-method *pretty-print-on Boolean (lambda (self indent out) (write-on self out)))
(define-method *pretty-print-on String (lambda (self indent out) (write-on self out)))
(define-method *pretty-print-on <symbol> (lambda (self indent out) (write-on self out)))
(define-method *pretty-print-on Number (lambda (self indent out) (write-on self out)))
(define-method *pretty-print-on Array (lambda (self indent out) (write-on self out)))

(define write-indent
  (lambda (i out)
    (while (< 0 i)
	   (next-put out #" ")
	   (set! i (- i 1)))))

(define pretty-print-cdr
  (lambda (v indent out)
    (cond
     ((null? v) 'skip)
     ((pair? v)
      (begin
	(next-put out #"\n")
	(write-indent indent out)
	(*pretty-print-on (car v) indent out)
	(pretty-print-cdr (cdr v) indent out)))
     ('else
      (begin
	(next-put-all out " . ")
	(*pretty-print-on v indent out))))))

(define *pretty-print-pair-on
  (lambda (v indent out)
    (next-put out #"(")
    (*pretty-print-on (car v) (+ indent *indent*) out)
    (pretty-print-cdr (cdr v) (+ indent *indent*) out)
    (next-put out #")")))

(define-method *pretty-print-on typeof-pair
  (lambda (v indent out)
    (let ((s (open-output-string)))
      (write-on v s)
      (let ((length (position s)))
        (next-put-all out
                      (if (> (+ indent length) (pretty-print-columns))
                          (call-with-output-string
                           (lambda (s2)
                             (*pretty-print-pair-on v indent s2)))
                          (get-output-string s)))))))

(define-method *pretty-print-on Object
  (lambda (v indent out)
    (let ((fields (type-fields v)))
      (next-put out #"{")
      (next-put-all out (symbol->string (type-name v)))
      (next-put-all out ":")
      (let ((lst (map (lambda (field) (slot-get v field)) fields)))
        (pretty-print-cdr lst indent out))
      (next-put out #"}"))))

(define pretty-print-on
  (lambda (v out)
    (*pretty-print-on v 0 out)
    (next-put out #"\n")))

(define pretty-print
  (lambda (v)
    (display (call-with-output-string
            (lambda (out) (pretty-print-on v out))))))
