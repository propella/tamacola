;;; A PLT-Scheme style pretty printer
;;;
;;; Usage
;;; (pretty-print object [output-port]) : Print an object
;;; (pretty-print-columns number) : Set column number
;;;
;;; Example: (pretty-print (function-body load))

(require "boot/writer.k")

(define *indent* 1)

;;; Pretty Pprinter

(define *pretty-print-columns* 80)

(define pretty-print-columns
  (lambda option
    (if option
	(set! *pretty-print-columns* (car option)))
    *pretty-print-columns*))

(define *pretty-print) ;; forward

(define write-indent
  (lambda (i out)
    (while (< 0 i)
	   (byte-port-put out #" ")
	   (set! i (- i 1)))))

(define pretty-print-cdr
  (lambda (v indent out)
    (cond
     ((not v) 'skip)
     ((pair? v)
      (byte-port-put out #"\n")
      (write-indent indent out)
      (*pretty-print (car v) indent out)
      (pretty-print-cdr (cdr v) indent out))
     ('else
      (write-string " . " out)
      (*pretty-print v indent out)))))

(define *pretty-print-pair
  (lambda (v indent out)
    (byte-port-put out #"(")
    (*pretty-print (car v) (+ indent *indent*) out)
    (pretty-print-cdr (cdr v) (+ indent *indent*) out)
    (byte-port-put out #")")))

(define pretty-print-pair
  (lambda (v indent out)
    (let ((pos (position out)))
      (write v out)
      (let ((length (- (position out) pos)))
	(if (> (+ indent length) (pretty-print-columns))
	    (let ()
	      (set-position out pos) ; rewind
	      (*pretty-print-pair v indent out)))))))

(define pretty-print-any
  (lambda (v indent out)
    (write-string "<type=" out)
    (write (typeof out) out)
    (write-string ">" out)))

(define *pretty-print
  (lambda (v indent out)
    (cond
     ((pair? v) (pretty-print-pair v indent out))
     ((or (not v)
	  (string? v)
	  (symbol? v)
	  (long? v)
	  (array? v)
	  (double? v)) (write v out))
;     ('else (pretty-print-any v indent out)))
     ('else (write v out))) ; todo
    ))

(define pretty-print) ;; forward
(define pretty-print-stdout
  (lambda (v)
    (printf "%s" (call-with-output-string
		  (lambda (out) (pretty-print v out))))))

;; Pretty-prints the value v using the same printed form as write, but
;; with newlines and whitespace inserted to avoid lines longer than
;; (pretty-print-columns)

;; (pretty-print v [port])
(define pretty-print
  (lambda (v . option)
    (if option
	(let ()
	  (*pretty-print v 0 (car option))
	  (byte-port-put (car option) #"\n"))
	(pretty-print-stdout v))
    ))
