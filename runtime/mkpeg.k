;; Make syntax.k from syntax.g

(define mkpeg
  (lambda (name infile outfile)
    (call-with-string-input-file infile
      (lambda (input)
        (call-with-string-output-file outfile
          (lambda (output)
            (next-put-all output "(library ")
            (write-on name output)
            (next-put-all output "\n\n")
            (for-each
             (lambda (e) (pretty-print-on e output))
             (->list (parse-stream peg/definitions input)) output)
            (next-put-all output "\n)\n")
            ))))))

(let ((name ())
      (input ())
      (output ()))
  (while *arguments*
    (let ((arg (car *arguments*)))
      (cond
       ((= arg "-n")
        (set! name (string->symbol (cadr *arguments*)))
        (set! *arguments* (cdr *arguments*)))
       ((= arg "-o")
        (set! output (cadr *arguments*))
        (set! *arguments* (cdr *arguments*)))
       ('else
        (set! input arg))))
    (set! *arguments* (cdr *arguments*)))
  (if (null? input)
      (error "usage: mkpeg [ -n library-name ] [ -o output.k ] input.g")
      (mkpeg name
             input
             (or output
                 (string-append (trim-extension input) ".k")))))
