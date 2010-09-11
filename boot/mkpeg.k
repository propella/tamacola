(require "boot/peg.k")
(require "boot/pretty-print.k")

(define mkpeg
  (lambda (name input output grammar)
    (load grammar)
    (let* ((*stream* (file-stream-open input "r"))
           (*parser* (new-simple-parser)))
      (call-with-output-file output
        (lambda (out)
          (let ((result (parse-stream peg/definitions *stream*)))
            (let ((e `(library ,name ,@result)))
              (pretty-print e out)))))
      (file-stream-close *stream*))))

(let ((name ())
      (input ())
      (output ())
      (grammar "peg-boot.k"))
  (while *arguments*
    (let ((arg (car *arguments*)))
      (cond
       ((= arg "-n")
        (set! name (string->symbol (cadr *arguments*)))
        (set! *arguments* (cdr *arguments*)))
       ((= arg "-o")
        (set! output (cadr *arguments*))
        (set! *arguments* (cdr *arguments*)))
       ((= arg "-g")
        (set! grammar (cadr *arguments*))
        (set! *arguments* (cdr *arguments*)))
       ('else 
        (set! input arg)))
      (set! *arguments* (cdr *arguments*))))
  (if (null? input)
      (error "usage: mkpeg [ -n library-name ] [ -o output.k ] [ -g grammar.k ] input.g")
      (mkpeg name
             input
             (or output
                 (string-append (trim-extension input) ".k"))
             grammar)))
