;; The tamacola shell
;; usage: tamacola [source ..] [-]
;; Source is filename, filename.k, or filename.abc.

(set! *IS-RUNTIME-MACRO-COMPILE-MODE* #t)

;;;; utility functions

(define file-exists?
  (lambda (path)
    (send avmplus/File exists path)))

;; Return true if path1 is newer than path2
(define file-is-newer?
  (lambda (path1 path2)
    (=== 0 (exec (+ "sh -c 'test " (+ path1 (+ " -nt " (+ path2 "'"))))))))

;;;; Re-define macro definition so that a source file which includes
;;;; macros genaretes also macro definition code in the binary.
    
;;;; Loading program

(define compile-file
  (lambda (path)
    (trace "compiling: " path)
    (call-with-string-input-file path
      (lambda (input)
        (let* ((exprs (->list (read* input)))
               (asm (abc-compile exprs))
               (bs (call-with-output-bytes
                    (lambda (port) (write-asm asm port))))
               (outfile (swap-extension path ".abc")))
          (bytes->file outfile bs)
          (load-byte-codes bs))))))

(define load-binary
  (lambda (path)
    (load-byte-codes (file->bytes path) ())))

;; Compile the module if necessary.
(define load-module
  (lambda (name)
    (let* ((src (+ name ".k"))
           (bin (+ name ".abc")))
      (if (or (not (file-exists? bin))
              (file-is-newer? src bin))
          (compile-file src)
          (load-binary bin)))))

(define load
  (lambda (name)
    (cond
     ((ends-with name ".k") (load-module (trim-extension name)))
     ((ends-with name ".abc") (load-binary name))
     ('else (load-module name))
     )))

;;;; Interactive shell

(define eval-stream
  (lambda (input)
    (let ((expr (->list (read* input))))
      (eval-all expr ()))))

(define repl
  (lambda ()
    (display "Cola/Tamarin\n> ")
    (while #t
      (try-catch e Object
        (let* ((input (read-line))
               (result (eval-stream (->stream input))))
          (if (=== result #undefined)
              '*do-nothing*
              (pretty-print result)))
        (begin
          (display
           (if (instanceof e Error)
               (send e getStackTrace)
               e))
          (display "\n")))
      (display "> "))))

(define do-repl
  (lambda ()
    (while (pair? *arguments*)
      (let ((arg (car *arguments*)))
        (set! *arguments* (cdr *arguments*))
        (cond
         ((= "-" arg) (repl))
         ('else (load arg)))))))

(if *arguments*
    (do-repl)
    (repl))
