;;; ActionScript Byte Code Cola Compiler
;;;
;;; Usage: [-S] [-abc] [-o outfile] [-x] [-r] filenames ...
;;;
;;; -S : Stop after the stage of compiler, and show an asselbler form.
;;; -abc : Stop after the stage of assembler, and show an ABC form.
;;; -o outfile : Place output in file outfile.
;;; -x : Execute the compiled program by avmshell.
;;; -r : Set runtime macro mode to generate macro definition in the abc.
;;; Object file name is decided based on last filename.

(require "boot/compile.k")
(require "boot/asm.k")
(require "boot/stream.k")
(require "boot/pretty-print.k")

(define execlp (extern "execlp" '(-> int (* char) ...)))
(define *stop-assembler* '())
(define *stop-abc* '())
(define *execute-after* '())
(define *execute-after-args* '())
(define *shell* "avmshell")
(define *outfile* '())

(pretty-print-columns 120)

(define abc-compile-file
  (lambda (paths)
    (let ((programs
           (map
            (lambda (path)
              (call-with-input-file path
                (lambda (input)
                  (->list (parse-stream peg/_sexprs input)))))
            paths)))
      (abc-compile (concatenate programs)))))
      
(define abc-cola-compiler
  (lambda (paths)
    (let ((asm (abc-compile-file paths)))
      (if *stop-assembler*
	  (pretty-print asm)
	  (let ((abc (from-asm asm)))
	    (if *stop-abc*
		(pretty-print abc)
                (let ((obj
                       (if *outfile*
                           *outfile*
                           (string-append (trim-extension (car (reverse paths))) ".abc"))))
                  (call-with-output-file obj
                    (lambda (port) (write-asm abc port)))
                  (if *execute-after*
                      (begin
                        (apply execlp `(,*shell* ,*shell* ,obj ,@*execute-after-args* 0))
                        (error "execlp call error"))))))))))

(let (programs)
  (while *arguments*
    (let ((arg (car *arguments*)))
      (cond
       ((= arg "-S") (set! *stop-assembler* 't))
       ((= arg "-abc") (set! *stop-abc* 't))
       ((= arg "-o")
        (begin
          (set! *outfile* (cadr *arguments*))
          (set! *arguments* (cdr *arguments*))))
       ((= arg "-x") (set! *execute-after* 't))
       ((= arg "-r") (set! *IS-RUNTIME-MACRO-COMPILE-MODE* #t))
       ((= arg "--")
        (begin
          (set! *execute-after-args* *arguments*)
          (set! *arguments* '("--"))))
       ('t (set! programs (cons arg programs)))))
    (set! *arguments* (cdr *arguments*)))
  (abc-cola-compiler (reverse programs)))
