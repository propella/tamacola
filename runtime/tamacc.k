;;; ActionScript Byte Code Cola Compiler
;;;
;;; Usage: [-S] [-abc] [-o outfile] [-x] [-r] filenames ...
;;;
;;; -S : Stop after the stage of compiler, and show an asselbler form.
;;; -abc : Stop after the stage of assembler, and show an ABC form.
;;; -o outfile : Place output in file outfile.
;;; -x : Execute the compiled program by avmshell.
;;; -r : Set runtime macro mode to generate macro definition in the abc.
;;;
;;; The output file name is decided based on last filename.  File
;;; names are either cola source or abc binary. When you use -x
;;; option, abc file names are passed to avmshell, otherwise, abcs are
;;; ignored.
;;;
;;; Note: Whole process should be reconsidered. -x option execute a
;;; program without any useful environment, but apparently it is
;;; useful to execute a program in the compiler's context with dynamic
;;; code loading.

(define *stop-assembler* '())
(define *stop-abc* '())
(define *execute-after* '())
(define *execute-after-args* '())
(define *outfile* '())
(define *shell* "avmshell")
(define *abcfiles* '()) ;; abc files executed before output shwn -x is specified.
(define *optimize* #f)

(pretty-print-columns 120)

(define abc-compile-file
  (lambda (paths)
    (let ((programs
           (map
            (lambda (path)
              (call-with-string-input-file
               path
               (lambda (input)
                 (->list (parse-stream peg/_sexprs input)))))
            paths)))
      (abc-compile (concatenate programs)))))
      
(define abc-cola-compiler
  (lambda (paths)
    (let ((asm (abc-compile-file paths)))
      (if *optimize*
      	  (set! asm (parse-collection pstart (cons asm '())))
	  '())
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
                        (let* ((cmd (string-join `("avmshell"
                                                   ,@*abcfiles*
                                                   ,obj
                                                   ,@*execute-after-args*)
                                                 " "))
                               (return (exec cmd)))
                          (exit return)))
                      '*do-nothing*))))))))

(let (programs)
  (while *arguments*
    (let ((arg (car *arguments*)))
      (cond
       ((= arg "-S") (set! *stop-assembler* 't))
       ((= arg "-abc") (set! *stop-abc* 't))
       ((= arg "-O") (set! *optimize* #t))
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
       ((ends-with arg ".abc")
        (set! *abcfiles* (cons arg *abcfiles*)))
       ('t (set! programs (cons arg programs)))))
    (set! *arguments* (cdr *arguments*)))
  (if (pair? programs)
      (abc-cola-compiler (reverse programs))
      (begin (println "usage: tamacc [-S] [-abc] [-x] [-o] cola source or abc files ... ")
             (exit 1))))
