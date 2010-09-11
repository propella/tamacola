;; AVM2 assembler / disassembler

(require "boot/asm.k")
(require "boot/check.k")
(require "abcsx/test.scm")

;;; read a S-expression file
(define read-file
  (lambda (infile)
    (let ((fp (fopen infile "r")))
      (let ((exp (read fp)))
	(fclose fp)
	exp))))

;;; write a ABC file
(define write-file
  (lambda (asm outfile)
    (call-with-output-file outfile
      (lambda (port)
	(write-asm asm port)))))

;; Run the test
(define test
  (lambda (x xx)
    (run-test)))

(run *arguments*)
(set! *arguments* ())
