(set! read-s32 read-u32) ;; Tamacola only supports signed integer

(define display print)

;;; read a S-expression file
(define read-file
  (lambda (infile)
    (call-with-string-input-file infile
      (lambda (input)
        (let* ((exp (read input)))
          (if (= exp 'FAIL)
              (error "Can not parse assembler file in" infile)
              exp))))))

;;; write a ABC file
(define write-file
  (lambda (asm outfile)
    (call-with-output-file outfile
      (lambda (port) (write-asm asm port)))))

;; Run the test
(define test
  (lambda (x xx) (run-test)))

(run *arguments*)
