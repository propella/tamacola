;;;; Platform dependent functions for avmshell

;;;; System Interface

(define exit ;; [r6rs-lib(-)]
  (lambda (n) (send avmplus/System exit n)))

(define *arguments* (array->list (slot-getq avmplus/System argv)))

(define exec (lambda (cmd) (send avmplus/System exec cmd)))

(define load-byte-codes
  (lambda (bytes callback)
    (let ((result (send
               (slot-getq avmplus/Domain currentDomain)
               loadBytes
               bytes)))
      (if callback
          (callback result)
          result))))

;;;; File Input / Output

;; Binary input
(define file->bytes
  (lambda (path)
    (send avmplus/File fileToByteArray path #t)))

;; Binary input ;; [r6rs-lib(-)]
(define call-with-input-file
  (lambda (path func)
    (let* ((bs (file->bytes path))
           (s (->stream bs)))
      (func s))))

;; String input
(define call-with-string-input-file
  (lambda (path func)
    (let* ((cs (send avmplus/File read path))
           (s (->stream cs)))
      (func s))))

;; Binary output
(define bytes->file
  (lambda (path bs)
    (send avmplus/File writeByteArray path bs)))

;; Binary output ;; [r6rs-lib]
(define call-with-output-file
  (lambda (path func)
    (let ((bs (call-with-output-bytes func)))
      (bytes->file path bs))))

;; String output
(define call-with-string-output-file
  (lambda (path func)
    (let ((cs (call-with-output-string func)))
      (send avmplus/File write path cs))))

;;;; Console

(define read-line ;; [plt]
  (lambda () (send avmplus/System readLine)))

(define display ;; [r6rs-lib]
  (lambda (str) (send avmplus/System write str)))

; (set! print display)
(define println trace)
