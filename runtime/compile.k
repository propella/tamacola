(define abc-compile
  (lambda (exprs)
    (parse-expr2 (compiler) syntax/start exprs)))

(define global ()) ;; makeshift

;; Evaluate expressions. Callback may be native loader complete event.
(define eval-all
  (lambda (exprs namespace callback)
    (let* ((asm (abc-compile exprs))
           (bytes (call-with-output-bytes
                   (lambda (port) (write-asm asm port)))))
      (load-byte-codes bytes callback))))

;; namespace is ignored now...
(define eval
  (lambda (expr namespace) (eval-all (list expr) namespace ())))

(define *eval-result* ())

;; Evaluate expressions. Callback is called with the result if available.
;; This is not also reentrant!!
(define eval-all*
  (lambda (exprs namespace callback)
    (let ((func (lambda (e) (callback *eval-result*)))
          (expr `(set! *eval-result*
                      (let () ,@exprs))))
      (set! *eval-result* #undefined)
      (eval-all (list expr) namespace func))))

;; For runtime quasiquote

(define qq-object
  (lambda (x)
    (if (pair? x)
        (let ((head (car x))
              (tail (cdr x)))
          (if (eq? head 'unquote)
              (car tail)
              (if (and (pair? head)
                       (eq? (car head) 'unquote-splicing))
                  (list 'concat (list '->list (cadr head)) (qq-object tail))
                  (list 'cons (qq-object head) (qq-object tail)))))
        (list 'quote x))))
