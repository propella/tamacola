;; ;;;; Platform dependent functions for avmshell

;; Flash-cola Interface class

(define exit ;; [r6rs-lib(-)]
   (lambda (n) (throw `(exit ,n))))

(define *arguments* ())
(define *translator* '())
(define *translators* '())

(define exec
  (lambda (cmd) (throw "exec doesn't work on flash")))

(define load-swfs
  (lambda (swfs callback)
    (let ((loader (new flash.display/Loader))
          (ctx (new flash.system/LoaderContext
                    #f
                    (slot-getq flash.system/ApplicationDomain currentDomain)
                    ())))
      (if callback
          (send (slot-getq loader contentLoaderInfo)
                 addEventListener
                 "complete"
                 callback)
           ())
      (send loader loadBytes swfs ctx))))

(define load-byte-codes
  (lambda (bytes callback)
    (let ((spec `((frame-size . (1 1))
                  ,(cons 'frame-rate 60)
                  ,(cons 'frame-count  1)
                  (tags 
                   (file-attribute)
                   ,(cons 'enable-debugger2 "")
                   (do-abc "*eval*" ,bytes)
                   (show-frame)
                   (end-tag)))))
      (let ((swf (call-with-output-bytes
                  (lambda (out) (write-swf spec out)))))
        (load-swfs swf callback)))))

;;;; Managing Languages

(define make-translator
   (lambda (parser)
     (lambda (str)
(trace "str: " str)
       (let ((expressions (parse-collection parser str)))
(trace "expressions: " expressions)
         (if (eq? expressions 'FAIL)
	     (throw (+ "Parse error: " str))
	     expressions)))))

(define make-translator2
   (lambda (parser translator)
     (lambda (str)
       (let ((tree (parse-collection parser str)))
(trace "tree: " tree)
       (let ((expressions (parse-stream translator (->stream (cons tree '())))))
(trace "parsed: " expressions)
         (if (eq? expressions 'FAIL)
	     (throw (+ "Parse error: " str))
	     expressions))))))

(define add-translator
  (lambda (name function)
    (let ((as (assoc name *translators*)))
      (cond (as
             (set! *translators* (delete as *translators*))))
      (set! *translators* (cons (cons name function) *translators*))
      (trace *translators*))))

;;;; File Input / Output

;; Binary input ;; [r6rs-lib(-)]
(define call-with-input-file
  (lambda (path func) (throw "call-with-input-file is not implemented on flash.")))

;; String input
(define call-with-string-input-file
  (lambda (path func) (throw "call-with-string-input-file is not implemented on flash.")))

;; Binary output ;; [r6rs-lib]
(define call-with-output-file
  (lambda (path func) (throw "call-with-output-file is not implemented on flash.")))

;; String output
(define call-with-string-output-file
  (lambda (path func) (throw "call-with-string-output-file is not implemented on flash.")))

;; ;;;; Console

(define read-line ;; [plt]
  (lambda () (throw "read-line is not implemented on flash.")))
    
(define newline (lambda () (display "\n")))

(define print display)
(define println (lambda (str) (display str) (newline)))

(define evalstring-default
  (lambda (s ns callback)
    (let ((expressions (read* (->stream s))))
;(trace "input: " (->list expressions))
      (if (eq? expressions 'FAIL)
          (throw (+ "Parse error: " s))
          (eval-all* (->list expressions) ns callback)))))

(define evalstring
  (lambda (s ns callback)
    (if *translator*
        (eval-all* (*translator* s) ns callback)
        (evalstring-default s ns callback))))
