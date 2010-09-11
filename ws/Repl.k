;; A calculator interactive demo

(define *stage* ())
(define *input* ()) ;; TextField for Input
(define *output* ()) ;; TextField for Output

(define display
  (lambda (str)
    (send *output* appendText str)
    (slot-setq! *output* scrollV (slot-getq *output* maxScrollV))))

(define repl-run
  (lambda (str)
    (display str)
    (display "\n=> ")
    (assemble (debugfile "Repl.k") (debugline 66) (pushundefined))
    (evalstring str '*ns* (lambda (e)
                            (display
                             (if (not (eq? e #undefined))
                                 (call-with-output-string
                                  (lambda (out) (pretty-print-on e out)))
                                 "#undefined\n"))))))

(define listener
  (lambda (e)
    (if (eq? (slot-getq e keyCode) 13)
	(let (dummy)
	  (repl-run (slot-getq *input* text))
	  (slot-setq! *input* text ""))
	())))

(define-pattern
  ((slot-set-all! obj ()) ())
  ((slot-set-all! obj ((name value) . xs))
   (let () (slot-setq! obj name value) (slot-set-all! obj xs))))

;; Screen configuration

(class Repl (flash.display/Sprite) (format)
       (constructsuper)
       (set! *stage* #self)

       (set! format (new flash.text/TextFormat))
       (slot-setq! format size 24)

       (set! *output* (new flash.text/TextField))
       (slot-set-all! *output* ((x 10)
                                (y 10)
                                (width 280)
                                (height 330)
                                (border 1)
                                (multiline 1)
                                (defaultTextFormat format)))
       
       (set! *input* (new flash.text/TextField))
       (slot-set-all! *input* ((type (slot-getq flash.text/TextFieldType INPUT))
                               (x 10)
                               (y 350)
                               (width 280)
                               (height 40)
                               (border 1)
                               (defaultTextFormat format)
                               (text "(+ 3 4)")))

       (send #self addChild *output*)
       (send #self addChild *input*)
       (send (slot-getq #self parent) addEventListener "keyDown" listener))
