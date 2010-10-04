;; The Tamacola Workspace

(set! *IS-RUNTIME-MACRO-COMPILE-MODE* #t)

;; Global variables

(define *stage* ())             ;; *stage* is assigned to the Stage object
(define *workspace* ())         ;; TextField for Output
(define *navigation* ())        ;; TextField for Navigation
;(define *home* "Home.html")     ;; the name of the home
(define *home* "S3-Yoshiki.html")     ;; the name of the home
(define *location* *home*)      ;; Current visiting URL
(define *history* ())           ;; History list
(define *language* ())          ;; TextField for Language Setting

(define stage (lambda () *stage*))
(assemble (debugfile "Workspace.k") (debugline 14) (pushundefined))

(define display
  (lambda (str)
    (let ((end (slot-getq *workspace* selectionEndIndex))
          (length (string-length str)))
      (send *workspace* replaceText end end str)
      (send *workspace* setSelection (+ end length) (+ end length)))))

(define-pattern
  ((slot-set-all! obj ()) ())
  ((slot-set-all! obj ((name value) . xs))
   (begin (slot-setq! obj name value) (slot-set-all! obj xs))))

;; Commands

(define doit
  (lambda (str) (evalstring str '*dummy* (lambda ()))))

(define printit
  (lambda (str)
    (evalstring str
                '*dummy*
                (lambda (e)
                  (if (eq? e #undefined)
                      ()
                      (let* ((result (call-with-output-string
                                      (lambda (out) (pretty-print-on e out))))
                             (out (+ "\n=> " result))
                             (end (slot-getq *workspace* selectionEndIndex)))
                        (send *workspace* replaceText end end out)
                        (send *workspace* setSelection end (+ end (slot-getq out length)))
                        (slot-setq! (slot-getq *workspace* stage) focus *workspace*)))))))
  

(define jump
  (lambda (url)
    (set! *history* (cons url *history*))
    (jump* url)))

(define jump*
  (lambda (url)
    (let ((loader (new flash.net/URLLoader)))
      (slot-setq! *navigation* text url)
      (set! *location* url)
      (send *workspace* setSelection 0 0)
      (send loader addEventListener
            "complete"
            (lambda (e)
              (if (send url match (new RegExp ".html$" ""))
                  (slot-setq! *workspace* htmlText (String (slot-getq loader data)))
                  (let ((format (new flash.text/TextFormat)))
                    (slot-set-all! format ((size 12)
                                           (font "Courier New")
                                           (align "left")
                                           (bold #f)))
                    (slot-setq! *workspace* text (String (slot-getq loader data)))
                    (send *workspace* setTextFormat format)))))
      (send loader load (new flash.net/URLRequest url)))))

;; The event handler

(define ws-select-language
  (lambda (e)
    (let* ((lang (slot-getq *language* text))
           (as   (assoc (intern lang) *translators*)))
      (set! *translator* '())
      (cond (as
             (set! *translator* (cdr as))
	     (slot-setq! *language* borderColor 0x000000))
            ((= (string-length lang) 0)
	     (slot-setq! *language* borderColor 0x000000))
	    (#t
	     (slot-setq! *language* borderColor 0xFF0000))))))

(define %select-source-range
  (lambda (str delimiter pos)
    (let ((start (send str lastIndexOf delimiter (- pos 1)))
          (end (send str indexOf delimiter pos)))
      (let ((%start (if (eq? -1 start) 0 (+ start 1)))
            (%end (if (eq? -1 end) (string-length str) end)))
        (cons %start %end)))))

(define select-source-range
  (lambda (str start end)
    (if (not (eq? start end))
        (cons start end)
        (let ((r-range (%select-source-range str "\r" start))
              (n-range (%select-source-range str "\n" start)))
          (let ((%start (max (car r-range) (car n-range)))
                (%end (min (cdr r-range) (cdr n-range))))
              (cons %start %end))))))

(define handleButtons
  (lambda (e)
    (let* ((text (slot-getq *workspace* text))
           (range (select-source-range
                   text
                   (slot-getq *workspace* selectionBeginIndex)
                   (slot-getq *workspace* selectionEndIndex)))
           (selection (substring text (car range) (cdr range)))
           (type (slot-getq e text)))
      (send *workspace* setSelection (car range) (cdr range))
      (cond
       ((equal? type "back")
        (if (pair? *history*)
            (begin
              (set! *history* (cdr *history*))
              (if (pair? *history*)
                  (jump* (car *history*))
                  'YOU-ARE-AT-HOME-ALREADY))
            (jump *home*)))
       ((equal? type "home") (jump *home*))
       ((equal? type "reload") (jump* *location*))
       ((equal? type "doit") (doit selection))
       ((equal? type "printit") (printit selection))
       ((equal? type "language") (ws-select-language))
       ('else (display 'else))))))

;; Screen configuration

(class Workspace (flash.display/Sprite) (format)
       (constructsuper)
       (set! *stage* #self)

       (set! format (new flash.text/TextFormat))
       (slot-setq! format size 18)
       (slot-setq! format font "Courier New")

       (set! *workspace* (new flash.text/TextField))
       (slot-set-all! *workspace* ((x 5)
                                   (y 5)
                                   (width 630)
                                   (height 410)
                                   (border #t)
                                   (multiline #t)
                                   (wordWrap #t)
                                   (alwaysShowSelection #t)
                                   (type (slot-getq flash.text/TextFieldType INPUT))
                                   (defaultTextFormat format)))
       
       (set! *navigation* (new flash.text/TextField))
       (slot-set-all! *navigation* ((x 5)
                                    (y 420)
                                    (width 520)
                                    (height 25)
                                    (border #t)
                                    (alwaysShowSelection #t)
                                    (type (slot-getq flash.text/TextFieldType INPUT))
                                    (defaultTextFormat format)))

       (set! *language* (new flash.text/TextField))
       (slot-set-all! *language*   ((x 530)
                                    (y 420)
                                    (width 100)
                                    (height 25)
                                    (border #t)
                                    (alwaysShowSelection #t)
                                    (type (slot-getq flash.text/TextFieldType INPUT))
                                    (defaultTextFormat format)))

       (let ((buttons (new flash.text/TextField)))
         (slot-set-all! buttons ((x 5)
                                 (y 450)
                                 (width 630)
                                 (height 25)
                                 (selectable #f)
                                 (defaultTextFormat format)
                                 (htmlText
                                  "<a href='event:back'>[Back]</a> <a href='event:home'>[Home]</a> <a href='event:reload'>[Reload] <a href='event:doit'>[Do It]</a> <a href='event:printit'>[Print It]</a> ")))
         (send buttons addEventListener "link" handleButtons)
	 (send *language* addEventListener "change" ws-select-language)

         (send #self addChild buttons))
       
       (send #self addChild *workspace*)
       (send #self addChild *navigation*)
       (send #self addChild *language*)
       (jump *location*))
