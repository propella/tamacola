;;;; The Tamacola Slide

(set! *IS-RUNTIME-MACRO-COMPILE-MODE* #t)

;;;; Global variables

(define *stage* ())             ;; *stage* is assigned to the Stage object
(define *workspace* ())         ;; TextField for Output
(define *slides* ())            ;; The slides

(define stage (lambda () *stage*))
(assemble (debugfile "Slide.k") (debugline 14) (pushundefined))

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

;;;; Commands

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

;;;; Event handler

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
        (slides-update (slides-goto *slides* (- (slides-position *slides*) 1))))
       ((equal? type "next")
        (slides-update (slides-goto *slides* (+ (slides-position *slides*) 1))))
       ((equal? type "doit") (doit selection))
       ((equal? type "reload") 
        (load-slides "Home.xml"
                     (lambda (slides)
                       (slides-update (slides-goto slides (slides-position *slides*))))))
       ((equal? type "printit") (printit selection))
       ('else (display 'else))))))

;;;; Slides object

(define-structure slides (source position))

;; Make slides object

(define slides-make
  (lambda (source position)
    (let ((slides (new-slides)))
      (set-slides-source slides source)
      (set-slides-position slides position)
      slides)))

(define slides-pages
  (lambda (slides) (slot-getq (slides-source slides) page)))

;; Return current content

(define slides-content
  (lambda (slides)
    (let ((page (slot-get (slides-pages slides)
                          (slides-position slides))))
      (send page children))))

;; Return a new slides to point a new location

(define slides-goto
  (lambda (slides position)
    (let* ((length (send (slides-pages slides) length))
           (next (+ (modulo position length)
                    (if (< position 0) length 0))))
      (slides-make (slides-source slides) next))))

;;;; Load contents

(define load-xml
  (lambda (url callback)
    (let ((loader (new flash.net/URLLoader)))
      (send loader addEventListener
            "complete"
            (lambda (e)
              (callback (new XML (slot-getq loader data)))))
      (send loader load (new flash.net/URLRequest url)))))

;; Load slide data from a xml file and return a slides object

(define slides-update
  (lambda (slides)
    ;; (let ((format (new flash.text/TextFormat)))
    ;;   (slot-setq! format size 18)
    ;;   (slot-setq! format font "Times")
    ;;   (slot-setq! *workspace* defaultTextFormat format))
    (send *workspace* setSelection 0 0)
    (set! *slides* slides)
    (slot-setq! *workspace* htmlText (slides-content *slides*))))

(define load-slides
  (lambda (url callback)
    (load-xml url
              (lambda (xml) (callback (slides-make xml 0))))))

(define init-source
  (lambda ()
    (load-slides "Home.xml"
                 (lambda (slides) (slides-update slides)))))

;;;; Screen configuration

(class Slide (flash.display/Sprite) (format)
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
                                   (type (slot-getq flash.text/TextFieldType INPUT))))

       (let ((buttons (new flash.text/TextField))
             (labels (string-join '("<a href='event:back'>[&lt;-]</a> "
                                    "<a href='event:next'>[-&gt;]</a> "
                                    "<a href='event:reload'>[Reload]</a> "
                                    "<a href='event:doit'>[Do It]</a> "
                                    "<a href='event:printit'>[Print It]</a> ")
                                  "")))
         
         (slot-set-all! buttons ((x 5)
                                 (y 450)
                                 (width 630)
                                 (height 25)
                                 (selectable #f)
                                 (defaultTextFormat format)
                                 (htmlText labels)))
         (send buttons addEventListener "link" handleButtons)

         (send #self addChild buttons))
       
       (send #self addChild *workspace*)
       (init-source)
)
