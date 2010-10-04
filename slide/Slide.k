;;;; The Tamacola Slide

(set! *IS-RUNTIME-MACRO-COMPILE-MODE* #t)

;;;; Global variables

(define *location* "Home.xml")
(define *stage* ())              ;; *stage* is assigned to the Stage object
(define *workspace* ())          ;; TextField for Output
(define *slides* ())             ;; The slides
(define *ok-sound-file* "Cat.mp3") ;; OK Sound file
(define *ok-sound* ())              ;; OK Sound object
(define *ng-sound-file* "Goose.mp3") ;; NG Sound file
(define *ng-sound* ())              ;; NG Sound object

(define stage (lambda () *stage*))
(assemble (debugfile "Slide.k") (debugline 14) (pushundefined))

(define display
  (lambda (obj)
    (let* ((str (String obj))
           (end (slot-getq *workspace* selectionEndIndex))
           (length (string-length str)))
      (send *workspace* replaceText end end str)
      (send *workspace* setSelection (+ end length) (+ end length)))))

(define-pattern
  ((slot-set-all! obj ()) ())
  ((slot-set-all! obj ((name value) . xs))
   (begin (slot-setq! obj name value) (slot-set-all! obj xs))))

;;;; Commands

(define play-result
  (lambda (obj)
    (if (or (istype obj Error) (eq? obj 'FAIL))
        (send *ng-sound* play)
        (send *ok-sound* play))))

(define doit
  (lambda (str)
    (evalstring-default str '*dummy* play-result)))

(define peg
  (lambda (str)
    (set! *translator* (make-translator peg/definitions))
    (evalstring str '*dummy* play-result)))

(define printit
  (lambda (str)
    (evalstring-default str
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
                        (slot-setq! (slot-getq *workspace* stage) focus *workspace*)))
                  (play-result e)))))
                  

;;;; Commands

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

(define get-selection
  (lambda ()
    (let* ((text (slot-getq *workspace* text))
           (range (select-source-range
                   text
                   (slot-getq *workspace* selectionBeginIndex)
                   (slot-getq *workspace* selectionEndIndex))))
      (send *workspace* setSelection (car range) (cdr range))
      (substring text (car range) (cdr range)))))

(define doit-selection
  (lambda () (doit (get-selection))))

(define peg-selection
  (lambda () (peg (get-selection))))

(define printit-selection
  (lambda () (printit (get-selection))))

(define slide-reload
  (lambda ()
    (load-slides *location*
                 (lambda (slides)
                   (slides-update (slides-goto slides (slides-position *slides*)))))))

(define slide-back
  (lambda ()
    (slides-update (slides-goto *slides* (- (slides-position *slides*) 1)))))

(define slide-next
  (lambda ()
    (slides-update (slides-goto *slides* (+ (slides-position *slides*) 1)))))

;;;; Event handler

(define handleButtons
  (lambda (e)
    (let ((type (slot-getq e text)))
      (cond
       ((equal? type "back") (slide-back))
       ((equal? type "next") (slide-next))
       ((equal? type "doit") (doit-selection))
       ((equal? type "reload") (slide-reload))
       ((equal? type "printit") (printit-selection))
       ((equal? type "peg") (peg-selection))
       ('else (display 'else))))))

(define handleKey
  (lambda (e)
    (let ((keyCode (slot-getq e keyCode))
          (ctrlKey (slot-getq e ctrlKey))
          (w (eq? (slot-getq e target) *workspace*)))
    (cond 
     ((and (eq? keyCode #"R") ctrlKey) (slide-reload))
     ((and (eq? keyCode #"D") ctrlKey w) (doit-selection))
     ((and (eq? keyCode #"P") ctrlKey w) (printit-selection))
     ((and (eq? keyCode 37) (not w)) (slide-back))
     ((and (eq? keyCode 39) (not w)) (slide-next))
     ))))

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
    (let* ((page (slot-get (slides-pages slides)
                           (slides-position slides)))
           (contents (send page children)))
      contents)))

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
    (load-slides *location*
                 (lambda (slides) (slides-update slides)))))

;;;; Screen configuration

(class Slide (flash.display/Sprite) (format)
       (constructsuper)
       (set! *stage* #self)

       (set! *ok-sound* (new flash.media/Sound))
       (send *ok-sound* load (new flash.net/URLRequest *ok-sound-file*))
       (set! *ng-sound* (new flash.media/Sound))
       (send *ng-sound* load (new flash.net/URLRequest *ng-sound-file*))

       (set! format (new flash.text/TextFormat))
       (slot-setq! format size 18)
       (slot-setq! format font "Courier New")

       (set! *workspace* (new flash.text/TextField))
       (slot-set-all! *workspace* ((x 5)
                                   (y 5)
                                   (width 790)
                                   (height 565)
;                                   (border #t)
                                   (multiline #t)
                                   (wordWrap #t)
                                   (alwaysShowSelection #t)
                                   (type (slot-getq flash.text/TextFieldType INPUT))))

       (let ((buttons (new flash.text/TextField))
             (labels (string-join '("<a href='event:back'>[&lt;-]</a> "
                                    "<a href='event:next'>[-&gt;]</a> "
                                    "<a href='event:reload'>[Reload]</a> "
                                    "<a href='event:doit'>[Do It]</a> "
                                    "<a href='event:printit'>[Print It]</a> "
                                    "<a href='event:peg'>[PEG]</a> "
                                    ) "")))
         
         (slot-set-all! buttons ((x 5)
                                 (y 570)
                                 (width 790)
                                 (height 25)
                                 (selectable #f)
                                 (defaultTextFormat format)
                                 (htmlText labels)))
         (send buttons addEventListener "link" handleButtons)

         (send #self addChild buttons))
       
       (send #self addChild *workspace*)
       (send (slot-getq #self stage) addEventListener "keyDown" handleKey)
       (slot-setq! XML prettyPrinting #f)
       (init-source)
)
