;; Copyright (c) 2010 Takashi Yamamiya
;;
;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:
;;
;; The above copyright notice and this permission notice shall be included in
;; all copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
;; THE SOFTWARE.

(define write-swf-u8 write-byte)

(define write-swf-u16
  (lambda (n out)
    (write-byte (bitwise-and n #xff) out)
    (write-byte (bitwise-and (arithmetic-shift n -8) #xff) out)))

(define write-swf-u32
  (lambda (n out)
    (write-byte (bitwise-and n #xff) out)
    (write-byte (bitwise-and (arithmetic-shift n -8) #xff) out)
    (write-byte (bitwise-and (arithmetic-shift n -16) #xff) out)
    (write-byte (bitwise-and (arithmetic-shift n -24) #xff) out)))

(define nbits-unsigned
  (lambda (lst)
    (let ((max (fold max 0 lst)))
      (+ (int (/ (log max) (log 2))) 1))))

(define nbits-signed
  (lambda (lst) (+ (nbits-unsigned lst) 1)))

(define push-bits
  (lambda (num nbits rest)
    (if (eq? nbits 0)
        rest
        (push-bits (arithmetic-shift num -1)
                     (- nbits 1)
                     (cons (bitwise-and num 1) rest)))))

;; Make a integer from the first nbits of list
(define bits->u8
  (lambda (lst)
    (let ((byte (take lst 8)))
      (fold (lambda (x n) (+ (* n 2) x)) 0 byte))))

;; Write a list of bit as binary data postfix 8 bits padding.
(define *write-bits
  (lambda (lst out)
    (if (pair? lst)
        (begin 
          (write-swf-u8 (bits->u8 lst) out)
          (*write-bits (drop lst 8) out))
        '())))

(define write-bits
  (lambda (bits out)
    (let* ((padding (- 8 (modulo (length bits) 8)))
           (filled (append bits (take '(0 0 0 0 0 0 0 0) padding))))
      (*write-bits filled out))))

(define write-fixed8dot8
  (lambda (n out)
    (let* ((integer-part (int n))
           (small-part (int (* (/ (- n integer-part) 1) 256))))
      (write-swf-u8 small-part out)
      (write-swf-u8 integer-part out))))

(define write-rectangle
  (lambda (xmin xmax ymin ymax out)
    (let ((nbits (nbits-signed (list xmin xmax ymin ymax))))
      (write-bits
        (push-bits nbits 5
          (push-bits xmin nbits
            (push-bits xmax nbits
              (push-bits ymin nbits
                (push-bits ymax nbits ())))))
        out))))

(define record-header-body
  (lambda (type body out)
    (let ((size (bytes-length body)))
      (if (< size 63)
          (let ((code-length (bitwise-ior (arithmetic-shift type 6) size)))
            (write-swf-u16 code-length out)
            (write-bytes body out))
          (let ((code-length (bitwise-ior (arithmetic-shift type 6) 63)))
            (write-swf-u16 code-length out)
            (write-swf-u32 size out)
            (write-bytes body out))))))

(define write-rgb
  (lambda (rgb out)
    (write-swf-u8 (car rgb) out)
    (write-swf-u8 (cadr rgb) out)
    (write-swf-u8 (caddr rgb) out)))

(define write-string-type
  (lambda (str out)
    (write-bytes (string->bytes str) out)
    (write-swf-u8 0 out)))

(define write-symbol-class
  (lambda (name1 out)
    (let ((num-symbols 1)
          (tag1 0))
      (record-header-body 76
                          (call-with-output-bytes
                           (lambda (p)
                             (write-swf-u16 num-symbols p)
                             (write-swf-u16 tag1 p)
                             (write-string-type name1 p)))
                          out))))

(define write-do-abc
  (lambda (lst out)
    (let ((name (car lst))
          (bs (cadr lst))
          (flags 0))
      (record-header-body 82
                          (call-with-output-bytes
                           (lambda (p)
                             (write-swf-u32 flags p)
                             (write-string-type name p)
                             (write-bytes bs p)))
                           out))))

(define write-enable-debugger2
  (lambda (str out)
    (let ((bytes (call-with-output-bytes
                  (lambda (p)
                    (write-swf-u16 0 p)
                    (write-string-type str p)))))
      (record-header-body 64 bytes out))))

(define write-tag
  (lambda (tag out)
    (let ((name (car tag)))
      (cond
       ((eq? name 'end-tag) (record-header-body 0 (bytes) out))
       ((eq? name 'show-frame) (record-header-body 1 (bytes) out))
       ((eq? name 'set-background-color)
        (record-header-body 9
                            (call-with-output-bytes
                             (lambda (p) (write-rgb (cdr tag) p)))
                            out))
       ;; FileAttributes: HasMetaData 0, UseNetwork 0, HasAS3 1
       ;; ((eq? name 'file-attribute) (record-header-body 69 (bytes #b00001000 0 0 0) out))
       ((eq? name 'file-attribute) (record-header-body 69 (bytes #x08 0 0 0) out))
       ((eq? name 'enable-debugger2) (write-enable-debugger2 (cdr tag) out))
       ((eq? name 'symbol-class) (write-symbol-class (cdr tag) out))
       ((eq? name 'do-abc) (write-do-abc (cdr tag) out))
       ('else (error "unknown tag:" name))))))

(define write-tags
  (lambda (tags out) (map (lambda (t) (write-tag t out)) tags)))

(define write-body
  (lambda (spec out)
    (let* ((frame-size (cdr (assq 'frame-size spec)))
           (frame-rate (cdr (assq 'frame-rate spec)))
           (frame-count (cdr (assq 'frame-count spec)))
           (tags (cdr (assq 'tags spec)))
           (tags-data (call-with-output-bytes
                       (lambda (p) (write-tags tags p)))))
      (write-rectangle 0 (car frame-size) 0 (cadr frame-size) out)
      (write-fixed8dot8 frame-rate out)
      (write-swf-u16 frame-count out)
      (write-bytes tags-data out))))

(define write-swf
  (lambda (spec out)
    (let* ((version 10)
           (body (call-with-output-bytes
                  (lambda (p) (write-body spec p))))
           (body-length (+ (bytes-length body) 8)))
      (write-bytes (string->bytes "FWS") out)
      (write-swf-u8 version out)
      (write-swf-u32 body-length out)
      (write-bytes body out)
      )))

;; (abc-data (file->bytes infile))

(define make-spec
  (lambda (width height classname abcfiles)
    (let* ((symbol-class
            (if classname `(,(cons 'symbol-class classname)) ()))
           (do-abc
            (map (lambda (abcfile)
                   (cons 'do-abc (list abcfile (file->bytes abcfile))))
                 abcfiles))
           (tags (concatenate
                  (list
                   '(
		     (file-attribute)
		     (enable-debugger2 . "")
		     )
                   symbol-class
                   do-abc
                   '((show-frame)
                     (end-tag))))))
      `((frame-size . (,(* width 20) ,(* height 20)))
        ,(cons 'frame-rate 30)
        ,(cons 'frame-count  1)
        (tags ,@tags)))))

(define usage
  (lambda ()
    (display "Usage: swfmake [-w width] [-h height] [-o outfile] [-c classname] abcfiles ...\n")
    (display "  if -o is ommited, last abcfile name is used for output.\n")
    (exit 1)))

(define process-args
  (lambda (args infiles outfile width height classname cont)
    (if (pair? args)
        (let ((key (car args)))
          (cond 
           ((equal? key "-o")
            (process-args (cddr args) infiles (cadr args) width height classname cont))
           ((equal? key "-w")
            (process-args (cddr args) infiles outfile (cadr args) height classname cont))
           ((equal? key "-h")
            (process-args (cddr args) infiles outfile width (cadr args) classname cont))
           ((equal? key "-c")
            (process-args (cddr args) infiles outfile width height (cadr args) cont))
           ('else
            (process-args (cdr args) (cons (car args) infiles) outfile width height classname cont))))
        (if (pair? infiles)
            (cont (reverse infiles)
                  (or outfile (string-append (trim-extension (car (reverse infiles))) ".swf"))
                  (or width "100")
                  (or height "100") classname)
            (usage)))))
  
(define swf-run
  (lambda (args)
    (process-args args () #f #f #f #f
      (lambda (infiles outfile width height classname)
        (let ((spec (make-spec (string->number width)
                               (string->number height)
                               classname
                               infiles)))
          (call-with-output-file outfile
            (lambda (out) (write-swf spec out))))))))
