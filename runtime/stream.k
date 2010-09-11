;; Stream library for boot strapping
;;
;; Function names are chosen from r6rs because polymorphic functions
;; are not supported yet.

;;;; Generic function definition

(define-generic next (obj) (error "next is not defined in " obj))
(define-generic peek (obj) (error "peek is not defined in " obj))
(define-generic next-put (obj value) (error "next-put is not defined in " obj))
(define-generic next-put-all (obj value) (error "next-put-all is not defined in " obj))
(define-generic position (obj) (error "position is not defined in " obj))
(define-generic set-position (obj v) (error "set-position is not defined in " obj))
(define-generic at-end? (obj) (error "at-end? is not defined in " obj))
(define-generic ->stream (obj) (error "cannot convert " obj " to stream"))
(define-generic up-to-end (obj) (error "up-to-end is not defined in " obj))

;;;; End-of-file object

(define-structure *end* ())
(define *end* (new-*end*))

(define eof-object (lambda () *end*)) ;; [r6rs-lib]
(define eof-object? (lambda (obj) (=== obj *end*))) ;; [r6rs-lib]

;;;; String Input Stream
;;;; Input and output for string stream is different object because
;;;; Tamarin's string is immutable

(define-structure string-input (buffer position))

(define string-input-with-string
  (lambda (str)
    (let ((self (new-string-input)))
      (set-string-input-buffer self str)
      (set-string-input-position self 0)
      self)))

(define-method position typeof-string-input string-input-position) ;; [cola]
(define-method set-position typeof-string-input set-string-input-position) ;; [cola]

(define-method at-end? typeof-string-input ;; [cola]
  (lambda (self)
    (not (< (slot-getq self position) (slot-getq (string-input-buffer self) length)))))

(define-method peek typeof-string-input ;; [cola]
  (lambda (self)
    (if (at-end? self)
	*end*
	(send (string-input-buffer self) charCodeAt (position self)))))

(define-method next typeof-string-input ;; [cola]
  (lambda (self)
    (let ((c (peek self)))
      (if (!= c *end*)
          (set-position self (+ (position self) 1))
          '())
      c)))

(define-method up-to-end typeof-string-input ;; [cola]
  (lambda (self)
    (substring (string-input-buffer self)
               (position self)
               (string-length (string-input-buffer self)))))

(define-method ->stream String string-input-with-string)

(define open-string-input-port string-input-with-string) ;; [r6rs-lib]
(define port-position position) ;; [r6rs-lib]
(define set-port-position! set-position);; [r6rs-lib]
(define get-char next) ;; [r6rs-lib]
(define lookahead-char peek) ;; [r6rs-lib]
(define port-eof? at-end?) ;; [r6rs-lib]

;;;; String Output Stream
;;;; I steal the idea from OMeta/JS
;;;; note: string output doesn't support set-position

(define-structure string-output (buffer position))

(define open-output-string ;; [r6rs]
  (lambda ()
    (let ((s (new-string-output)))
      (set-string-output-buffer s (array))
      (set-string-output-position s 0)
      s)))

(define-method position typeof-string-output string-output-position) ;; [cola]

(define-method next-put typeof-string-output
  (lambda (self c)
    (send (string-output-buffer self)
          push
          (send String fromCharCode c)) ; todo: (string c)
    (set-string-output-position self (+ (string-output-position self) 1))))

(define-method next-put-all typeof-string-output
  (lambda (self str)
    (send (string-output-buffer self) push str)
    (set-string-output-position self (+ (string-output-position self)
                                        (string-length str)))))

(define get-output-string ;; [srfi-6]
  (lambda (self) (send (string-output-buffer self) join "")))

(define call-with-output-string ;; [plt]
  (lambda (proc)
    (let ((out (open-output-string)))
      (proc out)
      (get-output-string out))))

(define put-char next-put) ;; [r6rs-lib(-)]
(define put-string next-put-all) ;; [r6rs-lib(-)]

;;;; Byte Stream
;;;; Wrapper object to adapt stream protocol to flash.util.ByteArray

(define-structure byte-stream (bytes))

(define open-input-bytes ;; [plt]
  (lambda (bs)
    (let ((s (new-byte-stream)))
      (set-byte-stream-bytes s bs)
      (slot-setq! bs position 0)
      s)))

(define open-output-bytes ;; [plt]
  (lambda () (open-input-bytes (make-bytes 0))))

(define get-output-bytes byte-stream-bytes) ;; [plt]

(define call-with-output-bytes ;; [plt]
  (lambda (proc)
    (let ((out (open-output-bytes)))
      (proc out)
      (get-output-bytes out))))

(define-method next typeof-byte-stream
  (lambda (self)
    (if (at-end? self)
        *end*
        (send (byte-stream-bytes self) readUnsignedByte))))

(define next-all-into
  (lambda (self bs offset size)
    (if (=== size 0)
        () ;; do nothing. note: readBytes read all available bytes if size is zero.
        (send (byte-stream-bytes self) readBytes bs offset size))))

(define next-all
  (lambda (self size)
    (let ((bs (make-bytes 0)))
      (next-all-into self bs 0 size)
      bs)))

(define-method next-put typeof-byte-stream
  (lambda (self b) (send (byte-stream-bytes self) writeByte b)))

(define-method next-put-all typeof-byte-stream
  (lambda (self bs) (send (byte-stream-bytes self) writeBytes bs)))

(define-method at-end? typeof-byte-stream
  (lambda (self) (=== 0 (slot-getq (byte-stream-bytes self) bytesAvailable))))

(define-method position typeof-byte-stream
  (lambda (self) (slot-getq (byte-stream-bytes self) position)))

(define-method set-position typeof-byte-stream
  (lambda (self i) (slot-setq! (byte-stream-bytes self) position i)))

(define-method ->stream flash.utils/ByteArray open-input-bytes)

(define read-byte next) ;; [plt]
(define read-bytes (lambda (size out) (next-all out size))) ;; [plt]
(define write-byte (lambda (x out) (next-put out x))) ;; [plt]
(define write-bytes (lambda (xs out) (next-put-all out xs))) ;; [plt]
(define file-position position)
