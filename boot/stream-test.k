;; Test case for stream

(require "boot/stream.k")
(require "boot/check.k")

;;;; Input byte stream

(let ((s (open-input-byte (bytes 1 2 254 255))))
  (check (peek s) => 1)
  (check (peek s) => 1)
  (check (next s) => 1)
  (check (next s) => 2)
  (check (next s) => 254)
  (check (next s) => 255)
  (check (next s) => *end*)
  (check (next s) => *end*)
  (check (at-end? s) => 't)
)

(let ((s (open-input-byte (bytes 1 2 3 4))))
  (check (next s) => 1)
  (check (next s) => 2)
  (check (up-to-end s) => (bytes 3 4))
  (check (at-end? s) => 't)
)

;;;; Output byte stream

(let ((p (open-output-bytes)))
  (check (bytes->list (get-output-bytes p)) => '())
  (put p 65)
  (check (bytes->list (get-output-bytes p)) => '(65))
  (put p 66)
  (put p 67)
  (put p 68)
  (put p 69)
  (put p 70)
  (check (bytes->string (get-output-bytes p)) => "ABCDEF")
  (put-all p (bytes 71 72 73 74 75))
  (check (bytes->string (get-output-bytes p)) => "ABCDEFGHIJK")
)

;;;; File stream

(let ((s (file-stream-open "test.txt" "w")))
  (put s 65)
  (put s 66)
  (put-all s (bytes 67 68 69))
  (file-stream-close s)
)

(let ((s (file-stream-open "test.txt" "r")))
  (check (peek s) => 65)
  (check (peek s) => 65)
  (check (next s) => 65)
  (check (next s) => 66)
  (check (next s) => 67)
  (check (next s) => 68)
  (check (next s) => 69)
  (check (next s) => *end*)
  (check (at-end? s) => 't)
)

;;;; PLT Byte port

(let ((p (open-output-string "")))
  (write-byte #"A" p)
  (check (get-output-string p) => "A"))

;; PLT String Port

;(check (port? (open-output-string "")) => 't)

(check (call-with-output-string
	(lambda (out)
	  (write-string "hello" out))) => "hello")

(let ((p (open-output-string "")))
  (write-string "hello " p)
  (write-string "world" p)
  (check (get-output-string p) => "hello world")
  (check (file-position p) => 11)
  (file-position p 5)
  (check (get-output-string p) => "hello")
  (write-string " cola" p)
  (check (get-output-string p) => "hello cola")
  )

(check (call-with-output-string
	(lambda (out) (write-string "hi" out))) => "hi")

(check (string-append "hello " "world") => "hello world")
