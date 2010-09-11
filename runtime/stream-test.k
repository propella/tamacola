;;;; String Input Port

(let ((s (open-string-input-port "ABC")))
  (check (lookahead-char s) => 65)
  (check (lookahead-char s) => 65)
  (check (get-char s) => 65)
  (check (get-char s) => 66)
  (check (get-char s) => 67)
  (check (get-char s) => (eof-object))
  (set-port-position! s 1)
  (check (get-char s) => 66)
)

;; Generic method API

(let ((s (open-string-input-port "ABC")))
  (check (peek s) => 65)
  (check (peek s) => 65)
  (check (next s) => 65)
  (check (next s) => 66)
  (check (next s) => 67)
  (check (next s) => (eof-object))
  (set-position s 1)
  (check (next s) => 66)
)

(let ((s (open-output-string)))
  (next-put s #"A")
  (next-put s #"B")
  (next-put s #"C")
  (check (position s) => 3)
  (next-put-all s "DEF")
  (check (position s) => 6)
  (check (get-output-string s) => "ABCDEF"))

;;;; Byte stream

(let ((s (open-output-bytes)))
  (next-put s 1)
  (next-put s 2)
  (next-put s 3)
  (set-position s 0)
  (check (next s) => 1)
  (check (next s) => 2)
  (check (next s) => 3)
  (check (next s) => *end*))
