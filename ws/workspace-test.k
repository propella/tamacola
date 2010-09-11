;; Return range of selected text.
;; If a lenght of selection is zero, a line is selected.

(define %select-soruce-range
  (lambda (str delimiter pos)
    (let ((start (send str lastIndexOf delimiter (- pos 1)))
          (end (send str indexOf delimiter pos)))
      (let ((%start (if (eq? -1 start) 0 (+ start 1)))
            (%end (if (eq? -1 end) (string-length str) end)))
        (cons %start %end)))))

(define select-soruce-range
  (lambda (str start end)
    (if (not (eq? start end))
        (cons start end)
        (let ((r-range (%select-soruce-range str "\r" start))
              (n-range (%select-soruce-range str "\n" start)))
          (let ((%start (max (car r-range) (car n-range)))
                (%end (min (cdr r-range) (cdr n-range))))
              (cons %start %end))))))

(define select-soruce-range*
  (lambda (str start end)
    (let ((range (select-soruce-range str start end)))
      (substring str (car range) (cdr range)))))

;(trace "case A")
(check (%select-soruce-range "0123,5678,0123" "," 0) => '(0 . 4))
(check (select-soruce-range* "0123\r5678\r0123" 0 0) => "0123")
(check (select-soruce-range* "0123\r5678\r0123" 0 4) => "0123")

;(trace "case B")
(check (%select-soruce-range "0123,5678,0123" "," 3) => '(0 . 4))
(check (select-soruce-range* "0123\r5678\r0123" 3 3) => "0123")
(check (%select-soruce-range "0123,5678,0123" "," 4) => '(0 . 4))
(check (select-soruce-range* "0123\r5678\r0123" 4 4) => "0123")
(check (%select-soruce-range "0123,5678,0123" "," 5) => '(5 . 9))
(check (select-soruce-range* "0123\r5678\r0123" 5 5) => "5678")
(check (select-soruce-range* "0123\r5678\r0123" 5 9) => "5678")

;(trace "case E")
(check (%select-soruce-range "0123,5678,0123" "," 8) => '(5 . 9))
(check (select-soruce-range* "0123\r5678\r0123" 8 8) => "5678")
(check (%select-soruce-range "0123,5678,0123" "," 9) => '(5 . 9))
(check (select-soruce-range* "0123\r5678\r0123" 9 9) => "5678")
(check (%select-soruce-range "0123,5678,0123" "," 10) => '(10 . 14))
(check (select-soruce-range* "0123\r5678\r0123" 10 10) => "0123")

;(trace "case F")
(check (select-soruce-range* "0123\n5678\n0123" 3 3) => "0123")
(check (select-soruce-range* "0123\n5678\n0123" 4 4) => "0123")
(check (select-soruce-range* "0123\n5678\n0123" 5 5) => "5678")

;(trace "case G")
(check (select-soruce-range* "0123\r\n5678\r\n0123" 3 3) => "0123")
(check (select-soruce-range* "0123\r\n5678\r\n0123" 4 4) => "0123")
(check (select-soruce-range* "0123\r\n5678\r\n0123" 5 5) => "")
(check (select-soruce-range* "0123\r\n5678\r\n0123" 6 6) => "5678")
