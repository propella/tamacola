;; test cases for swf.k

;; proc is (proc obj port)
(define to-bytes
  (lambda (proc obj) (call-with-output-bytes (lambda (port) (proc obj port)))))

(check (modulo 987 10) => 7)
(check (modulo 10 3) => 1)
(check (string->number "123") => 123)
(check (string->bytes "ABC") => (bytes #x41 #x42 #x43))

(check (trim-extension "file.txt") => "file")
(check (trim-extension "file") => "")
(check (trim-extension ".file") => "")
(check (trim-extension "../file.txt") => "../file")

(check (push-bits 1 1 ()) => '(1))
(check (push-bits 7 3 '(0 0 0)) => '(1 1 1 0 0 0))

(check (to-bytes write-fixed8dot8 1.0) => (bytes #x00 #x01))
(check (to-bytes write-fixed8dot8 1.5) => (bytes #x80 #x01))
(check (bits->u8 '(0 0 0 0  0 0 0 1)) => #x01)
(check (bits->u8 '(1 0 0 0  0 0 0 0)) => #x80)
(check (to-bytes write-swf-u16 #xff) => (bytes #xff #x00))
(check (to-bytes write-swf-u16 #x100) => (bytes #x00 #x01))
(check (to-bytes write-swf-u32 #xff) => (bytes #xff #x00 #x00 #x00))
(check (to-bytes write-swf-u32 #x100) => (bytes #x00 #x01 #x00 #x00))

(check (to-bytes *write-bits '(0 0 0 1 0 0 0 0)) => (bytes #x10))
(check (to-bytes *write-bits '(0 0 0 0 1 1 1 1  0 0 0 1 0 0 0 0)) => (bytes #x0f #x10))
(check (to-bytes write-bits '(0 0 0 0 1 1 1 1  0 0 0 1)) => (bytes #x0f #x10))
(check (to-bytes write-rgb '(#x12 #x34 #x56)) => (bytes #x12 #x34 #x56))

(process-args '("-w" "100" "-h" "200" "-c" "Class" "-o" "out.swf" "f1.abc" "f2.abc")
              () #f #f #f #f
  (lambda (infiles outfile width height classname)
    (check infiles => '("f1.abc" "f2.abc"))
    (check width => "100")
    (check height => "200")
    (check classname => "Class")
    (check outfile => "out.swf")
))

(process-args '("file.abc") () #f #f #f #f
  (lambda (infiles outfile width height classname)
    (check infiles => '("file.abc"))
    (check width => "100")
    (check height => "100")
    (check classname => #f)
    (check outfile => "file.swf")))

(check (call-with-output-bytes
        (lambda (out) (record-header-body 0 (bytes) out))) => (bytes 0 0))
(check (call-with-output-bytes
        (lambda (out) (record-header-body 1 (bytes 1 2 3) out))) => (bytes #x43 00 1 2 3))

(check (nbits-unsigned '(255)) => 8)
(check (nbits-unsigned '(256)) => 9)
