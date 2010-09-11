;; SRFI-78 style lightweight testing with pretty-string

(require "boot/pretty-print.k")
(pretty-print-columns 120)

(define *check-write* pretty-print)
