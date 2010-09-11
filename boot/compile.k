;;; -*- coke -*-

(require "boot/pretty-print.k")
(require "boot/peg.k")
(require "boot/peg-grammar.k")

(define syntax (namespace global 'syntax))
(in syntax (define expr ())) ;; forward

(require "boot/syntax-lib.k")
(grammar-load "boot/syntax.g" syntax)

(define abc-compile
  (lambda (exprs)
    (parse-expr2 (compiler) syntax/start exprs)))
