;; redefine a local variable

(let ((a "Hello, World!"))
  (let ((a a))
    (print a)))
