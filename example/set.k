;; Set variable

(define foo "Some value")
(set! foo "Updating a global variable is ok!")

(let ((foo "Some value"))
  (set! foo "Updating a local variable is ok!")
  (print foo))

(print foo)
