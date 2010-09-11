;; Define ::namespace.
(define greeting "Hello, Again!")

;; Define human::greeting
(in-ns "human")
(define greeting "**BAD!! Hello, Hell!**\n")
(define bye "bye!")

;; Re-assign human::greeting
(in-imports "human")
(set! greeting "Hello, World!\n")

;; Define cat::greeting
(define cat/greeting "Namespaces work, meow.\n")

;; The trace function can not be used because the imports excludes "" namespace.
((slot-get avmplus/System "write") greeting)

(in-imports "cat")
((slot-get avmplus/System "write") greeting)

;; Show a value in "" namespace
(in-ns "")
(in-imports "")
(trace greeting)
(import human)
(trace bye)
