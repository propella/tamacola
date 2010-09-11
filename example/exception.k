;; Exception handler example

(define f
  (lambda ()
    (trace "With exception: ")
    (try-catch e String
               (let ()
                 (throw "OK")
                 (trace "NG"))
               (trace e)
               )
    (trace "Without exception: ")
    (try-catch e String
               (trace "OK")
               (trace "NG")
               )))
(f)
