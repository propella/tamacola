;; cat.k -- concatenate and print files
;;
;; ./abc-run.sh example/cat.k example/*.k

(define argv (slot-getq avmplus/System argv))
(define i (slot-getq argv length))

(while (< 0 i)
  (let ((name (slot-get argv (- i 1))))
    (send avmplus/System write
	  (send avmplus/File read name)))
  (set! i (- i 1)))
