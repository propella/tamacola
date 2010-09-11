;; Create object and access test

(print (new Array 1 2 3))

(print (send (new Array 1 2 3) reverse))
(print (slot-getq Math PI))
(print (slot-get Math "PI"))

(let ((obj (new Object)))
  (slot-setq! obj slot "Slot access by compile time name done!")
  (print (slot-getq obj slot)))

(let ((obj (new Object)))
  (slot-set! obj "slot" "Slot access by runtime name done!")
  (print (slot-get obj "slot")))
