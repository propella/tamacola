;; An interactive demo

(define *stage* "undefined")

(define listener
  (lambda (e)
    (let ((x (slot-getq e stageX))
	  (y (slot-getq e stageY))
	  (r (* (send Math random) 256))
	  (g (* (send Math random) 256))
	  (b (* (send Math random) 256))
	  (alpha (/ 20 100))
	  (graphics (slot-getq *stage* graphics)))

    (let ((status (+ (+ x ", ") y)))
      (send graphics beginFill (+ (+ (* r 65536) (* g 256)) b) alpha)
      (send graphics drawCircle x y 10)
      (send graphics endFill)
      (slot-setq! (slot-getq *stage* field) text status)
))))

(class interactive (flash.display/Sprite) (field)
       (constructsuper)
       (set! *stage* #self)
       (set! field (new flash.text/TextField))
       (send #self addChild field)
       (send (slot-getq #self parent) addEventListener "mouseMove" listener)
)
