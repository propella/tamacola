;; Hello World with GUI

(import flash.display)
(import flash.text)

(class Hello (Sprite) ()
       (constructsuper)
       (let ((field (new TextField)))
	 (slot-setq! field text "Hello, World!")
         (assemble (debugfile "textField.k") (debugline 7) (pushundefined))
         (trace "Hello, World!")
	 (send #self addChild field)))
