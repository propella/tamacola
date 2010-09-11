(define N 100)
(define myarray (new-FArary-with-list (list N N)))
(define *stage* '())
(define g '())

(define init-field
  (lambda ()
(let
 ()
 (let
  ((format (FArray-format myarray)))
  (let
   ((_j (slot-get format 0)) (_i (slot-get format 1)))
   (do
    ((i 0 (+ i 1)))
    ((or (< i 0) (< N i)))
    (do
     ((j 0 (+ j 1)))
     ((or (< j 0) (< N j)))
     (slot-set!
      (FArray-next myarray)
      (+ (+ 0 (* _i i)) (* _j j))
      (send Math floor (+ (send Math random) 0.5)))))))
 (flip-FArray myarray))
(let
 ()
 (let
  ((format (FArray-format myarray)))
  (let
   ((_j (slot-get format 0)) (_i (slot-get format 1)))
   (do
    ((i 0 (+ i 1)))
    ((or (< i 0) (< N i)))
    (do
     ((j 0 (+ j 1)))
     ((or (< j 0) (< N j)))
     (slot-set!
      (FArray-next myarray)
      (+ (+ 0 (* _i i)) (* _j j))
      (let
       ((format (FArray-format myarray)))
       (let
        ((_0 (slot-get format 0)) (_1 (slot-get format 1)))
        (slot-get (FArray-current myarray) (+ (+ 0 (* _0 j)) (* _1 i))))))))))
 (flip-FArray myarray))
))

(define draw-FArray
  (lambda (array g)
     (let ((data (FArray-current array))
 	   (len (length-FArray array)))
       (send g clear)
       (do ((i 0 (+ i 1)))
	   ((>= i len) #t)
	 (let ((c (slot-get data i)))
	   (send g beginFill (FElement-color c))
	   (send g drawRect (FElement-x c) (FElement-y c) 1 1))))))

(define mydraw-FArray
  (lambda ()
  (let ((str "%such{a == 1 | a[i][j] < myarray} do {g.drawRect(i, j, 1, 1);}"))
    ;(eval (parse-stream tstmt (->stream (cons (parse-collection lstmt str) '()))))
(let
 (a i j)
 (let
  ((format (FArray-format myarray)))
  (let
   ((_j (slot-get format 0)) (_i (slot-get format 1)))
   (do
    ((i 0 (+ 1 i)))
    ((>= i (/ (slot-get format 2) (slot-get format 1))))
    (do
     ((j 0 (+ 1 j)))
     ((>= j (/ (slot-get format 1) (slot-get format 0))))
     (let
      ()
      (set! a (slot-get (FArray-current myarray) (+ (+ 0 (* _j j)) (* _i i))))
      (if (= a 1) (let () (send g drawRect i j 1 1)) (quote ()))))))))
)))

(define draw-FArray-numbers
  (lambda ()
    (send g clear)
    "g.clear();"
    (send g beginFill 0xFF00FF)
    "g.beginFill(0xFF00FF);"
    (mydraw-FArray)))
;       (do ((i 0 (+ i 1)))
;	   ((>= i len) #t)
;	 (let ((c (slot-get data i)))
;	   (send g beginFill (FElement-color c))
;	   (send g drawRect (FElement-x c) (FElement-y c) 1 1))))))

(define next-life-step
  (lambda ()
(let
 ()
 (let
  ((format (FArray-format myarray)))
  (let
   ((_j (slot-get format 0)) (_i (slot-get format 1)))
   (do
    ((i 1 (+ i 1)))
    ((or (< i 1) (< N i)))
    (do
     ((j 1 (+ j 1)))
     ((or (< j 1) (< N j)))
     (slot-set!
      (FArray-next myarray)
      (+ (+ 0 (* _i i)) (* _j j))
      (+
       (+
        (+
         (+
          (+
           (+
            (+
             (+
              (let
               ((format (FArray-format myarray)))
               (let
                ((_0 (slot-get format 0)) (_1 (slot-get format 1)))
                (slot-get
                 (FArray-current myarray)
                 (+ (+ 0 (* _0 (- j 1))) (* _1 (- i 1))))))
              (let
               ((format (FArray-format myarray)))
               (let
                ((_0 (slot-get format 0)) (_1 (slot-get format 1)))
                (slot-get
                 (FArray-current myarray)
                 (+ (+ 0 (* _0 j)) (* _1 (- i 1)))))))
             (let
              ((format (FArray-format myarray)))
              (let
               ((_0 (slot-get format 0)) (_1 (slot-get format 1)))
               (slot-get
                (FArray-current myarray)
                (+ (+ 0 (* _0 (+ j 1))) (* _1 (- i 1)))))))
            (let
             ((format (FArray-format myarray)))
             (let
              ((_0 (slot-get format 0)) (_1 (slot-get format 1)))
              (slot-get
               (FArray-current myarray)
               (+ (+ 0 (* _0 (- j 1))) (* _1 i))))))
           (if
            (>
             (let
              ((format (FArray-format myarray)))
              (let
               ((_0 (slot-get format 0)) (_1 (slot-get format 1)))
               (slot-get (FArray-current myarray) (+ (+ 0 (* _0 j)) (* _1 i)))))
             0)
            10
            0))
          (let
           ((format (FArray-format myarray)))
           (let
            ((_0 (slot-get format 0)) (_1 (slot-get format 1)))
            (slot-get
             (FArray-current myarray)
             (+ (+ 0 (* _0 (+ j 1))) (* _1 i))))))
         (let
          ((format (FArray-format myarray)))
          (let
           ((_0 (slot-get format 0)) (_1 (slot-get format 1)))
           (slot-get
            (FArray-current myarray)
            (+ (+ 0 (* _0 (- j 1))) (* _1 (+ i 1)))))))
        (let
         ((format (FArray-format myarray)))
         (let
          ((_0 (slot-get format 0)) (_1 (slot-get format 1)))
          (slot-get (FArray-current myarray) (+ (+ 0 (* _0 j)) (* _1 (+ i 1)))))))
       (let
        ((format (FArray-format myarray)))
        (let
         ((_0 (slot-get format 0)) (_1 (slot-get format 1)))
         (slot-get
          (FArray-current myarray)
          (+ (+ 0 (* _0 (+ j 1))) (* _1 (+ i 1))))))))))))
 (flip-FArray myarray))
(let
 ()
 (let
  ((format (FArray-format myarray)))
  (let
   ((_j (slot-get format 0)) (_i (slot-get format 1)))
   (do
    ((i 0 (+ i 1)))
    ((or (< i 0) (< N i)))
    (do
     ((j 0 (+ j 1)))
     ((or (< j 0) (< N j)))
     (slot-set!
      (FArray-next myarray)
      (+ (+ 0 (* _i i)) (* _j j))
      (if
       (=
        (modulo
         (let
          ((format (FArray-format myarray)))
          (let
           ((_0 (slot-get format 0)) (_1 (slot-get format 1)))
           (slot-get (FArray-current myarray) (+ (+ 0 (* _0 j)) (* _1 i)))))
         10)
        3)
       1
       (if
        (=
         (modulo
          (let
           ((format (FArray-format myarray)))
           (let
            ((_0 (slot-get format 0)) (_1 (slot-get format 1)))
            (slot-get (FArray-current myarray) (+ (+ 0 (* _0 j)) (* _1 i)))))
          10)
         2)
        (int
         (/
          (let
           ((format (FArray-format myarray)))
           (let
            ((_0 (slot-get format 0)) (_1 (slot-get format 1)))
            (slot-get (FArray-current myarray) (+ (+ 0 (* _0 j)) (* _1 i)))))
          10))
        0)))))))
 (flip-FArray myarray))
))

(init-field)
(trace (FArray-current myarray))

(define count 0)

(define step
   (lambda ()
     (set! count (+ count 1))
     (next-life-step)
     (set! g (slot-getq *stage* graphics))
     (draw-FArray-numbers)
     (if (= (modulo count 100) 0) 
	 (trace "count" count)
	 '())
))


(class Dots (flash.display/Sprite) ()
       (constructsuper)
       (set! *stage* #self)
       (assemble (debugfile "test.k") (debugline 48) (pushundefined))
       (send *stage* addEventListener "enterFrame" step)
       )

