(define for-each-array
  (lambda (fn ary)
    (let ((len (array-length ary))
	  (i 0))
      (while (< i len)
	     (fn (slot-get ary i))
	     (set! i (+ i 1))))))

(define for-each-array2
  (lambda (fn ary ary2)
    (let ((len (array-length ary))
	  (i 0))
      (if (not (= len (array-length ary2)))
	  (error "The length don't match")
	  '())
      (while (< i len)
           (fn (slot-get ary i) (slot-get ary2 i))
           (set! i (+ i 1))))))

(define index-iterator
  (lambda (array mask)
    (let ((current 0)
	  (len (array-length array)))
      (if (not (array? mask))
	  (lambda ()
	      (set! current (+ current 1))
	      (- current 1))
	  (lambda ()
	    (while (and (< current len) (< (slot-get mask current) 0))
		   (set! current (+ current 1)))
	    (if (< current len)
		(let ((ret (slot-get mask current)))
		  (set! current (+ current 1))
		  ret)
		'()))))))

(define do-action
  (lambda (array mask expression)
    (let* ((iterator (index-iterator array mask))
	   (i 0)
 	   (current (iterator))
	   (len (array-length array)))
      (while (and (not (null? current) (< i len)))
	     (if (>= current 0)
		 (expression (slot-get array current) i)
		 '())
	     (set! i (+ i 1))
	     (set! current (iterator))))))

(define assign
  (lambda (array mask expression)
    (let* ((iterator (index-iterator array mask))
	   (i 0)
 	   (current (iterator))
	   (len (array-length array)))
      (while (and (not (null? current) (< i len)))
	     (if (>= current 0)
		 (slot-set! array current (expression i))
		 '())
	     (set! i (+ i 1))
	     (set! current (iterator))))))

;;========================================================

;; (let* ((a [1 1 1 1])
;;        (m [0 1 -1 -1])
;;        (b [0 1 2 3])
;;        (e (lambda (elem i) (slot-set! a i (+ elem 1)))))
;;   (check (begin (assign a m (lambda (i) (slot-get b i))) a) => [0 1 1 1])
;;   (check (begin (do-action b m e) a) => [1 2 1 1]))

(define cross-product-iterator
  (lambda (col arity condition detect?)
    (let* ((length (array-length col))
	   (i 0)
	   (n (send Math pow length arity))
	   (args (new Array arity)))
      (if (<= length 0)
	  (lambda () '())
	  (lambda ()
	    (let ((ret '()))
	      (while (and (null? ret) (< i n))
		     (let ((t i)
			   (j 0))
		       (while (< j arity)
			      (slot-set! args j (slot-get col (modulo t length)))
			      (set! t (int (/ t length)))
			      (set! j (+ j 1)))
		       (set! ret (if (send condition apply () args)
				     (begin
				       (if detect? (set! i n) '())
				       args)
				     '()))
		     (set! i (+ i 1))))
	      ret))))))

(define cross-product-do
  (lambda (col arity condition action detect?)
    (let ((length (array-length col)))
      (if (> length 0)
	  (let ((n (send Math pow length arity))
		(i 0)
		(args (new Array arity)))
	    (while (< i n)
		   (let ((t i)
			 (j 0))
		     (while (< j arity)
			    (slot-set! args j (slot-get col (modulo t length)))
			    (set! t (int (/ t length)))
			    (set! j (+ j 1)))
		     (if (send condition apply () args)
			 (begin (send action apply () args)
				(if detect? (set! i n) '()))
			 '()))
		   (set! i (+ i 1))))
	  '()))))

(define cross-product-do-select-action
  (lambda (col arity condition action)
    (cross-product-do col arity condition action #f)))

(define cross-product-do-detect-action
  (lambda (col arity condition action)
    (cross-product-do col arity condition action #t)))

(define ranges-iterator
  (lambda (ranges)
    (let ((includes (lambda (r c)
		      (if (pair? r)
			  (if (= (car r) 'range-i)
			      (and (<= (cadr r) c) (<= c (caddr r)))
			      (and (<= (cadr r) c) (< c (caddr r))))
			  (= r c))))
	  (next (lambda (r c)
		  (+ c (if (pair? r)
			   (cadddr r)
			   1)))))
    (if (null? ranges)
	(lambda () '())
	(let* ((current-range ranges)
	       (current (if (pair? (car current-range))
			    (cadr (car current-range))
			    (car current-range))))
	    (lambda ()
	      (let ((ret '()))
		(while (and (null? ret) (not (null? current-range)))
		       (if (includes (car current-range) current)
			   (let ((c current))
			     (set! current (next (car current-range) current))
			     (set! ret c))
			   (set! current-range (cdr current-range))))
		ret)))))))

(define cross-product-ranges-generator
  (lambda (ranges-list body)
    (if (null? ranges-list)
	body
	(let ((v (car (car ranges-list)))
	      (ranges (car ranges-list)))
	  (if (= (length (cdr (cadr ranges))) 1)
	      (let* ((range (cdar (cdr (cadr ranges)))))
		(let* (
		     (key (cadr (car range)))
		     (init (cadr range))
		     (stop (caddr range))
		     (step (car (cdddr range))))
		`(do ((,v ,init (+ ,v ,step)))
		     ((or (< ,v ,init) (,(if (= key 'ranges-i) '<= '<) ,stop ,v)))
		 ,(cross-product-ranges-generator (cdr ranges-list) body))))
	      `(let ((iterator (ranges-iterator ,@(cdr ranges)))
		     (,v '()))
		 (while (let () (set! ,v (iterator)) (not (null? ,v)))
			,(cross-product-ranges-generator (cdr ranges-list) body))))))))

(define cross-product-range-generator
  (lambda (ranges body)
    (if (null? ranges)
	body
	(let* ((range (car ranges))
	       (v (car range))
	       (spec (cdr (car (cdr range))))
	       (key (cadr (car spec)))
	       (init (cadr spec))
	       (stop (caddr spec))
	       (step (car (cdddr spec))))
;	      (trace "key=" key)
;	      (trace "init=" init)
;	      (trace "stop=" stop)
;	      (trace "step=" step)
	      `(do ((,v ,init (+ ,v ,step)))
		   ((or (< ,v ,init) (,(if (= key 'ranges-i) '<= '<) ,stop ,v)))
		 ,(cross-product-range-generator (cdr ranges) body))))))

(define cross-product-iteration-generator
  (lambda (var-list body format format-index)
    (if (null? var-list)
	body
	(cross-product-iteration-generator
	 (cdr var-list)
	 `(do ((,(car var-list) 0 (+ 1 ,(car var-list))))
	      ((>= ,(car var-list) (/ (slot-get ,format ,format-index) (slot-get ,format ,(- format-index 1)))))
	    ,body)
	 format (+ format-index 1)))))

(define slot-assign-generator
  (lambda (names result v body)
    (if (not (null? names))
	(slot-assign-generator (cdr names) `(+ ,result (* ,(concat-symbol '_ (car names)) ,(car names))) v body)
	`(slot-set! (FArray-next ,v) ,result ,body))))

(define slot-access-generator
  (lambda (exprs current result v)
    (if (not (null? exprs))
	(slot-access-generator (cdr exprs) (+ current 1) `(+ ,result (* ,(intern (string-append "_" (String current))) ,(car exprs))) v)
	`(slot-get (FArray-current ,v) ,result))))

(define slot-access-generator-for-query
  (lambda (names result base av v)
    (if (null? names)
	`(let () (set! ,av (slot-get (FArray-current ,base) ,result)) ,v)
	(slot-access-generator-for-query (cdr names) `(+ ,result (* ,(concat-symbol '_ (car names)) ,(car names))) base av v))))

(define format-to-index-generator
  (lambda (names index varName)
    (if (not (null? names))
	`((,(concat-symbol '_ (car names)) (slot-get ,varName ,index)) ,@(format-to-index-generator (cdr names) (+ index 1) varName))
	'())))

;(trace (slot-assign-generator '(i j k) 'a '(+ 3 4)))
;(trace (slot-assign-generator '(i) 'a '(+ 3 4)))
;(trace (format-to-index-generator '(i j k) 0 'fff))
;(trace (format-to-index-generator '(i) 0 'ggg))

(define-structure FArray (current next format))

(define new-FArray
  (lambda (n)
    (let ((obj (new typeof-FArray)))
      (slot-setq! obj current (new Array n))
      (slot-setq! obj next (new Array n))
      (slot-setq! obj format [1 n])
      obj)))

(define new-FArary-with-list
  (lambda (l)
    (let ((obj (new typeof-FArray))
	  (num (fold * 1 l))
	  (prevBase 0)
	  (base 1))
      (slot-setq! obj current (new Array num))
      (slot-setq! obj next (new Array num))
      (slot-setq! obj format (list->array (map (lambda (x) (set! prevBase base) (set! base (* base x)) prevBase) (reverse (cons 1 l)))))
      obj)))

(define flip-FArray
  (lambda (arr)
    (let ((t (FArray-next arr)))
      (set-FArray-next arr (FArray-current arr))
      (set-FArray-current arr t))))

(define FArrayFlip flip-FArray)

(define length-FArray
  (lambda (arr)
    (array-length (FArray-current arr))))

(define toString-FArray
  (lambda (arr)
    (let* ((stream (open-output-string))
	   (f (FArray-format arr))
	   (format (if (= (modulo (array-length f) 2) 0) (list->array (append (array->list f) (list (slot-get f (- (array-length f) 1))))) f))
	   (current (FArray-current arr))
	   (widest 0))
      (define printRow
	(lambda (index taskX taskY)
	  (if (< taskY 1)
	      (begin 
		(printCol index taskX)
		(next-put stream #"\n"))
	      (let ((rep (/ (slot-get format (+ taskY 1)) (slot-get format taskY)))
		    (step (slot-get format taskY))
		    (next 0))
		(while (< next (* rep step))
		       (printRow (+ index next) taskX (- taskY 2))
		       (set! next (+ next step)))))))
      (define printCol
	(lambda (index task)
	  (if (< task 1)
	      (let ((rep (slot-get format 1))
		    (i 0))
		(while (< i rep)
		       (putElem (+ index i))
		       (if (= i (- rep 1)) '() (next-put stream #" "))
		       (set! i (+ i 1))))
	      (let ((rep (/ (slot-get format (+ task 1)) (slot-get format task)))
		    (step (slot-get format task))
		    (next 0))
		(while (< next (* rep step))
		       (printCol (+ index next) (- task 2))
		       (set! next (+ next step))
		       (if (= next (* (- rep 1) step)) '() (next-put stream #" ")))))))
      (define putElem
	(lambda (index)
	  (let* ((str (String (slot-get current index)))
		 (len (string-length str)))
	    (next-put stream #" ")
	    (next-put-all stream str))))
      (for-each-array (lambda (x) (set! widest (max x widest))) current)
      (printRow 0 (- (array-length format) 3) (- (array-length format) 2))
      (get-output-string stream))))

(define FArrayToString toString-FArray)
      
(define fill-FArray
  (lambda (arr v)
    (let ((i 0)
	  (c (FArray-next arr)))
      (while (< i (array-length c))
	     (slot-set! c i v)
	     (set! i (+ i 1))))))

(define fill-FArray2
  (lambda (arr v)
    (fill-FArray arr v)
    (flip-FArray arr)
    (fill-FArray arr v)))

;(define arr (new-FArary-with-list '(2 3 4)))

;(trace (FArray-next arr))
;(trace (FArray-format arr))

;(set-FArray-next arr (list->array (iota 24)))

;(trace (toString-FArray arr))

;; (fill-FArray arr 1)
;; (trace (FArray-next arr))
;; (trace (FArray-current arr))

;; (flip-FArray arr)
;; (trace (FArray-next arr))
;; (trace (FArray-current arr))

;; (fill-FArray arr 2)
;; (trace (FArray-next arr))
;; (trace (FArray-current arr))

(define-structure FElement (color x y))

(define new-FElement
  (lambda (c xx yy)
    (let ((obj (new typeof-FElement)))
      (slot-setq! obj color c)
      (slot-setq! obj x xx)
      (slot-setq! obj y yy)
      obj)))

