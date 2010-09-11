;; -*- fundamental -*-

thead       	 =  .:a
tident		 = '( 'ident .:n )		 -> n

trange		 = .:sym '( ('range | 'range-i):key <ttrans sym (NEXT)>:start
		   	    	      		    <ttrans sym (NEXT)>:stop
						    <ttrans sym (NEXT)>:step ) -> `(list (quote ,key) ,start ,stop ,step)

tmake-list	 = '( .+:ret ) -> `(list ,@ret)

tassign-body	 = .:sym .:body .:ret .:names (
		   	'( 'index-v tident:n
			    '( (tident:i -> `(,@names ,i):names)
			       .:m <trange sym m>:r -> `(,@ret (,i ,r)):ret )+ )
			   	       	        -> (cons n `(let ((format (FArray-format ,n))) (let ,(format-to-index-generator (reverse names) 0 'format) ,(cross-product-range-generator ret (slot-assign-generator names 0 n body)))))
		 	  | tident:n	-> (if (assq n sym)
			    		       (cons (car (car (cdr (assq n sym))))
			    		   	     (slot-assign-generator (cdr (car (cdr (assq n sym)))) 0 (car (car (cdr (assq n sym)))) body))
			    		       (cons '() `(define ,n ,body)))
			 )

tassign		 = .:sym '( 'assign .:n  <ttrans sym (NEXT)>:rhs ) <tassign-body sym rhs '() '() n>:n

tarray-decl	 = '( 'array-decl tident+:n ) 	-> (->list n)

tfind-variables  = .:sym
		   ( '( 'such '( 'ident .:orig) . '( 'array-decl '( 'ident .:n ) tident+:ids) ) -> (cons `(,n (,orig ,@ids)) sym)
		   | '( 'all '( 'ident .:orig) '( '( 'array-decl '( 'ident .:n ) tident+:ids) -> (cons `(,n (,orig ,@ids)) sym):sym)+) -> sym
		   )

make-query 	 = .:sym ('( 'such <ttrans sym (NEXT)>:a <ttrans sym (NEXT)>:cond tarray-decl:t) .:rhs
		   	    	  	      		    -> `(let ,t (let ((format (FArray-format ,a)))
		      	    			      	       	    	  (let ,(format-to-index-generator (reverse (cdr t)) 0 'format) ,(cross-product-iteration-generator (reverse (cdr t)) (slot-access-generator-for-query (reverse (cdr t)) 0 a (car t) (if (null? cond) rhs `(if ,cond ,rhs '()))) 'format 1))))
			 |'( 'all <ttrans sym (NEXT)>:a '( tarray-decl:t )) .:rhs
			     	  	      		    -> `(let ,t (let ((format (FArray-format ,a)))
		      	    			      	       	    	  (let ,(format-to-index-generator (reverse (cdr t)) 0 'format) ,(cross-product-iteration-generator (reverse (cdr t)) (slot-access-generator-for-query (reverse (cdr t)) 0 a (car t) rhs ) 'format 1))))
			)

tquery		 = .:sym '( 'query 'do &<tfind-variables sym (NEXT)>:s .:lhs <ttrans s (NEXT)>:rhs ) <make-query s lhs rhs>

ttrans 		 = .:sym (
		     '( 'compoundStmt (.:a <tstmt sym a>:s -> `(,@ss ,s):ss)* )        -> `(begin ,@ss)
		      | '( 'index-e <ttrans sym (NEXT)>:b '( (.:a <ttrans sym a>)*:e ) ) -> `(let ((format (FArray-format ,b))) (let ,(format-to-index-generator (map (lambda (i) (intern (String (- i 1)))) (iota (length (->list e)))) 0 'format) ,(slot-access-generator (reverse (->list e)) 0 0 b)))
		 | '( ('ident | 'scalar | 'string) .:n )     	      -> n
		 | '( 'if <ttrans sym (NEXT)>:c <ttrans sym (NEXT)>:t <ttrans sym (NEXT)>:f )  -> `(if ,c ,t ,f)
		 | '( 'or <ttrans sym (NEXT)>:x <ttrans sym (NEXT)>:y )	     	     	      -> `(let ((v ,x)) (if v v ,y))
		 | '( 'and <ttrans sym (NEXT)>:x <ttrans sym (NEXT)>:y )	       	      -> `(let ((v ,x)) (if v ,y v))
		 | '( 'prop <ttrans sym (NEXT)>:p <ttrans sym (NEXT)>:m )	      	      -> `(slot-getq ,p ,m)
		 | '( 'send 'null <ttrans sym (NEXT)>:sel (.:a <ttrans sym a>)*:m )	      -> `(,sel ,@m)
		 | '( 'send <ttrans sym (NEXT)>:rec <ttrans sym (NEXT)>:sel (.:a <ttrans sym a>)*:m )	 -> `(send ,rec ,sel ,@m)
		 | '( 'new '( (.:n <ttrans sym n>)*:ts ) ) <tmake-list (reverse (->list ts))>:ts  -> `(new-FArary-with-list ,ts)
		 | '( 'lambda '( tident*:v ) (.:n <ttrans sym n>)*:s)	   	       				  -> `(lambda ,(->list v) ,@s)
		 | '( 'return (.:n <ttrans sym n>):s)	   	       				  	  	  -> `(return ,s)
                 | '( thead:t (.:n <ttrans sym n>)*:ts ) -> `(,t ,@ts)
                 | .) 

tstmt            = .:sym (
		      .:a <tassign sym a>:a	-> (cdr a)
		    | .:a <tquery sym a>
		    | .:a <ttrans sym a>
                    )

ttop-stmt        = .:sym (
                      .:a <tassign sym a>:a	-> (if (null? (car a)) (cdr a) `(let () ,(cdr a) (flip-FArray ,(car a))))
		    | .:a <tquery sym a>	-> 
		    | .:a <ttrans sym a>
                    )


tstart		 = .:a <tstmt '() a>

tstarts		 = '( tstart*:t ) -> (->list t)
