;; -*- fundamental -*-

head       	 =  .:a
ident		 = '( 'ident .:n )		 -> n

range		 = .:sym '( ('range | 'range-i):key <trans sym (NEXT)>:start
		   	    	      		    <trans sym (NEXT)>:stop
						    <trans sym (NEXT)>:step ) -> `(list (quote ,key) ,start ,stop ,step)

make-list	 = '( .+:ret ) -> `(list ,@ret)

assign-body	 = .:sym .:body .:ret .:names (
		   	'( 'index-v ident:n
			    '( (ident:i -> `(,@names ,i):names)
			       .:m <range sym m>:r -> `(,@ret (,i ,r)):ret )+ )
			   	       	        -> (cons n `(let ((format (FArray-format ,n))) (let ,(format-to-index-generator (reverse names) 0 'format) ,(cross-product-range-generator ret (slot-assign-generator names 0 n body)))))
		 	  | ident:n	-> (if (assq n sym)
			    		       (cons (car (car (cdr (assq n sym))))
			    		   	     (slot-assign-generator (cdr (car (cdr (assq n sym)))) 0 (car (car (cdr (assq n sym)))) body))
			    		       (cons '() `(define ,n ,body)))
			 )

assign		 = .:sym '( 'assign .:n  <trans sym (NEXT)>:rhs ) <assign-body sym rhs '() '() n>:n

array-decl	 = '( 'array-decl ident+:n ) 	-> (->list n)

find-variables  = .:sym
		   ( '( 'such '( 'ident .:orig) . '( 'array-decl '( 'ident .:n ) ident+:ids) ) -> (cons `(,n (,orig ,@ids)) sym)
		   | '( 'all '( 'ident .:orig) '( '( 'array-decl '( 'ident .:n ) ident+:ids)   -> (cons `(,n (,orig ,@ids)) sym):sym)+) -> sym
		   )

make-query 	 = .:sym ('( 'such <trans sym (NEXT)>:a <trans sym (NEXT)>:cond array-decl:t) .:rhs
		   	    	  	      		    -> `(let ,t (let ((format (FArray-format ,a)))
		      	    			      	       	    	  (let ,(format-to-index-generator (reverse (cdr t)) 0 'format) ,(cross-product-iteration-generator (reverse (cdr t)) (slot-access-generator-for-query (reverse (cdr t)) 0 a (car t) (if (null? cond) rhs `(if ,cond ,rhs '()))) 'format 1))))
			 |'( 'all <trans sym (NEXT)>:a '( array-decl:t )) .:rhs
			     	  	      		    -> `(let ,t (let ((format (FArray-format ,a)))
		      	    			      	       	    	  (let ,(format-to-index-generator (reverse (cdr t)) 0 'format) ,(cross-product-iteration-generator (reverse (cdr t)) (slot-access-generator-for-query (reverse (cdr t)) 0 a (car t) rhs ) 'format 1))))
			)

query		 = .:sym '( 'query 'do &<find-variables sym (NEXT)>:s .:lhs <trans s (NEXT)>:rhs ) <make-query s lhs rhs>

trans 		 = .:sym (
		     '( 'compoundStmt (.:a <stmt sym a>:s -> `(,@ss ,s):ss)* )        -> `(begin ,@ss)
		      | '( 'index-e <trans sym (NEXT)>:b '( (.:a <trans sym a>)*:e ) ) -> `(let ((format (FArray-format ,b))) (let ,(format-to-index-generator (map (lambda (i) (intern (String (- i 1)))) (iota (length (->list e)))) 0 'format) ,(slot-access-generator (reverse (->list e)) 0 0 b)))
		 | '( ('ident | 'scalar | 'string) .:n )     	      -> n
		 | '( 'if <trans sym (NEXT)>:c <trans sym (NEXT)>:t <trans sym (NEXT)>:f )  -> `(if ,c ,t ,f)
		 | '( 'or <trans sym (NEXT)>:x <trans sym (NEXT)>:y )	     	     	      -> `(let ((v ,x)) (if v v ,y))
		 | '( 'and <trans sym (NEXT)>:x <trans sym (NEXT)>:y )	       	      -> `(let ((v ,x)) (if v ,y v))
		 | '( 'prop <trans sym (NEXT)>:p <trans sym (NEXT)>:m )	      	      -> `(slot-getq ,p ,m)
		 | '( 'send 'null <trans sym (NEXT)>:sel (.:a <trans sym a>)*:m )	      -> `(,sel ,@m)
		 | '( 'send <trans sym (NEXT)>:rec <trans sym (NEXT)>:sel (.:a <trans sym a>)*:m )	 -> `(send ,rec ,sel ,@m)
		 | '( 'new '( (.:n <trans sym n>)*:ts ) ) <make-list (reverse (->list ts))>:ts  -> `(new-FArary-with-list ,ts)
		 | '( 'lambda '( ident*:v ) (.:n <trans sym n>)*:s)	   	       				  -> `(lambda ,(->list v) ,@s)
		 | '( 'return (.:n <trans sym n>):s)	   	       				  	  	  -> `(return ,s)
                 | '( head:t (.:n <trans sym n>)*:ts ) -> `(,t ,@ts)
                 | .) 

stmt            = .:sym (
		      .:a <assign sym a>:a	-> (cdr a)
		    | .:a <query sym a>
		    | .:a <trans sym a>
                    )

top-stmt        = .:sym (
                      .:a <assign sym a>:a	-> (if (null? (car a)) (cdr a) `(let () ,(cdr a) (flip-FArray ,(car a))))
		    | .:a <query sym a>	-> 
		    | .:a <trans sym a>
                    )


start		 = .:a <stmt '() a>

starts		 = '( start*:t ) -> (->list t)
