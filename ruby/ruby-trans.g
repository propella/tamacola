;; -*- fundamental -*-

rhead       	 =  .
rident		 = '( 'ident .:n )						-> n
rrange		 = '( ('range-i | 'range):op rtrans:s rtrans:e )		-> `(list ,op ,s ,e)

rmake-do-list	 = .:n .:range 	  	     	      	       			-> `(,n ,(caddr range) (+ ,n 1))
		   

rtrans 		 = '( 'compStmt rtrans*:ss )					-> `(begin ,@ss)
		 | '( 'def push-scope rident:f
                      '( 'args (rident:n <add-var-to-scope n>)*:a )
                       rtrans:s pop-scope:v)					-> `(define ,f
		       								      (lambda (,@a)
										      	(let ,(lset-difference v (->list a)) ,s)))
		 | '( ('cond -> '(cond):r)
		       '( rtrans:c rtrans*:ss -> `(,@r (,c ,@ss)):r)*)   	-> r
		 | '( 'for '( rident:n <add-var-to-scope n>)
		      	    rrange:range <rmake-do-list n range>:l rtrans:s)	-> `(do (,l) (,(if (= (car range) 'range-i)
		      	      	       	 	      		      	       	   	     	   `(> ,n ,(caddr (cdr range)))
		      	      	       	 	      		      	       	   	     	   `(>= ,n ,(caddr (cdr range)))))
											,s)
		 | '( 'while rtrans:c rtrans:s) 				-> `(while ,c ,s)
		 | '( 'ident ('true						-> '#t
		      	     |'false						-> '#f
			     |'nil						-> '()
			     |'self						-> #self
			     | .:n (&<var-in-scope n>		       	     	-> n
			           |&<ruby-isUppercase n>			-> n 
		      	     	   | __		  				-> `(send #self ,n)
				   )))
		 | '( 'numeric .:n)						-> n
		 | '( 'index rtrans:n rtrans:i)					-> `(slot-get ,n ,i)
		 | '( 'assign rident:n rtrans:e <add-var-to-scope n>)		-> `(set! ,n ,e)
		 | '( 'assign '( 'index-lhs rtrans:l rtrans:i) rtrans:e )	-> `(slot-set! ,l ,i ,e)
		 | '( 'call rident:n . rtrans*:a) 		  		-> (if (= n 'print)
		      	    	       						       `(trace ,@a)
										       `(,n ,@a))
		 | '( 'send rident:sel rtrans:rec . rtrans*:a) 		  	-> (if (= sel 'new)
		      	    	       		    				       (let ((ll (->list a)))
		      	    	       		    				         (if (= (length ll) 2)
										             `(let ((ret (new ,rec ,(car ll)))
											            (v ,(cadr ll)))
											        (do ((i 0 (+ i 1)))
											          ((>= i ,(car ll)))
												  (slot-set! ret i v))
												ret)
											     `(new ,rec ,@a)))
		      	    	       		    				       `(send ,rec ,sel ,@a))
                 | '( rhead:t rtrans*:ts )					-> `(,t ,@ts)
                 | .

rtstart		 = rtrans*:x							-> (->list x)

