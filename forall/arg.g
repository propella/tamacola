;; -*- fundamental -*-

ttrans 		 = (.:sym) :origSym '( 'compoundStmt ((<ttrans sym (NEXT)>:s (_ -> (trace "cdr s" (cdr s)) ) -> (cdr s):sym) -> `(,@ss ,(car s)):ss)* ) -> `((begin ,@ss) . ,origSym)
		 | .:sym '( 'ident .:n )  	      		    -> (cons (if (memq? n sym) 'found n) sym)
		 | .:sym '( 'decl .:n )  	      		    -> (begin (trace "cons" (cons n sym)) (cons '() (cons n sym)))

tstart		 = <ttrans '(c) (NEXT)>

