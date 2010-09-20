peephole = .:r '( ( '( 'pop ) &'( 'pushundefined ) .
	          | .:i								     -> (cons i r):r)*)

;	       	  | '( 'lessequal -> 'lfle | 'lessthan -> 'lflt):key &'( 'iftrue ) . -> `(,@r (,key)):r
									 -> `(,r)

pcode0	 = '( 'code :h (.:n <peephole '() (reverse n)>):p) -> `(,h ,@(reverse p)) | .

defs	 = '( pcode0*:pp ) -> `(,@pp)

pmethods = '( 'method :h '( defs*:p ) ) -> `(,h (,@p)) | .

pstart   = '( 'asm :h pmethods*:p )  -> `(,h ,@p) | .
