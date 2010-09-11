SYMBOL = 'hello -> 'hello ;

LIST-MANY = '( .* ) ;

NESTED = 'foo -> 'foo
     | '( NESTED :e ) -> (list e) ;

MYRULE = . :e <LIST-MANY e> ;
