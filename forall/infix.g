__               = [ \t\n\r]*
in-d          = [0123456789]

in-m          	  = in-m:a "*" in-d:b -> `(* ,a ,b)
		  | in-m:a "/" in-d:b -> `(/ ,a ,b)
		  | in-d:b  	  	 -> b


in-a          	  = in-a:a "+" in-m:b -> `(+ ,a ,b)
		  | in-a:a "-" in-m:b -> `(- ,a ,b)
		  | in-m:b  	  	 -> b

in-start	  = in-a
