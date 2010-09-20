COLA = ../../cola.sh

all : peg boot grammar num struct plus calc

boot :
	$(COLA) peg-boot-test.k

peg :
	$(COLA) peg-test.k

num :
	$(COLA) num-test.k

struct :
	$(COLA) struct-test.k

plus :
	$(COLA) plus-test.k

calc :
	$(COLA) calc-test.k < /dev/null

grammar :
	$(COLA) peg-grammar-test.k

