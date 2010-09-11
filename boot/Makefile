COLA = ../cola.sh
TAMACC = ../tamacc
ABCRUN = $(TAMACC) -x
AVM = avmshell

export ABCSX = ../cola.sh abcsx.k

goal : peg-grammar.k

all : goal test asm-run run

%.sx : %.k
	$(COLA) tamacc.k -S $< > $@

%.k : %.g
	$(COLA) mkpeg.k $<

test : test-std test-pattern test-pretty test-writer test-stream test-asm test-peg-regression test-peg test-compile test-swf

test-std :
	$(COLA) std-test.k

test-pattern :
	$(COLA) pattern-test.k

test-pretty :
	$(COLA) pretty-print-test.k

test-writer :
	$(COLA) writer-test.k

test-stream :
	$(COLA) stream-test.k

test-swf :
	$(COLA) mkswf.k -test

test-asm :
	$(COLA) abcsx.k -test

test-peg : peg-grammar.k
	$(MAKE) -C pegtest

test-peg-regression :
	$(COLA) mkpeg.k peg-grammar.g -n peg -o peg-grammar1.k -g peg-boot.k
	$(COLA) mkpeg.k peg-grammar.g -n peg -o peg-grammar2.k -g peg-grammar1.k 
	$(COLA) mkpeg.k peg-grammar.g -n peg -o peg-grammar3.k -g peg-grammar2.k
	diff peg-grammar2.k peg-grammar3.k

peg-grammar.k : peg-grammar.g
	$(COLA) mkpeg.k -n peg $< -o $@

test-compile :
	$(COLA) compile-test.k

asm-run :
	$(MAKE) RUNASM="../boot/abcsx-run" -C ../abcsx run

run : goal
	$(MAKE) ABCRUN="../boot/tamacc -x" -C ../example run

TARGET = let2
try :
	$(TAMACC) ../example/$(TARGET).k
	$(AVM) ../example/$(TARGET).abc

clean :
	rm -rf test.txt syntax.k peg-grammar1.k peg-grammar2.k peg-grammar3.k # peg-grammar.k 
