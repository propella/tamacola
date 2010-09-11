DUMP=./abcsx.ss -dump
ASM=./abcsx.ss -asm

#DUMP=./abcsx-gauche.scm -dump
#ASM=./abcsx-gauche.scm -asm

RUNASM := ./runasm.sh
SWFMAKE = ./swf-gauche.scm

REGRESSION = examples/textField

run :
	$(RUNASM) examples/hello.sx
	$(RUNASM) examples/type.sx
	$(RUNASM) examples/parseInt.sx
	$(RUNASM) examples/arithmetic.sx
	$(RUNASM) examples/ifte.sx
	$(RUNASM) examples/callLocal.sx
	$(RUNASM) examples/call.sx
	$(RUNASM) examples/send.sx
	$(RUNASM) examples/closure.sx
	$(RUNASM) examples/class.sx
	$(RUNASM) examples/namespace.sx
	$(RUNASM) examples/array.sx
	$(RUNASM) examples/with.sx
	$(RUNASM) examples/exception.sx
	$(RUNASM) examples/vector.sx
#	$(RUNASM) examples/activation.sx -- works only before tamarin 711

run-gosh :
	$(MAKE) ASM="./abcsx-gauche.scm -asm" run

partial : examples/partial1.sx.abc examples/partial2.sx.abc
	avmshell examples/partial1.sx.abc examples/partial2.sx.abc

all : test test-dump run run-gosh test-define partial test-regression test-swf

# unit test
test :
	./abcsx.ss -test
	./abcsx-gauche.scm -test

# dump test
test-dump : examples/textField.abc
	$(DUMP) examples/textField.abc

# swf test
test-swf : Hello.swf
	open Hello.swf

test-debug : Hello.swf
	fdb Hello.swf

Hello.swf : examples/textField.sx
	$(ASM) examples/textField.sx
	$(SWFMAKE) -w 100 -h 100 -c Hello -o $@ examples/textField.sx.abc

# regression test
test-regression :
	$(ASM) $(REGRESSION).sx
	$(DUMP) $(REGRESSION).sx.abc > $(REGRESSION)2.sx
	$(ASM) $(REGRESSION)2.sx
	$(DUMP) $(REGRESSION)2.sx.abc > $(REGRESSION)3.sx
	diff $(REGRESSION)2.sx $(REGRESSION)3.sx

clean :
	rm -f *.abc examples/*.abc Hello.swf
	rm -f $(REGRESSION)2.sx $(REGRESSION)3.sx

%.abc : %.as
#	asc $<
	asc -import ~/src/tamarin-central/core/builtin.abc -import examples/flashglobal.as $<

%.sx.abc : %.sx
	$(ASM) $<

test-define : examples/re-define1.sx.abc examples/re-define2.sx.abc examples/re-define3.sx.abc
#	avmshell -Dverbose=interp $^
	avmshell examples/re-define1.sx.abc examples/re-define2.sx.abc examples/re-define3.sx.abc
#	avmshell examples/re-define2.sx.abc examples/re-define1.sx.abc examples/re-define3.sx.abc
