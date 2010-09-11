AVM = avmshell
BOOTCC = ../boot/tamacc
MKSWF= ../boot/mkswf
ABCRUN = ./tamacc -x
ABCSX = $(AVM) abcsx.swf --

ASMFILES =  std.abc stream.abc peg-lib.abc peg-grammar.abc instruction.abc abc.abc test.abc writer.abc pretty-print.abc platform-shell.abc abcsx.abc

CCFILES = std.abc pattern.abc stream.abc peg-lib.abc peg-grammar.abc writer.abc pretty-print.abc instruction.abc abc.abc syntax.abc compile.abc platform-shell.abc tamacc.abc

## Main targets

goal : mkswf.swf tamacc.swf

all :  test asm-run run goal abcsx.swf tamacola.swf

test : test-std test-pattern test-stream test-peg test-writer test-peg-grammar test-asm test-platform test-compile test-macro test-plus test-swf

asm-run : abcsx.swf
	$(MAKE) RUNASM=../runtime/abcsx-run -C ../abcsx run

run : tamacc.swf
	$(MAKE) TAMACC=../runtime/tamacc -C ../example run

clean :
	rm -f *.abc *.swf syntax.k peg-grammar?.k

## Commnands

abcsx.swf : $(ASMFILES)
	$(MKSWF) -o $@ $(ASMFILES)

tamacc.swf : $(CCFILES)
	$(MKSWF) -o $@ $(CCFILES)

mkswf.swf : std.abc stream.abc platform-shell.abc swf.abc mkswf.abc
	$(MKSWF) -o $@ $^

tamacola.swf : std.abc stream.abc peg-grammar.abc writer.abc pretty-print.abc instruction.abc abc.abc peg-lib.abc syntax.abc compile.abc pattern.abc platform-shell.abc core.abc tamacola.abc
	$(MKSWF) -o tamacola.swf $^

mkpeg.swf : std.abc stream.abc writer.abc pretty-print.abc peg-lib.abc peg-grammar.abc platform-shell.abc mkpeg.abc
	$(MKSWF) -o $@ $^ 

## Tests

test-std : std.abc std-test.abc
	$(AVM) $^

test-pattern : std.abc pattern.abc pattern-test.abc
	$(AVM) $^

test-stream : std.abc stream.abc stream-test.abc
	$(AVM) $^

test-writer : std.abc stream.abc writer.abc writer-test.abc
	$(AVM) $^

test-pretty : std.abc stream.abc writer.abc pretty-print.abc pretty-print-test.abc
	$(AVM) $^

test-peg : stream.abc std.abc peg-lib.abc peg-test.abc
	$(AVM) $^

test-peg-grammar : stream.abc std.abc writer.abc peg-lib.abc peg-grammar.abc peg-grammar-test.abc
	$(AVM) $^

REGRESSION = std.abc stream.abc writer.abc pretty-print.abc peg-lib.abc platform-shell.abc

test-peg-regression : $(REGRESSION) peg-grammar.abc mkpeg.abc
	$(AVM) $(REGRESSION) peg-grammar.abc mkpeg.abc -- -o peg-grammar1.k ../boot/peg-grammar.g
	$(BOOTCC) -o peg-grammar1.abc core.k peg.k peg-grammar1.k
	$(AVM) $(REGRESSION) peg-grammar1.abc mkpeg.abc -- -o peg-grammar2.k ../boot/peg-grammar.g
	$(BOOTCC) -o peg-grammar2.abc core.k peg.k peg-grammar2.k
	$(AVM) $(REGRESSION) peg-grammar2.abc mkpeg.abc -- -o peg-grammar3.k ../boot/peg-grammar.g
	diff peg-grammar1.k peg-grammar3.k

test-asm : abcsx.swf
	$(ABCSX) -test

test-compile : std.abc stream.abc instruction.abc abc.abc peg-lib.abc compile.abc syntax.abc pattern.abc platform-shell.abc compile-test.abc
	$(AVM) $^

test-platform : std.abc stream.abc instruction.abc abc.abc peg-lib.abc compile.abc syntax.abc pattern.abc platform-shell.abc platform-shell-test.abc
	$(AVM) $^

test-swf : std.abc stream.abc swf.abc swf-test.abc
	$(AVM) $^

test-macro : tamacola.swf
	$(AVM) tamacola.swf -- macro-test.k

test-plus : tamacola.swf
	$(AVM) tamacola.swf -- ../boot/pegtest/num-test.k

## Source Code Generation

../boot/peg-grammar.k : ../boot/peg-grammar.g
	$(MAKE) -C ../boot peg-grammar.k

syntax.k : mkpeg.swf ../boot/syntax.g
	$(AVM) mkpeg.swf -- -n syntax -o syntax.k ../boot/syntax.g

## Generic Build Rules

%.abc : %.k
	$(BOOTCC) core.k $<

%.abc : ../boot/%.k
	$(BOOTCC) -o $@ core.k $<

%.abc : ../boot/pegtest/%.k
	$(BOOTCC) -o $@ core.k $<

%.abc : ../abcsx/%.k
	$(BOOTCC) -o $@ core.k $<

%.abc : ../abcsx/%.scm
	$(BOOTCC) -o $@ core.k $<

## Special Build Rules (It requires additional macros)

peg-grammar.abc : core.k peg.k ../boot/peg-grammar.k
	$(BOOTCC) -o $@ $^

peg-test.abc : core.k peg.k ../boot/pegtest/peg-test.k
	$(BOOTCC) -o $@ $^

syntax.abc : core.k peg.k syntax.k
	$(BOOTCC) -o $@ $^

compile.abc : core.k peg.k compile.k ../boot/syntax-lib.k
	$(BOOTCC) -o $@ $^

core.abc : core.k peg.k
	$(BOOTCC) -r -o $@ $^

