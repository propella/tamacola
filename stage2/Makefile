# Makefile for self building stage
#
# NOTE:
# - All rules for test case are prefixed by "test-"
# - All source code for test cases are postfixed by "-test.k"

AVM = avmshell
MKSWF = ../bin/mkswf
BOOTCC = ../bin/tamacc
ABCSX = $(AVM) abcsx.swf --

goal : tamacc.swf mkswf.swf

test : test-std test-pattern test-stream test-peg test-writer test-pretty test-peg-grammar test-asm test-platform test-compile test-macro test-swf
all : test asm-run run

# Command line tool binary

abcsx.swf : std.abc stream.abc peg-lib.abc peg-grammar.abc instruction.abc abc.abc test.abc writer.abc pretty-print.abc platform-shell.abc abcsx.abc
	$(MKSWF) -o $@ $^

tamacc.swf : std.abc pattern.abc stream.abc peg-lib.abc peg-grammar.abc writer.abc pretty-print.abc instruction.abc abc.abc syntax.abc compile.abc platform-shell.abc peephole.abc tamacc.abc
	$(MKSWF) -o $@ $^

mkswf.swf : std.abc stream.abc platform-shell.abc swf.abc mkswf.abc
	$(MKSWF) -o $@ $^

run-tamacola : tamacola.swf
	$(AVM) tamacola.swf

core.abc : core.k peg.k
	$(BOOTCC) -r -o $@ $^

tamacola.swf : std.abc pattern.abc stream.abc peg-lib.abc peg-grammar.abc writer.abc pretty-print.abc instruction.abc abc.abc syntax.abc compile.abc platform-shell.abc core.abc tamacola.abc
	$(MKSWF) -o tamacola.swf $^

# Test case

test-std : std.abc std-test.abc
	$(AVM) $^

test-pattern : std.abc pattern.abc pattern-test.abc
	$(AVM) $^

test-stream : std.abc stream.abc stream-test.abc
	$(AVM) $^

test-peg : stream.abc std.abc peg-test.abc
	$(AVM) $^

test-writer : std.abc stream.abc writer.abc writer-test.abc
	$(AVM) $^

test-pretty : std.abc stream.abc writer.abc pretty-print.abc pretty-print-test.abc
	$(AVM) $^

test-peg-grammar : stream.abc std.abc writer.abc peg-lib.abc peg-grammar.abc peg-grammar-test.abc
	$(AVM) $^

test-asm : abcsx.swf
	$(ABCSX) -test

test-compile : std.abc stream.abc instruction.abc abc.abc peg-lib.abc compile.abc syntax.abc pattern.abc platform-shell.abc compile-test.abc
	$(AVM) $^

test-macro : tamacola.swf macro-test.k
	$(AVM) tamacola.swf -- macro-test.k

test-platform : std.abc stream.abc instruction.abc abc.abc peg-lib.abc compile.abc syntax.abc pattern.abc platform-shell.abc platform-shell-test.abc
	$(AVM) $^

test-swf : std.abc stream.abc swf.abc swf-test.abc
	$(AVM) $^

# Making Grammar

syntax.k : mkpeg.swf syntax.g
	$(AVM) mkpeg.swf -- -n syntax syntax.g

mkpeg.swf : std.abc stream.abc writer.abc pretty-print.abc peg-lib.abc peg-grammar.abc platform-shell.abc mkpeg.abc
	$(MKSWF) -o $@ $^ 

# Building rules

%.abc : %.k core.k
	$(BOOTCC) core.k $<

peg-test.abc : core.k peg.k peg-lib.k peg-test.k
	$(BOOTCC) $^

%.abc : %.scm
	$(BOOTCC) core.k $<

peg-grammar.abc : core.k peg.k peg-grammar.k
	$(BOOTCC) -o $@ $^

compile.abc : core.k peg.k compile.k syntax-lib.k
	$(BOOTCC) -o $@ $^

syntax.abc : core.k peg.k syntax.k
	$(BOOTCC) $^

peephole.abc : mkpeg.swf peephole.g
	$(AVM) mkpeg.swf -- peephole.g
	$(BOOTCC) -o $@ ../runtime/core.k ../runtime/peg.k peephole.k

../runtime/tamacc.swf :
	make -C ../runtime tamacc.swf

# Run examples

asm-run : abcsx.swf abcsx-run
	$(MAKE) RUNASM=$(PWD)/abcsx-run -C ../abcsx run

run : tamacc.swf tamacc
	$(MAKE) -C ../example TAMACC=$(PWD)/tamacc run

# Source files (todo: there must be a shorter way to write them)

core.k : ../runtime/core.k
	cp $< $@
std.k : ../runtime/std.k
	cp $< $@
std-test.k : ../runtime/std-test.k
	cp $< $@
pattern.k : ../boot/pattern.k
	cp $< $@
pattern-test.k : ../boot/pattern-test.k
	cp $< $@
stream.k : ../runtime/stream.k
	cp $< $@
stream-test.k : ../runtime/stream-test.k
	cp $< $@
peg.k : ../runtime/peg.k
	cp $< $@
peg-lib.k : ../runtime/peg-lib.k
	cp $< $@
peg-grammar.k : ../boot/peg-grammar.k ## TODO: It Should Be Generated From .g
	cp $< $@
peg-test.k : ../boot/pegtest/peg-test.k
	cp $< $@
writer.k : ../runtime/writer.k
	cp $< $@
writer-test.k : ../runtime/writer-test.k
	cp $< $@
peg-grammar-test.k : ../boot/pegtest/peg-grammar-test.k
	cp $< $@
instruction.k : ../abcsx/instruction.k
	cp $< $@
abc.k : ../abcsx/abc.k
	cp $< $@
test.scm : ../abcsx/test.scm
	cp $< $@
platform-shell.k : ../runtime/platform-shell.k
	cp $< $@
platform-shell-test.k : ../runtime/platform-shell-test.k
	cp $< $@
abcsx.k : ../runtime/abcsx.k
	cp $< $@
compile.k : ../runtime/compile.k
	cp $< $@
compile-test.k : ../boot/compile-test.k
	cp $< $@
syntax.g : ../boot/syntax.g
	cp $< $@
syntax-lib.k : ../boot/syntax-lib.k
	cp $< $@
peephole.g : ../runtime/peephole.g
	cp $< $@
pretty-print.k : ../runtime/pretty-print.k
	cp $< $@
pretty-print-test.k : ../runtime/pretty-print-test.k
	cp $< $@
macro-test.k : ../runtime/macro-test.k
	cp $< $@
swf.k : ../abcsx/swf.k
	cp $< $@
swf-test.k : ../abcsx/swf-test.k
	cp $< $@
mkswf.k : ../runtime/mkswf.k
	cp $< $@
mkpeg.k : ../runtime/mkpeg.k
	cp $< $@
tamacc.k : ../runtime/tamacc.k
	cp $< $@
tamacola.k : ../runtime/tamacola.k
	cp $< $@
abcsx-run : ../runtime/abcsx-run
	cp $< $@
tamacc : ../runtime/tamacc
	cp $< $@
tamacola : ../runtime/tamacola tamacola.swf
	cp $< $@

# Clean up

clean :
	rm -f *.abc *.swf *.k *.g *.scm abcsx-run tamacc tamacola
