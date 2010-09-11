TAMACC = ../bin/tamacc
MKSWF = ../bin/mkswf

ifeq ($(shell uname),Darwin)
  OPEN = open # fdb
else
  ifeq ($(shell uname),Linux)
    # OPEN = ???
  else
    ifeq ($(findstring MINGW32,$(shell uname)),MINGW32)
      OPEN = start
    endif
  endif
endif

STAGE2 = ../stage2/std.abc ../stage2/stream.abc ../stage2/peg-lib.abc ../stage2/peg-grammar.abc ../stage2/writer.abc ../stage2/pretty-print.abc ../stage2/instruction.abc ../stage2/abc.abc ../stage2/swf.abc ../stage2/compile.abc ../stage2/syntax.abc ../stage2/pattern.abc ../stage2/core.abc

RUBY  = ../ruby/ruby.abc ../ruby/ruby-support.abc ../ruby/ruby-trans.abc ruby-workspace.abc
FORALL = ../forall/particles.abc ../forall/forall-support.abc ../forall/forall.abc ../forall/forall-trans.abc forall-workspace.abc

run : Workspace.swf
	$(OPEN) Workspace.swf

all : Workspace.swf Repl.swf
	$(OPEN) Workspace.swf
	$(OPEN) Repl.swf

Repl.swf : stage2 platform-flash.abc Repl.abc
	$(MKSWF) -w 300 -h 400 -o $@ -c Repl $(STAGE2) platform-flash.abc Repl.abc

Workspace.swf : stage2 ruby forall platform-flash.abc ruby-workspace.abc peg-workspace.abc forall-workspace.abc peg-memo-macro.abc Workspace.abc
	$(MKSWF) -w 640 -h 480 -o $@ -c Workspace $(STAGE2) platform-flash.abc peg-workspace.abc $(RUBY) $(FORALL) peg-memo-macro.abc Workspace.abc

stage2 : .FORCE
	$(MAKE) -C ../stage2 tamacola swf.abc

ruby : .FORCE
	$(MAKE) -C ../ruby ruby.abc ruby-support.abc ruby-trans.abc

forall : .FORCE
	$(MAKE) -C ../forall particles.abc forall-support.abc forall.abc forall-trans.abc

peg-memo-macro.abc : ../runtime/peg-memo-macro.k
	$(TAMACC) -r -o $@ ../runtime/core.k $<

%.abc : %.k
	$(TAMACC) -o $@ ../runtime/core.k $<

%.abc : ../runtime/%.k ../runtime/core.k
	$(TAMACC) -o $@ ../runtime/core.k $<

.FORCE :

clean :
	rm -f *.swf *.abc

test : test-workspace

test-workspace :
	../stage2/tamacola workspace-test.k
