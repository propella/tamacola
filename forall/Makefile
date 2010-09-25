AVM = avmshell
TAMACC = ../bin/tamacc
ABCRUN = $(TAMACC) -x
MKSWF = ../bin/mkswf
TAMACOLA = ../stage2/tamacola

ifeq ($(shell uname),Darwin)
  OPEN = open
else
  ifeq ($(shell uname),Linux)
    # OPEN = ???
  else
    ifeq ($(findstring MINGW32,$(shell uname)),MINGW32)
      OPEN = start
    endif
  endif
endif

test-forall.trans : particles.abc forall.abc forall-support.abc forall-trans.abc forall-trans-test.abc
	$(TAMACOLA) $^

test-forall : forall.abc ../runtime/peg-memo-macro.abc forall-support.abc forall-test.abc
	$(AVM) ../stage2/std.abc ../stage2/stream.abc ../stage2/peg-lib.abc ../stage2/peg-grammar.abc ../stage2/platform-shell.abc forall.abc forall-support.abc forall-test.abc

test-forall.shell : particles.k forall.k forall-support.k forall-trans.k forall-trans-test.k
	$(TAMACOLA) $^

test-golly : golly.abc golly-test.abc
	$(TAMACOLA) $^

test-infix : infix.abc infix-test.abc
	$(TAMACOLA) $^

forall-life.swf : particles.abc forall-life.abc
	$(MKSWF) -w 100 -h 100 -c Dots -o $@ ../stage2/std.abc $^
	$(OPEN) $@

forall.abc  : forall.g ../runtime/peg-memo-macro.k
	$(AVM) ../stage2/mkpeg.swf -- -n forall $<
	$(TAMACC) -o $@ ../runtime/core.k ../runtime/peg.k ../runtime/peg-memo-macro.k $(subst .g,,$<).k

forall-trans.abc  : forall-trans.g
	$(AVM) ../stage2/mkpeg.swf -- -n forall-trans $<
	$(TAMACC) -o $@ ../runtime/core.k ../runtime/peg.k $(subst .g,,$<).k

infix.abc  : infix.g ../runtime/peg-memo-macro.k
	$(AVM) ../stage2/mkpeg.swf -- $<
	$(TAMACC) -o $@ ../runtime/core.k ../runtime/peg.k ../runtime/peg-memo-macro.k $(subst .g,,$<).k

%.abc   : %.g
	$(AVM) ../stage2/mkpeg.swf -- $<
	$(TAMACC) -o $@ ../runtime/core.k ../runtime/peg.k $(subst .g,,$<).k

%.abc : %.k
	$(TAMACC) -o $@ ../runtime/core.k $<

%.abc : ../runtime/%.k ../runtime/core.k
	$(TAMACC) -o $@ ../runtime/core.k $<

clean :
	rm -f *.abc *.swf forall.k forall-trans.k

.FORCE :

