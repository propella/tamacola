AVM = avmshell
TAMACC = ../bin/tamacc
ABCRUN = $(TAMACC) -x
MKSWF = ../bin/mkswf
TAMACOLA = ../stage2/tamacola
BENCH = nfib10
OPEN = start

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

all : run test-redefine test-macro-shell rest

run :
	$(ABCRUN) hello.k
	$(ABCRUN) 3plus4.k
	$(ABCRUN) if.k
	$(ABCRUN) call.k
	$(ABCRUN) callfunc.k
	$(ABCRUN) set.k
	$(ABCRUN) send.k
	$(ABCRUN) new.k
	$(ABCRUN) class.k
	$(ABCRUN) let.k
	$(ABCRUN) let2.k
	$(ABCRUN) while.k
	$(ABCRUN) letfunc.k
	$(ABCRUN) letfunc2.k
	$(ABCRUN) letfunc3.k
	$(ABCRUN) closure.k
	$(ABCRUN) nfibs.k
	$(ABCRUN) cat.k -- cat.k
	$(ABCRUN) macro.k
	$(ABCRUN) exception.k
	$(ABCRUN) undefined.k
	$(ABCRUN) namespace.k
	$(ABCRUN) namespace2.k
	$(ABCRUN) vector.k

test-redefine : re-define1.abc re-define2.abc re-define3.abc
	$(AVM) $^

rest : hello-swf interactive calcFlash benchmark

benchmark :
	time $(TAMACC) $(BENCH).k
	time $(AVM) $(BENCH).abc

calc :
	$(ABCRUN) ../runtime/core.k ../runtime/std.k calc.k

hello-swf : textField.abc
	$(MKSWF) -w 100 -h 100 -c Hello textField.abc
	$(OPEN) textField.swf

interactive : interactive.abc
	$(MKSWF) -w 200 -h 200 -c interactive interactive.abc
	$(OPEN) interactive.html

calcFlash : std.abc calcFlash.abc
	$(MKSWF) -w 300 -h 400 -o calcFlash.swf -c calcFlash std.abc calcFlash.abc
	$(OPEN) calcFlash.html

test : test.k particles.abc
	$(TAMACC) ../runtime/core.k test.k
	$(MKSWF) -w 500 -h 500 -c Boxes ../stage2/std.abc particles.abc test.abc
	$(OPEN) test.swf

%.abc   : %.g
	$(AVM) ../stage2/mkpeg.swf -- $<
	$(TAMACC) -o $@ ../runtime/core.k ../runtime/peg.k $(subst .g,,$<).k

%.abc : %.k
	$(TAMACC) -o $@ ../runtime/core.k  $<

%.abc : ../runtime/%.k ../runtime/core.k
	$(TAMACC) -o $@ ../runtime/core.k $<


# simple infix syntax (calculator) test

%.abc : %.g ../stage2/mkpeg.swf
	$(AVM) ../stage2/mkpeg.swf -- $<
	$(TAMACC) -o $@ ../runtime/core.k ../runtime/peg.k $(subst .g,,$<).k

../stage2/mkpeg.swf :
	$(MAKE) -C ../stage2 mkpeg.swf

test-infix : tamacola
	$(TAMACOLA) ../stage2/mkpeg.k infix.g
	$(TAMACOLA) infix.k infix-test.k

test-macro-shell : tamacola
	$(TAMACOLA) macro-shell1.k macro-shell2.k
	rm macro-shell2.abc
	$(TAMACOLA) macro-shell1.k macro-shell2.k

tamacola :
	$(MAKE) -C ../stage2 tamacola

clean :
	rm -f *.abc *.swf infix.k
