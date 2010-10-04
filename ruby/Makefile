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

test-ruby.trans : ruby.abc ruby-support.abc ruby-trans.abc ruby-trans-test.abc
	../stage2/tamacola $^

test-ruby.syntax : ruby.abc ruby-support.abc ruby-test.abc
	../stage2/tamacola $^

ruby.abc  : ruby.g ../runtime/peg-memo-macro.k
	$(AVM) ../stage2/mkpeg.swf -- -n ruby $<
	$(TAMACC) -o $@ ../runtime/core.k ../runtime/peg.k ../runtime/peg-memo-macro.k $(subst .g,,$<).k

ruby-trans.abc  : ruby-trans.g
	$(AVM) ../stage2/mkpeg.swf -- -n ruby-trans $<
	$(TAMACC) -o $@ ../runtime/core.k ../runtime/peg.k $(subst .g,,$<).k

%.abc : %.k
	$(TAMACC) -o $@ ../runtime/core.k $<

clean :
	rm -f *.abc *.swf ruby.k ruby-trans.k


