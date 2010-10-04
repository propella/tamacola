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

PICTURES = stack.png buildprocess.png tools.png

run : Slide.swf
	-$(MAKE) $(PICTURES)
	$(OPEN) $<

Slide.swf : stage2 ../ws/peg-memo-macro.abc ../ws/platform-flash.abc Slide.abc 
	$(MKSWF) -w 800 -h 600 -o $@ -c Slide $(STAGE2) ../ws/peg-memo-macro.abc ../ws/platform-flash.abc Slide.abc

Roppongi.swf : stage2 ../ws/peg-memo-macro.abc ../ws/platform-flash.abc Slide.abc roppongi.abc
	$(MKSWF) -w 800 -h 600 -o $@ -c Slide $(STAGE2) ../ws/peg-memo-macro.abc ../ws/platform-flash.abc Slide.abc roppongi.abc

stage2 : .FORCE
	$(MAKE) -C ../stage2 tamacola swf.abc

../ws/peg-memo-macro.abc :
	$(MAKE) -C ../ws peg-memo-macro.abc

stack.png : ../paper/stack.eps
	convert -density 160 $< $@

buildprocess.png : ../paper/buildprocess.eps
	convert -density 110 $< $@
tools.png : ../paper/tools.eps
	convert -density 220 $< $@

%.abc : %.k
	$(TAMACC) -o $@ ../runtime/core.k $<

tamacola-st : Roppongi.swf .FORCE
	rm -rf $@
	mkdir $@
	cp Roppongi.swf $@
	cp Roppongi.xml $@
	cp Cat.mp3 Goose.mp3 $@
	cp Roppongi.html $@


.FORCE :

clean :
	rm -f *.swf *.abc $(PICTURES)
