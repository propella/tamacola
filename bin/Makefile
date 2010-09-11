# This makefile build provided tools.
#
# todo: write rules to build from the boot stage.

TARGET = tamacola tamacola.swf tamacc tamacc.swf mkswf mkswf.swf abcsx abcsx.swf

all : $(TARGET)

# Shell scripts

tamacola : ../runtime/tamacola
	cp $< $@
tamacc : ../runtime/tamacc
	cp $< $@
mkswf : ../runtime/mkswf
	cp $< $@
abcsx : ../runtime/abcsx
	cp $< $@

# SWF binary files

tamacola.swf : .FORCE
	$(MAKE) -C ../stage2 $@
	cp ../stage2/$@ .
tamacc.swf : .FORCE
	$(MAKE) -C ../stage2 $@
	cp ../stage2/$@ .
mkswf.swf : .FORCE
	$(MAKE) -C ../stage2 $@
	cp ../stage2/$@ .
abcsx.swf : .FORCE
	$(MAKE) -C ../stage2 $@
	cp ../stage2/$@ .

clean :
	rm -f $(TARGET)

.FORCE :