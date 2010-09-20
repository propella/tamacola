build : .FORCE
	$(MAKE) -C stage2

boot : cola/cola .FORCE
	$(MAKE) -C boot
	$(MAKE) -C runtime
	$(MAKE) -C stage2 BOOTCC=../runtime/tamacc MKSWF=../runtime/mkswf tamacc.swf mkswf.swf
	$(MAKE) -C bin

test : cola/cola test-boot test-runtime test-stage2

all : test boot run-example workspace benchmark

cola/cola :
	$(MAKE) -C cola

test-stage2 :
	$(MAKE) -C runtime
	$(MAKE) -C stage2 BOOTCC=../runtime/tamacc MKSWF=../runtime/mkswf test

test-runtime :
	$(MAKE) -C runtime test

test-boot :
	$(MAKE) -C boot test

run-example :
	$(MAKE) -C example
	$(MAKE) -C ws

workspace :
	$(MAKE) -C ws

# Measure building speed of bin/tamacc.swf
benchmark :
	$(MAKE) -C stage2 clean
	time $(MAKE) -C stage2

# Make sure old bin/tamacc is still working.
test-old :
	git checkout bin
	time $(MAKE) -C stage2 clean goal

clean :
	$(MAKE) -C bin clean
	$(MAKE) -C boot clean
	$(MAKE) -C abcsx clean
	$(MAKE) -C runtime clean
	$(MAKE) -C stage2 clean
	$(MAKE) -C example clean
	$(MAKE) -C forall clean
	$(MAKE) -C ruby clean
	$(MAKE) -C ws clean
	$(MAKE) -C paper clean

# todo: *.g file *.scm are not included. pegtest/* are test.
count :
	@echo "\n all cola source code"
	find boot abcsx runtime -name '*.k' -or -name '*.g' -or -name '*.scm' | xargs wc -l
	@echo "\n test code only"
	find boot abcsx runtime -name '*-test.k' -or -name '*.scm' | xargs wc -l

demo :
	$(MAKE) -C ws Workspace.swf
	$(MAKE) -C forall Workspace.swf

archive :
	git clone ssh://tinlizzie.org/git/tamacola tamacola
	cd tamacola; \
		git submodule update --init; \
		git clean -f; \
		rm -rf cola paper; \
		rm -rf abcsx/.git .git

.FORCE :
