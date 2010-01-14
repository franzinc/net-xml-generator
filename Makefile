# user Makefile for the xml generator

incl = $(shell if test -f ../../makefile.top; then echo yes; else echo no; fi)

ifeq ($(incl),yes)
include ../../makefile.top
include ../../makefile.defs
endif

ifeq ($(OS_NAME),windows)
runlisp = ../lisp +s build.tmp +B +cn -I dcl -q -batch
else
runlisp = ../lisp -I dcl +s build.tmp -q -batch
endif

SOURCES = Makefile net-xml-generator.cl README.md \
	xml-generator-blurb.cl xml-generator-blurb.html

default: clean
	rm -f build.tmp build.out
ifeq ($(OS_NAME),windows)
	echo '(dribble "build.out")' >> build.tmp
endif
	echo '(compile-file "net-xml-generator.cl" :recompile t)' >> build.tmp
	echo '(exit 0)' >> build.tmp
	$(runlisp)
ifeq ($(OS_NAME),windows)
	cat build.out
endif

clean: FORCE
	rm -f *.fasl

install: FORCE
ifndef DESTDIR
	@echo DESTDIR not defined
	exit 1
endif
	cp -p net-xml-generator.fasl $(DESTDIR)/code

FORCE:
