#
# This is a multipurpose makefile
#
# Procedures:
# IMPORTANT: splint main.c
# Profiling: gprof project
# kprof is graphical gprof: kprof
# Anotated code : gprof -l -A -x project
# Better anotated code : make coverage (then just less the .c.cov)
# WARNING just use kcachegrind it is good:
#                    valgrind --tool=callgrind -v ./project && kcachegrind callgrind.out.4330
# Memory alignment stuff
# pahole project
# valgrind --tool=cachegrind -v ./project

######### IMPORTANT DO NOT COMMENT THESE FLAGS

# The compiler to use
CC        = gcc
# The name of the executable to produce
EXEC      = project
# Ansi flags, make the compiler more informative
ANSI      = -ansi -pedantic -Wall -Wextra -std=c99
# Puts cacheline size in a macro
CLS       = -DCLS=$(getconf LEVEL1_DCACHE_LINESIZE)
# Use this if you want to use SSE2 instructions
SSE2      = -msse2
# Debug flags for gdb/GUD
DEBUG     = -O0 -gdwarf-2 -g3 -ggdb
#DEBUG     = -O0 -gdwarf-2
# Profile flags for gprof
PROFILE   = -pg -fprofile-arcs -ftest-coverage
# Optimization flags for production
OPTIMIZE  = -O9 -D NDEBUG
# Static for static StandAlones
STATIC = -static
# PThreads are usefull
THREADS = -pthread

#########  COMMENT THESE

# Uncomment the flags that you need in a particular compile
# Always use ANSI
CFLAGS   = $(ANSI)
#CFLAGS  += $(DEBUG)
#CFLAGS  += -D_BSD_SOURCE
#CFLAGS  += $(PROFILE)
CFLAGS  += $(OPTIMIZE)
#CFLAGS  += $(CLS) $(SSE2)
#CFLAGS  += $(STATIC)
#CFLAGS  += $(THREADS)

##  Base Makefile
##  Diferent classes of files
#CS    = $(wildcard *.c)
CS    = splayLCT.c splayTree.c struct.c smallMain.c splayPrinter.c
HS    = $(wildcard *.h)
IS    = $(addsuffix .i, $(basename $(CS)))
SS    = $(addsuffix .s, $(basename $(IS)))
OS    = $(addsuffix .o, $(basename $(SS)))
HOLES = $(addsuffix .hls, $(basename $(SS)))
COVS  = $(addsuffix .c.gcov, $(basename $(CS)))
STATS = $(addsuffix .gcda, $(basename $(CS))) $(addsuffix .gcno, $(basename $(CS)))

# Phony targets
.PHONY: all coverage clean tags depend dox check

# Default Compile
# Commented jLCT.h libSplayLCT.so libPointerLCT.so
all: $(SS) $(EXEC) $(HOLES) check pcheck CLI pCLI tester linTester splayLCT.so pointerLCT.so graphTester

coverage: $(COVS)

## Linking rule
$(EXEC): $(OS)
	@echo Linking: $@
	$(CC) $(CFLAGS) $(OS) -o $@ -lm -lc -lbsd

## Run Preprocessor
%.i: %.c
	@echo PreProcess: $<
	$(CC) $(CFLAGS) -E -o $@ $<

## Generate Assembly
%.s: %.i
	@echo Assemble: $<
	$(CC) $(CFLAGS) -S -o $@ $<

## Build Object
%.o: %.s
	@echo Build Object from: $<
	$(CC) $(CFLAGS) -c -o $@ $<

## Find Holes
%.hls: %.o
	@echo Holes in: $<
	touch $@
	pahole -I -H 1 $< | sed 's/.* \(.\+:[0-9]\+\) .*/\1:1:/'

## Pack using directory links
%.tgz: $(CS) $(HS) Makefile
	@echo Pack Using directory
	tar -chvzf $@ $(basename $@ .tgz)

## Coverage statistics
%.c.gcov: %.gcda %.gcno
	@echo Anotated coverage source $<
	gcov -a -b -f $<
	dos2unix $@

## Make dependencies
depend: depend.mak

## Make dependencies forcing creation of all files
depend.mak: $(CS) $(HS)
	@echo Making dependencies ...
	$(CC) -MM $(CS) | sed 's/\.o/\.i/g' > depend.mak

-include depend.mak

## Clean up
clean:
	@echo Cleaning Up
	rm -f $(EXEC) $(STATS) $(COVS) $(IS) $(SS) $(OS) gmon.out depend.mak *~ callgrind.out.* cachegrind.out.* CLI pCLI tester linTester pointerLCT.o splayLCT.so pointerLCT.so jLCT.class jLCT.h libSplayLCT.so libPointerLCT.so {pointer,splay}{In,Out}put *.hls graphTester

## TAGS
tags: TAGS
TAGS: $(CS) $(HS) commonLCT.c
	@echo Making TAGS
	etags $^

## doc, make documentation with doxymacs
dox: $(CS) $(HS) Doxyfile
	doxygen

## run splint for checking code
check: splayLCT.c splayTree.c struct.c LCT.h
	splint $^

pcheck: pointerLCT.c LCT.h
	splint $^

CLI: splayLCT.o splayTree.o CLI.c splayPrinter.o
	@echo Linking: $@
	$(CC) $(CFLAGS) $^ -o $@ -lm -lc -DSPRINT

pCLI: pointerLCT.o CLI.c
	@echo Linking: $@
	$(CC) $(CFLAGS) $^ -o $@ -lm -lc

tester: pointerLCT.o mkTests.c
	@echo Linking: $@
	$(CC) $(CFLAGS) $^ -o $@ -lbsd

linTester: pointerLCT.o mkLinTests.c
	@echo Linking: $@
	$(CC) $(CFLAGS) $^ -o $@ -lbsd

graphTester: splayLCT.o splayTree.o graphTests.c
	@echo Linking: $@
	$(CC) $(CFLAGS) $^ -o $@ -lbsd

splayLCT.so: LCT.h splayPrinter.h splayTree.c splayLCT.c splayPrinter.c
	@echo Linking: $@
	$(CC) $(CFLAGS) -fPIC -shared $^ -o $@

pointerLCT.so: LCT.h pointerLCT.c
	@echo Linking: $@
	$(CC) $(CFLAGS) -fPIC -shared $^ -o $@

jLCT.class: jLCT.java
	javac -cp .:/usr/share/java/jna.jar $^

jLCT.h: jLCT.class
	javah -jni jLCT

JAVA_HOME = "/usr/lib/jvm/default-runtime"

libSplayLCT.so: jLCT.c LCT.h splayTree.c splayLCT.c
	$(CC) $(CFLAGS) -shared -fPIC -I$(JAVA_HOME)/include -I$(JAVA_HOME)/include/linux $^ -o $@

libPointerLCT.so: jLCT.c LCT.h pointerLCT.c
	$(CC) $(CFLAGS) -shared -fPIC -I$(JAVA_HOME)/include -I$(JAVA_HOME)/include/linux $^ -o $@

# Run with
# java -cp . -Djava.library.path=. jLCT
#
# gdb --args python LCT.py
#
