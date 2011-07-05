# -*- Mode: makefile; -*-
# Makefile for the SexpLib project

# to create sexpgrammar.[ch] use the following command:
# sbcl --load sexpgrammar.lisp sexpgrammar
CC = gcc-mp-4.5

CFLAGS = -ggdb -pg -ansi -pedantic -Wall -Wextra -Wmissing-include-dirs -Wswitch-default -Wswitch-enum -Wdeclaration-after-statement -Wmissing-declarations 
INCLUDES = -I .
LINKFLAGS = -L. -lsexp

OUTPUT_SRC = main.c
SOURCES := $(wildcard *.c)
HEADERS := $(wildcard *.h)
OBJECTS := $(patsubst %.c,%.o,$(SOURCES))
OBJECTS_LIB := $(filter-out $(patsubst %.c,%.o,$(OUTPUT_SRC)),$(OBJECTS))
OUTPUT = sexptest
OUTPUT_LIB = libsexp.a

%.o : %.c %.h
	$(CC) -c $(CFLAGS) $(DEFINES) $(INCLUDES) $< -o $@

$(OUTPUT): $(OUTPUT_LIB) 
	$(CC) $(patsubst %.c,%.o,$(OUTPUT_SRC)) -o $(OUTPUT) $(LINKFLAGS)

$(OUTPUT_LIB): $(OBJECTS)
	$(RM) -f $(OUTPUT_LIB)
	$(AR) cr $(OUTPUT_LIB) $(OBJECTS_LIB)
	ranlib $(OUTPUT_LIB)



all: $(OUTPUT)

lint:
	splint *.c

.PHONY : clean
clean :
	rm $(OBJECTS) $(OUTPUT) $(OUTPUT_LIB)

check-syntax: 
	gcc -o nul -S ${CHK_SOURCES} 

