# -*- Mode: makefile; -*-
# Makefile for the SexpLib project

CC = gcc

CFLAGS = -ggdb -pg -ansi -pedantic -Wall -Wextra -Wmissing-include-dirs -Wswitch-default -Wswitch-enum -Wdeclaration-after-statement -Wmissing-declarations 
INCLUDES = -I .
LINKFLAGS = 

SOURCES := $(wildcard *.c)
HEADERS := $(wildcard *.h)
OBJECTS := $(patsubst %.c,%.o,$(wildcard *.c))
OUTPUT = sexptest

%.o : %.c %.h 
	$(CC) -c $(CFLAGS) $(DEFINES) $(INCLUDES) $< -o $@

$(OUTPUT): $(OBJECTS)
	$(CC) -o $(OUTPUT) $(OBJECTS) $(LINKFLAGS)

all: $(OUTPUT)

lint:
	splint *.c

.PHONY : clean
clean :
	rm $(OBJECTS) $(OUTPUT)

check-syntax: 
	gcc -o nul -S ${CHK_SOURCES} 
