# -*- Mode: makefile; -*-

# Copyright (C) 2011 Alexey Veretennikov (alexey dot veretennikov at gmail.com)
# 
#	This file is part of Libsexp.
#
# Libsexp is free software: you can redistribute it and/or modify
# it under the terms of the GNU Lesser General Public License as published
# by the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# Libsexp is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU Lesser General Public License for more details.
#
# You should have received a copy of the GNU Lesser General Public License
# along with Libsexp.  If not, see <http://www.gnu.org/licenses/>.


# to create sexpgrammar.[ch] use the following command:
# sbcl --load sexpgrammar.lisp sexpgrammar
CC = gcc
LEX = flex

CFLAGS = -ggdb -pg --std=c99 -pedantic -Wall -Wextra -Wmissing-include-dirs -Wswitch-default -Wswitch-enum -Wdeclaration-after-statement -Wmissing-declarations 
INCLUDES = -I .
LINKFLAGS = -L. -lsexp -lfl

OUTPUT_SRC = main.c
SOURCES := $(wildcard *.c)
HEADERS := $(wildcard *.h)
LEXES   := $(wildcard *.lex)
OBJECTS := $(patsubst %.c,%.o,$(SOURCES)) libsexp.yy.o
OBJECTS_LIB := $(filter-out $(patsubst %.c,%.o,$(OUTPUT_SRC)),$(OBJECTS))
OUTPUT = sexptest
OUTPUT_LIB = libsexp.a

.PHONY: all clean

all: $(OUTPUT)

%.o : %.c %.h
	$(CC) -c $(CFLAGS) $(DEFINES) $(INCLUDES) $< -o $@

libsexp.yy.o: libsexp.yy.c
	$(CC) -c -o $@ $<

libsexp.yy.c: sexp.l
	$(LEX) -o $@ $<


$(OUTPUT): $(OUTPUT_LIB) 
	$(CC) $(patsubst %.c,%.o,$(OUTPUT_SRC)) -o $(OUTPUT) $(LINKFLAGS)

$(OUTPUT_LIB): $(OBJECTS)
	$(RM) -f $(OUTPUT_LIB)
	$(AR) cr $(OUTPUT_LIB) $(OBJECTS_LIB)
	ranlib $(OUTPUT_LIB)

lint:
	splint *.c

clean :
	rm $(OBJECTS) $(OUTPUT) $(OUTPUT_LIB) libsexp.yy.c 

check-syntax: 
	gcc -o nul -S ${CHK_SOURCES} 

