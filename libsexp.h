/* -*- Mode: C; tab-width: 2; indent-tabs-mode: nil; c-basic-offset: 2 -*- */
/*
 Copyright (C) 2011 Alexey Veretennikov (alexey dot veretennikov at gmail.com)
 
 This file is part of Libsexp.

 Libsexp is free software: you can redistribute it and/or modify
 it under the terms of the GNU Lesser General Public License as published
 by the Free Software Foundation, either version 3 of the License, or
 (at your option) any later version.

 Libsexp is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 GNU Lesser General Public License for more details.

 You should have received a copy of the GNU Lesser General Public License
 along with Libsexp.  If not, see <http://www.gnu.org/licenses/>.
*/
#ifndef _LIBSEXP_H_
#define _LIBSEXP_H_

#include <stdio.h>

/*
 * Type declarations
 */

/* forward declaration */
typedef struct sexp_item sexp_item;

/* visitor function used in traverse through Sexp */
typedef void (*appy_to_item_t) (sexp_item* item, void* data);


/*
 * Functions operating with sexp_items
 * Implemented in sexpitem.c and libsexp.c
 */

/* return non-zero if item is cons cell */
int sexp_item_is_cons(sexp_item* item);

/* return non-zero if item is atom */
int sexp_item_is_atom(sexp_item* item);

/* return non-zero if item is atom of type nil */
int sexp_item_is_nil(sexp_item* item);

/* return non-zero if item is atom of type integer */
int sexp_item_is_integer(sexp_item* item);

/* return non-zero if item is atom of type float */
int sexp_item_is_float(sexp_item* item);

/* return non-zero if item is atom of type string */
int sexp_item_is_string(sexp_item* item);

/* return non-zero if item is atom of type symbol */
int sexp_item_is_symbol(sexp_item* item);


/* Print the information about Atom sexp in simple format */
void sexp_item_print(sexp_item* item);

/* get the number from the atom as a integer value.
 * Returs INT_MAX if incorrect token type
 */
int sexp_item_inumber(sexp_item* item);

/* get the number from the atom as a floating-point value.
 * Returns NAN if incorret token type
 */
double sexp_item_fnumber(sexp_item* item);

/* get the string value if item is an atom of type string.
 * Returns 0 if incorrect token type
 */
const char* sexp_item_string(sexp_item* item);

/* get the string value if item is an atom of type string.
 * Returns 0 if incorrect token type
 */
const char* sexp_item_symbol(sexp_item* item);



sexp_item* sexp_item_free(sexp_item* item);
sexp_item* sexp_item_car(sexp_item* item);
sexp_item* sexp_item_cdr(sexp_item* item);

/* calculates the length of the list item. -1 if item is not of type list */
int sexp_item_length(sexp_item* item);
/* return i-th element of the list item, 0 if not found */
sexp_item* sexp_item_nth(sexp_item* item, int i);

/*
 * returns a pointer to the attribute value specified by attribute name.
 * Example: given list item = (function 1 2 :test 3.14)
 * Call: sexp_item_attribute(item,"test") will return pointer to
 * the item containing 3.14
 * If no attribute or its value found returns nil
 */
sexp_item* sexp_item_attribute(sexp_item* item, const char* attribute);

/*
 * Check if item is symbol. If symbol is not 0, also test if the item
 * is symbol with value from the 2nd argument(regardless of the character
 * case)
 */
int sexp_item_is_symbol_like(sexp_item* item, const char* symbol);

/*
 * Check if item is a list starting with symbol. If symbol != 0,
 * starts if the list starts with this symbol
 */
int sexp_item_starts_with_symbol(sexp_item* item, const char* symbol);

/* sexp item tree traversal */
void sexp_item_traverse(sexp_item* item,appy_to_item_t function, void* data);

/*
 * calculate the size of the item in dynamic memory, including
 * sizes of all elements if item is a list
 */
unsigned int sexp_item_size(sexp_item* item);

/*
 * Main parse function - parses from file.
 * Returns the parsed root item when parsing is complete
 * or zero pointer if parsing failed
 */
sexp_item* sexp_parse_file(FILE* input);

/*
 * Auxulary parse function - parses from null-terminated string.
 * Returns the parsed root item when parsing is complete
 * or zero pointer if parsing failed
 */
sexp_item* sexp_parse_str(const char* read_buffer);

#endif /* _LIBSEXP_H_ */
