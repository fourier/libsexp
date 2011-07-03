/* -*- Mode: C; tab-width: 2; indent-tabs-mode: nil; c-basic-offset: 2 -*- */
#ifndef _LIBSEXP_H_
#define _LIBSEXP_H_

/*
 * Type declarations
 */

/*
 * Enum specifying atom-level token types as
 * integer, float, string, symbol
 */
typedef enum
{
  EIntegerNumber,
  EFloatNumber,
  EString,
  ESymbol,
  ENil
} AtomTokenType;

/* Structure holding atoms */
typedef struct
{
  AtomTokenType type;
  union Value
  {
    int int_number;
    double float_number;
    char* string;
    char* symbol;
  } value;
} atom_token;

/* struct representing SEXP item: ATOM or CONS */
typedef struct sexp_item_tag
{
  atom_token* atom;             /* if not empty - ATOM */
  /* otherwise CONS */
  struct sexp_item_tag* car;
  struct sexp_item_tag* cdr;
} sexp_item;

typedef void (*appy_to_item_t) (sexp_item* item, void* data);

/*
 * Functions operating on atoms
 * Implemented in sexptoken.c
 */

/* Print the information about Atom token in simple format */
void atom_token_print(atom_token* token);


/*
 * Functions operating with sexp_items
 * Implemented in sexpitem.c
 */

sexp_item* sexp_item_free(sexp_item* item);
sexp_item* sexp_item_car(sexp_item* item);
sexp_item* sexp_item_cdr(sexp_item* item);
/* return non-zero if item is atom of type nil */
int sexp_item_is_nil(sexp_item* item);
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

/* sexp item tree traversal */
void sexp_item_traverse(sexp_item* item,appy_to_item_t function, void* data);

/*
 * Parse function. Returns the parsed root item when parsing is complete
 * Implemented in sexpparse.c
 * 
 */
sexp_item* sexp_parse(const char* text);


#endif /* _LIBSEXP_H_ */
