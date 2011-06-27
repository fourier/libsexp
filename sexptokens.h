/* -*- Mode: C; tab-width: 2; indent-tabs-mode: nil; c-basic-offset: 2 -*- */
#ifndef _SEXPTOKENS_H_
#define _SEXPTOKENS_H_

#include "sexpgrammar.h"

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
  ESymbol
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

/* Structure holding top-level s-expression tokens*/
typedef struct
{
  TerminalType type;
  atom_token* atom;
} sexp_token;

/*
 * Set of structures represening concrete SEXP elements:
 * they can be of 2 types, ATOMs or CONSes
 */

/* types of the SEXP: ATOM or CONS */
typedef enum
{
  EAtom = EATOM,
  ECons
} SexpValueType;

/* struct representing SEXP item: ATOM or CONS */
typedef struct sexp_item_tag
{
  SexpValueType type;
  union
  {
    atom_token* atom;
    struct sexp_cons_tag* cons;
  } value;
} sexp_item;

/* struct representing CONS */
typedef struct sexp_cons_tag
{
  sexp_item* car;
  struct sexp_cons_tag* cdr;
} sexp_cons;


/*
 * Function declarations
 */

/*
 * Funcitons operating with atom_token structures
 */

/* Allocate memory for Atom token and empty necessary fields */
atom_token* atom_token_alloc(AtomTokenType type);

/* Constructors: allocate memory and create Atom of specified type */
atom_token* atom_token_integer_alloc(char* begin, char* end);
atom_token* atom_token_float_alloc(char* begin, char* end);
atom_token* atom_token_string_alloc(char* begin, char* end);
atom_token* atom_token_symbol_alloc(char* begin, char* end);

/* Clone the token */
atom_token* atom_token_clone(atom_token* token);

/* Free allocated memory for Atom token */
atom_token* atom_token_free(atom_token* token);

/* Print the information about Atom token in verbose format */
void atom_token_verbose_print(atom_token* token);

/* Print the information about Atom token in simple format */
void atom_token_print(atom_token* token);

/*
 * Funcitons operating with sex_token structures
 */

/* Allocate memory for sexp token end empty necessary fields */
sexp_token* sexp_token_alloc(TerminalType type);

/* Free allocated memort for Sexp token */
sexp_token* sexp_token_free(sexp_token* token);

/* Print the information about Sexp token in verbose format */
void sexp_token_verbose_print(sexp_token* token);

/* Print the information about Sexp token in simple format */
void sexp_token_print(sexp_token* token);

/*
 * Funcitons operating with CONSes and sexp_items
 */

sexp_cons* sexp_cons_alloc(sexp_item* car, sexp_cons* cdr);
sexp_cons* sexp_cons_free(sexp_cons* cons);

sexp_item* sexp_item_create_atom(atom_token* original);
sexp_item* sexp_item_create_cons(sexp_item* car, sexp_cons* cdr);
sexp_item* sexp_item_create_cons_plain(sexp_cons* cons);
sexp_item* sexp_item_free(sexp_item* item);

sexp_item* sexp_item_car(sexp_item* item);
sexp_cons* sexp_item_cdr(sexp_item* item);

/* verbose print sexp_item as a CONS chain */
void sexp_item_print(sexp_item* item);


#endif /* _SEXPTOKENS_H_ */
