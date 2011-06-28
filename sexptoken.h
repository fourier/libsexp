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

/* Structure holding top-level s-expression tokens*/
typedef struct
{
  TerminalType type;
  atom_token* atom;
} sexp_token;


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
atom_token* atom_token_nil_alloc();

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
sexp_token* sexp_token_alloc(TerminalType type, atom_token* atom);

/* Free allocated memort for Sexp token */
sexp_token* sexp_token_free(sexp_token* token);

/* Print the information about Sexp token in verbose format */
void sexp_token_verbose_print(sexp_token* token);

/* Print the information about Sexp token in simple format */
void sexp_token_print(sexp_token* token);


#endif /* _SEXPTOKENS_H_ */
