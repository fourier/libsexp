/* -*- Mode: C; tab-width: 2; indent-tabs-mode: nil; c-basic-offset: 2 -*- */
#ifndef _SEXPTOKENS_H_
#define _SEXPTOKENS_H_

#include "sexpgrammar.h"
#include "libsexp.h"


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
 * Functions operating with atom_token structures
 */

/* Allocate memory for Atom token and empty necessary fields */
atom_token* atom_token_alloc(AtomTokenType type);

/* Constructors: allocate memory and create Atom of specified type */
atom_token* atom_token_integer_alloc(const char* begin, const char* end);
atom_token* atom_token_float_alloc(const char* begin, const char* end);
atom_token* atom_token_string_alloc(const char* begin, const char* end);
atom_token* atom_token_symbol_alloc(const char* begin, const char* end);
atom_token* atom_token_nil_alloc();

/* Free allocated memory for Atom token */
atom_token* atom_token_free(atom_token* token);

/*
 * Functions operating with sex_token structures
 */

/* Allocate memory for sexp token end empty necessary fields */
sexp_token* sexp_token_alloc(TerminalType type, atom_token* atom);

/* Free allocated memort for Sexp token */
sexp_token* sexp_token_free(sexp_token* token);


#endif /* _SEXPTOKENS_H_ */
