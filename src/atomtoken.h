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

#ifndef _SEXPTOKENS_H_
#define _SEXPTOKENS_H_

#include "libsexp.h"

/*
 * Type declarations
 */

#ifndef ATOM_TOKEN_STRUCT
#define ATOM_TOKEN_STRUCT
typedef struct atom_token atom_token;
#endif /* ATOM_TOKEN_STRUCT */

/*
 * Function declarations
 */

/* Constructors: allocate memory and create Atom of specified type */
atom_token* atom_token_integer_alloc(const char* begin, const char* end);
atom_token* atom_token_float_alloc(const char* begin, const char* end);
atom_token* atom_token_string_alloc(const char* begin, const char* end);
atom_token* atom_token_symbol_alloc(const char* begin, const char* end);
atom_token* atom_token_nil_alloc();

/* Free allocated memory for Atom token */
atom_token* atom_token_free(atom_token* token);

/* getters of the atom_token properties */
int atom_token_is_integer(atom_token* token);
int atom_token_is_float(atom_token* token);
int atom_token_is_string(atom_token* token);
int atom_token_is_symbol(atom_token* token);
int atom_token_is_nil(atom_token* token);

/* getters of the atom_token value */
int atom_token_integer(atom_token* token);
double atom_token_float(atom_token* token);
const char* atom_token_string(atom_token* token);
const char* atom_token_symbol(atom_token* token);

/* size of token in dynamic memory */
unsigned int atom_token_size(atom_token* token);


/* Pretty-print token value */
void atom_token_print(atom_token* token);

#endif /* _SEXPTOKENS_H_ */
