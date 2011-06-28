/* -*- Mode: C; tab-width: 2; indent-tabs-mode: nil; c-basic-offset: 2 -*- */
#ifndef _SEXPLEXER_H_
#define _SEXPLEXER_H_

#include "sexptoken.h"

/* Read input stream for the Sexp token */
sexp_token* read_sexp_token(char** str);

/* Read input stream for the Atom token */
atom_token* read_atom_token(char** str);


#endif /* _SEXPLEXER_H_ */
