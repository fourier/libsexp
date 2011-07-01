/* -*- Mode: C; tab-width: 2; indent-tabs-mode: nil; c-basic-offset: 2 -*- */
#ifndef _SEXPITEM_H_
#define _SEXPITEM_H_

#include "sexptoken.h"
#include "libsexp.h"

/* create sexp_item from token, taking ownership of the atom */
sexp_item* sexp_item_create_atom(sexp_token* from);
sexp_item* sexp_item_create_cons(sexp_item* car, sexp_item* cdr);

#endif /* _SEXPITEM_H_ */
