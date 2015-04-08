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

#ifndef _SEXPITEM_H_
#define _SEXPITEM_H_

#include "atomtoken.h"

/* struct representing SEXP item: ATOM or CONS */
#ifndef SEXP_ITEM_FWD_DECL
#define SEXP_ITEM_FWD_DECL
typedef struct sexp_item sexp_item;
#endif


/* create sexp_item from token, taking ownership of the atom */
sexp_item* sexp_item_create_atom(atom_token* atom);
sexp_item* sexp_item_create_cons(sexp_item* car, sexp_item* cdr);

/* get the atom of the sexp item if sexp is atom */
atom_token* sexp_item_atom(sexp_item* item);


#endif /* _SEXPITEM_H_ */
