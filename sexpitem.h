/* -*- Mode: C; tab-width: 2; indent-tabs-mode: nil; c-basic-offset: 2 -*- */
#ifndef _SEXPITEM_H_
#define _SEXPITEM_H_

#include "sexptoken.h"

/* struct representing SEXP item: ATOM or CONS */
typedef struct sexp_item_tag
{
  atom_token* atom;             /* if not empty - ATOM */
  /* otherwise CONS */
  struct sexp_item_tag* car;
  struct sexp_item_tag* cdr;
} sexp_item;


/*
 * Funcitons operating with sexp_items
 */

/* create sexp_item from token, taking ownership of the atom */
sexp_item* sexp_item_create_atom(sexp_token* from);
sexp_item* sexp_item_create_cons(sexp_item* car, sexp_item* cdr);
sexp_item* sexp_item_free(sexp_item* item);
sexp_item* sexp_item_car(sexp_item* item);
sexp_item* sexp_item_cdr(sexp_item* item);
/* return non-zero if item is atom of type nil */
int sexp_item_is_nil(sexp_item* item);
/* calculates the length of the list item. -1 if item is not of type list */
int sexp_item_length(sexp_item* item);
/* return i-th element of the list item, 0 if not found */
sexp_item* sexp_item_nth(sexp_item* item, int i);

/* verbose print sexp_item as a CONS chain */
void sexp_item_print(sexp_item* item);



#endif /* _SEXPITEM_H_ */
