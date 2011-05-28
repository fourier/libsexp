/* -*- Mode: C; tab-width: 2; indent-tabs-mode: nil; c-basic-offset: 2 -*- */
#ifndef _SEXPCONTAINERS_H_
#define _SEXPCONTAINERS_H_

#include "sexptokens.h"

/* S-expressions containers */
/* First container used is a list */

/*
 * Item in the list or stack of Sexp tokens
 * List and stack can use the same one-way list structure
 * Differences btw stack and list only in method of adding and
 * removing(pop) elements
 */
typedef struct sexp_token_cont_item_tag
{
  sexp_token* token;
  struct sexp_token_cont_item_tag* next;
} sexp_token_cont_item;

/*
 * General container functions
 */

/* Allocate memory for the next list item */
sexp_token_cont_item* sexp_token_cont_alloc(sexp_token* token);

/* Free allocated memory for the list item and its data  */
sexp_token_cont_item* sexp_token_cont_free(sexp_token_cont_item* item);

/*
 * List container functions
 */

/* Free allocated memory for the whole list */
sexp_token_cont_item* sexp_token_cont_list_free(sexp_token_cont_item* head);

/*
 * Append token to the end of the token list and return the
 * list item
 */
sexp_token_cont_item* sexp_token_list_add(sexp_token_cont_item* head,
                                          sexp_token* token);

/*
 * Stack container functions
 */

/*
 * Add element to the top of the stack.
 * Returns the new top
 */
sexp_token_cont_item* sexp_token_stack_push(sexp_token_cont_item* top,
                                            sexp_token* token);

/*
 * Remove top element of the stack
 * Returns the new top
 */
sexp_token_cont_item* sexp_token_stack_pop(sexp_token_cont_item* top);
                                 


#endif /* _SEXPCONTAINERS_H_ */
