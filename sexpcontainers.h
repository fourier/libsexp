/* -*- Mode: C; tab-width: 2; indent-tabs-mode: nil; c-basic-offset: 2 -*- */
#ifndef _SEXPCONTAINERS_H_
#define _SEXPCONTAINERS_H_

#include "sexptoken.h"
#include "sexpitem.h"


/* S-expressions containers */
/* First container used is a list */

/*
 * Item in the list or stack of Sexp tokens
 * List and stack can use the same one-way list structure
 * Differences btw stack and list only in method of adding and
 * removing(pop) elements
 */
#define DECLARE_SEXP_LIST(SEXP_LIST_NAME,SEXP_ELEMENT_TYPE) \
  typedef struct SEXP_LIST_NAME##_tag                       \
  {                                                         \
    SEXP_ELEMENT_TYPE* value;                               \
    struct SEXP_LIST_NAME##_tag* next;                      \
  } SEXP_LIST_NAME

DECLARE_SEXP_LIST(sexp_token_cont_item,sexp_token);
DECLARE_SEXP_LIST(sexp_item_cont_item, sexp_item);

/*
 * General container functions
 */

#define DECLARE_SEXP_LIST_ALLOC(SEXP_LIST_NAME,SEXP_ELEMENT_TYPE)   \
  SEXP_LIST_NAME* SEXP_LIST_NAME##_alloc(SEXP_ELEMENT_TYPE* value)

#define DECLARE_SEXP_LIST_FREE(SEXP_LIST_NAME)                \
  SEXP_LIST_NAME* SEXP_LIST_NAME##_free(SEXP_LIST_NAME* item)

/*
 * Alloc/Free for sexp_token_cont_item structure
 */

/* Allocate memory for the next list item */
DECLARE_SEXP_LIST_ALLOC(sexp_token_cont_item,sexp_token);

/* Free allocated memory for the list item and its data  */
DECLARE_SEXP_LIST_FREE(sexp_token_cont_item);

/*
 * Alloc/Free for sexp_token_cont_item structure
 */

/* Allocate memory for the next list item */
DECLARE_SEXP_LIST_ALLOC(sexp_item_cont_item, sexp_item);

/* Free allocated memory for the list item and its data  */
DECLARE_SEXP_LIST_FREE(sexp_item_cont_item);


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
sexp_item_cont_item* sexp_item_stack_push(sexp_item_cont_item* top,
                                          sexp_item* item);

/*
 * Remove top element of the stack
 * Returns the new top
 */
sexp_item_cont_item* sexp_item_stack_pop(sexp_item_cont_item* top,
                                         sexp_item** element);



#endif /* _SEXPCONTAINERS_H_ */
