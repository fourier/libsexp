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

#ifndef _SEXPCONTAINERS_H_
#define _SEXPCONTAINERS_H_

#include "atomtoken.h"
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

DECLARE_SEXP_LIST(sexp_item_cont_item, sexp_item);

/*
 * General container functions
 */

#define DECLARE_SEXP_LIST_ALLOC(SEXP_LIST_NAME,SEXP_ELEMENT_TYPE)   \
  SEXP_LIST_NAME* SEXP_LIST_NAME##_alloc(SEXP_ELEMENT_TYPE* value)

#define DECLARE_SEXP_LIST_FREE(SEXP_LIST_NAME)                \
  SEXP_LIST_NAME* SEXP_LIST_NAME##_free(SEXP_LIST_NAME* item)

/*
 * Alloc/Free for sexp_item_cont_item structure
 */

/* Allocate memory for the next list item */
DECLARE_SEXP_LIST_ALLOC(sexp_item_cont_item, sexp_item);

/* Free allocated memory for the list item and its data  */
DECLARE_SEXP_LIST_FREE(sexp_item_cont_item);


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
