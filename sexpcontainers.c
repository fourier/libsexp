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

#include <stdlib.h>
#include <string.h>

#include "sexpcontainers.h"

#define DEFINE_SEXP_LIST_ALLOC(SEXP_LIST_NAME,SEXP_ELEMENT_TYPE)        \
  DECLARE_SEXP_LIST_ALLOC(SEXP_LIST_NAME,SEXP_ELEMENT_TYPE)             \
  {                                                                     \
    SEXP_LIST_NAME* item = calloc(1, sizeof(SEXP_LIST_NAME));           \
    if (item)                                                           \
    {                                                                   \
      item->next  = 0;                                                  \
      item->value = value;                                              \
    }                                                                   \
    return item;                                                        \
  }

#define DEFINE_SEXP_LIST_FREE(SEXP_LIST_NAME,SEXP_ELEMENT_TYPE)    \
  DECLARE_SEXP_LIST_FREE(SEXP_LIST_NAME)                           \
  {                                                                \
    if (item)                                                      \
    {                                                              \
      item->value = SEXP_ELEMENT_TYPE##_free(item->value);         \
      free (item);                                                 \
    }                                                              \
    return (SEXP_LIST_NAME*)0;                                     \
  }


DEFINE_SEXP_LIST_ALLOC(sexp_token_cont_item,sexp_token)
DEFINE_SEXP_LIST_FREE(sexp_token_cont_item,sexp_token)

DEFINE_SEXP_LIST_ALLOC(sexp_item_cont_item,sexp_item)
DEFINE_SEXP_LIST_FREE(sexp_item_cont_item,sexp_item)

/* TODO: parametrize functions for working with list and stack! */

static void sexp_token_cont_add_item(sexp_token_cont_item* head,
                                     sexp_token_cont_item* item)
{
  if (head && item)
  {
    while (head->next)
      head = head->next;
    head->next = item;
  }
}

sexp_token_cont_item* sexp_token_list_add(sexp_token_cont_item* head,
                                          sexp_token* token)
{
  sexp_token_cont_item* item = sexp_token_cont_item_alloc(token);
  sexp_token_cont_add_item(head,item);
  return item;
}

sexp_token_cont_item* sexp_token_cont_list_free(sexp_token_cont_item* head)
{
  sexp_token_cont_item* prev;
  if (head)
  {
    while (head->next)
    {
      prev = head;
      head = head->next;
      prev = sexp_token_cont_item_free(prev);
    }
    head = sexp_token_cont_item_free(head);
  }
  return (sexp_token_cont_item*)0;
}


sexp_item_cont_item* sexp_item_stack_push(sexp_item_cont_item* top,
                                          sexp_item* item)
{
  sexp_item_cont_item* result =  sexp_item_cont_item_alloc(item);
  result->next = top;
  return result;
}

sexp_item_cont_item* sexp_item_stack_pop(sexp_item_cont_item* top,
                                         sexp_item** element)
{
  sexp_item_cont_item* result = top->next;
  *element = top->value;
  /* TODO: fix this deallocation! */
  top->value = 0;
  sexp_item_cont_item_free(top);
  return result;
}
