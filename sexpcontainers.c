/* -*- Mode: C; tab-width: 2; indent-tabs-mode: nil; c-basic-offset: 2 -*- */
#include <stdlib.h>
#include <string.h>

#include "sexpcontainers.h"


sexp_token_cont_item* sexp_token_cont_alloc(sexp_token* token)
{
  sexp_token_cont_item* item = calloc(1, sizeof(sexp_token_cont_item));
  if (item)
  {
    item->next  = 0;
    item->token = token;
  }
  return item;
}

sexp_token_cont_item* sexp_token_cont_free(sexp_token_cont_item* item)
{
  if (item)
  {
    item->token = sexp_token_free(item->token);
    free (item);
  }
  return (sexp_token_cont_item*)0;
}



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
  sexp_token_cont_item* item = sexp_token_cont_alloc(token);
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
      prev = sexp_token_cont_free(prev);
    }
    head = sexp_token_cont_free(head);
  }
  return (sexp_token_cont_item*)0;
}


sexp_token_cont_item* sexp_token_stack_push(sexp_token_cont_item* top,
                                            sexp_token* token)
{
  sexp_token_cont_item* item =  sexp_token_cont_alloc(token);
  item->next = top;
  return item;
}

sexp_token_cont_item* sexp_token_stack_pop(sexp_token_cont_item* top)
{
  sexp_token_cont_item* result = top->next;
  sexp_token_cont_free(top);  
  return result;
}
