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

#include <stdio.h>
#include <stdlib.h>
#include <assert.h>

#include "sexpitem.h"
#include "sexpcontainers.h"


sexp_item* sexp_item_create_atom(atom_token* atom)
{
  sexp_item* result = calloc(sizeof(sexp_item),1);
  result->atom = atom;
  return result;
}

sexp_item* sexp_item_create_cons(sexp_item* car, sexp_item* cdr)
{
  sexp_item* result = calloc(sizeof(sexp_item),1);
  result->car = car;
  result->cdr = cdr;
  return result;
}

static sexp_item* sexp_item_rotate_right(sexp_item* Q)
{
  /*
   * Let P be Q's left child.
   * Set P to be the new root.
   * Set Q's left child to be P's right child.
   * Set P's right child to be Q.
   */
  sexp_item* P = sexp_item_car(Q);
  sexp_item* T = sexp_item_cdr(P);
  Q->car = T;
  P->cdr = Q;
  return P;
}

sexp_item* sexp_item_free(sexp_item* item)
{
  /* iteration version othe free function */
  sexp_item* root = item;
  sexp_item* r;
  if (root)
  {
    while(root)
    {
      if (root->atom)
      {
        atom_token_free(root->atom);
        root->atom = 0;
      }
      /*
       * if left branch exist perform right rotation 
       * NOTE: while rotating items will contain atoms
       * as well as CARs and CDRs
       */
      if (sexp_item_car(root))
        root = sexp_item_rotate_right(root);
      else /* otherwise delete root, root = right(root) */
      {
        r = root->cdr;
        free(root);
        root = r;
      }
    }
  }

  return (sexp_item*)0;
}



sexp_item* sexp_item_car(sexp_item* item)
{
  sexp_item* result = 0;
  if (item)
  {
    /* assert(!item->atom); */
    result = item->car;
  }
  return result;
}

sexp_item* sexp_item_cdr(sexp_item* item)
{
  sexp_item* result = 0;
  if ( item)
  {
    /* assert(!item->atom); */
    result = item->cdr;
  }
  return result;
}

void sexp_item_traverse(sexp_item* item,appy_to_item_t function, void* data)
{
  sexp_item_cont_item* stack;
  if (item)
  {
    /* initialize stack */
    stack = sexp_item_cont_item_alloc(item);
    while (stack)
    {
      stack = sexp_item_stack_pop(stack,&item);
      if ( !item->atom )
      {
        if (sexp_item_cdr(item))
          stack = sexp_item_stack_push(stack,sexp_item_cdr(item));
        if ( sexp_item_car(item))
          stack = sexp_item_stack_push(stack, sexp_item_car(item));
      }
      function(item,data);
    }
    stack = sexp_item_cont_item_free(stack);
  }
}

int sexp_item_is_nil(sexp_item* item)
{
  int result = 0;
  assert(item);
  result = item->atom && atom_token_is_nil(item->atom);
  return result;
}

int sexp_item_length(sexp_item* item)
{
  int result = -1;
  sexp_item* next = item;
  assert(item);
  if ( sexp_item_is_nil(item))
    result = 0;
  else if (!item->atom)
  {
    result = 1;
    while ( !sexp_item_is_nil(next = next->cdr))
      result ++;
  }
  return result;
}


sexp_item* sexp_item_nth(sexp_item* item, int i)
{
  sexp_item* result = sexp_item_car(item);
  assert(item);
  assert(i >= 0);
  for (; i > 0 && !sexp_item_is_nil(result) ; -- i)
    result = sexp_item_car(item = item->cdr);
  
  return result;
}

