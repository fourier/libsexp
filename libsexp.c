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
#include <ctype.h>
#include <stdio.h>
#include <math.h>
#include <limits.h>

#include "libsexp.h"
#include "sexpitem.h"
#include "sexptoken.h"

int sexp_item_is_atom(sexp_item* item)
{
  return item && item->atom;
}


int sexp_item_is_cons(sexp_item* item)
{
  return item && item->car;
}


int sexp_item_is_integer(sexp_item* item)
{
  return (item &&
          sexp_item_is_atom(item) &&
          item->atom &&
          item->atom->type == EIntegerNumber);
}


int sexp_item_is_float(sexp_item* item)
{
  return (item &&
          sexp_item_is_atom(item) &&
          item->atom &&
          item->atom->type == EFloatNumber);
}


int sexp_item_is_string(sexp_item* item)
{
  return (item &&
          sexp_item_is_atom(item) &&
          item->atom &&
          item->atom->type == EString);
}


int sexp_item_is_symbol(sexp_item* item)
{
    return (item &&
          sexp_item_is_atom(item) &&
          item->atom &&
          item->atom->type == ESymbol);
}


void sexp_item_print(sexp_item* item)
{
  if (item && item->atom)
    atom_token_print(item->atom);
}


int sexp_item_inumber(sexp_item* item)
{
  int result = INT_MAX;
  if (item &&
      sexp_item_is_atom(item) &&
      item->atom &&
      item->atom->type == EIntegerNumber)
    result = item->atom->value.int_number;
  return result;
}

double sexp_item_fnumber(sexp_item* item)
{
  double result = NAN;
  atom_token* token = 0;
  if (item &&
      sexp_item_is_atom(item)
      && (token = item->atom))
  {
    if (token->type == EFloatNumber)
      result = token->value.float_number;
    else if(token->type == EIntegerNumber)
      result = (double)token->value.int_number;
  }
  return result;
}

const char* sexp_item_string(sexp_item* item)
{
  const char* result = 0;
  if (item &&
      sexp_item_is_atom(item) &&
      item->atom &&
      item->atom->type == EString)
    result = item->atom->value.string;
  return result;
}

const char* sexp_item_symbol(sexp_item* item)
{
  const char* result = 0;
  if (item &&
      sexp_item_is_atom(item) &&
      item->atom &&
      item->atom->type == ESymbol)
    result = item->atom->value.symbol;
  return result;  
}


sexp_item* sexp_item_attribute(sexp_item* item, const char* attribute)
{
  sexp_item* result = (sexp_item*)0;
  sexp_item* next = item;
  sexp_item* car;
  char* attribute_name, *p;
  if (item && attribute)
  {
    /* copy attribute to the attribute_name upper-case prepending ':' */
    attribute_name = calloc(strlen(attribute)+2,1);
    p = attribute_name;
    *p++ = ':';
    while(*attribute)
      *p++ = toupper(*attribute++);

    while(next && !sexp_item_is_nil(next))
    {
      car = sexp_item_car(next);
      if (car->atom && car->atom->type == ESymbol &&
          !strcmp(car->atom->value.symbol,attribute_name))
      {
        result = sexp_item_car(sexp_item_cdr(next));
        break;
      }
      next = sexp_item_cdr(next);
    }
    
    free(attribute_name);
  }
  return result;
}

int sexp_item_starts_with_symbol(sexp_item* item, const char* symbol)
{
  /* TODO: test this function! */
  int result = 0;
  sexp_item* car;
  if (item)
  {
    car = sexp_item_car(item);
    result = sexp_item_is_symbol_like(car,symbol);
  }
  return result;
}

int sexp_item_is_symbol_like(sexp_item* item, const char* symbol)
{
  int result = 0;
  char* p;
  if (item && item->atom && item->atom->type == ESymbol)
  {
    result = 1;
    if (symbol)
    {
      p = item->atom->value.symbol;
      while (*p || *symbol)
        if (toupper(*symbol++) != *p++)
          return 0;
      result = (*symbol == *p) && (*symbol == 0);
    }
  }
  return result;
}
