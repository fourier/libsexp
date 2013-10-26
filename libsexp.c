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
#include "atomtoken.h"

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
          atom_token_is_integer(item->atom));
}


int sexp_item_is_float(sexp_item* item)
{
  return (item &&
          sexp_item_is_atom(item) &&
          item->atom &&
          atom_token_is_float(item->atom));
}


int sexp_item_is_string(sexp_item* item)
{
  return (item &&
          sexp_item_is_atom(item) &&
          item->atom &&
          atom_token_is_string(item->atom));
}


int sexp_item_is_symbol(sexp_item* item)
{
    return (item &&
          sexp_item_is_atom(item) &&
          item->atom &&
          atom_token_is_symbol(item->atom));
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
      atom_token_is_integer(item->atom))
    result = atom_token_integer(item->atom);
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
    if (atom_token_is_float(token))
      result = atom_token_float(token);
    else if(atom_token_is_integer(token))
      result = (double)atom_token_integer(token);
  }
  return result;
}

const char* sexp_item_string(sexp_item* item)
{
  const char* result = 0;
  if (item &&
      sexp_item_is_atom(item) &&
      item->atom &&
      atom_token_is_string(item->atom))
    result = atom_token_string(item->atom);
  return result;
}

const char* sexp_item_symbol(sexp_item* item)
{
  const char* result = 0;
  if (item &&
      sexp_item_is_atom(item) &&
      item->atom &&
      atom_token_is_symbol(item->atom))
    result = atom_token_symbol(item->atom);
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
      if (car->atom && atom_token_is_symbol(car->atom) &&
          !strcmp(atom_token_symbol(car->atom),attribute_name))
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
  if (item && item->atom && atom_token_is_symbol(item->atom))
  {
    result = 1;
    if (symbol)
    {
      p = atom_token_symbol(item->atom);
      while (*p || *symbol)
        if (toupper(*symbol++) != *p++)
          return 0;
      result = (*symbol == *p) && (*symbol == 0);
    }
  }
  return result;
}
