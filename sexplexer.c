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

#include "sexplexer.h"
#include "sexputils.h"


sexp_token* read_sexp_token(const char** str)
{
  sexp_token* token = (sexp_token*)0;
  atom_token* atom  = (atom_token*)0;
  const char* ptr = *str;
  const char* p = ptr;
  if (ptr)
  {
    do
    {
      ptr = p;
      p = skip_whitespaces(p);
      p = skip_comment(p);
    } while(ptr != p);
    
    switch (*ptr)
    {
    case '(':
      token = sexp_token_alloc(EOPENPAREN,0);
      ptr++;
      break;
    case ')':
      token = sexp_token_alloc(ECLOSEPAREN,0);
      ptr++;
      break;
    default:
      atom = read_atom_token(&ptr);
      if ( atom )               /* parsed successfully */
        token = sexp_token_alloc(EATOM,atom);
      break;
    }
  }
  *str = ptr;
  return token;
}

static atom_token* read_atom_token_integer(const char** str)
{
  atom_token* token = (atom_token*)0;
  const char* ptr = *str;
  if (ptr)
  {
    ptr = find_end_of_integer_number(ptr);
    if ( ptr != *str)           /* integer */
      token = atom_token_integer_alloc(*str,ptr);
    *str = ptr;
  }
  return token;
}

static atom_token* read_atom_token_float(const char** str)
{
  atom_token* token = (atom_token*)0;
  const char* ptr = *str;
  if (ptr)
  {
    ptr = find_end_of_floating_point_number(ptr);
    if ( ptr != *str)           /* float */
      token = atom_token_float_alloc(*str,ptr);
    *str = ptr;
  }
  return token;
}

static atom_token* read_atom_token_string(const char** str)
{
  atom_token* token = (atom_token*)0;
  const char* ptr = *str;
  if (ptr)
  {
    ptr = find_end_of_quoted_string(ptr);
    if ( ptr != *str)           /* string */
      token = atom_token_string_alloc(*str,ptr);
    *str = ptr;
  }
  return token;
}

/* return nonzero if string is NIL */
static int check_if_nil(const char* begin, const char* end)
{
  int result = 0;
  if ( end - begin == 3)
    result = (begin[0] == 'N' || begin[0] == 'n')
      && (begin[1] == 'I' || begin[1] == 'i')
      && (begin[2] == 'L' || begin[2] == 'l');
  return result;
}

static atom_token* read_atom_token_symbol(const char** str)
{
  atom_token* token = (atom_token*)0;
  const char* ptr = *str;
  if (ptr)
  {
    ptr = find_end_of_symbol(ptr);
    if ( ptr != *str)           /* symbol */
      token = check_if_nil(*str,ptr) ? atom_token_nil_alloc() : 
        atom_token_symbol_alloc(*str,ptr);
    *str = ptr;
  }
  return token;
}


atom_token* read_atom_token(const char** str)
{
  atom_token* token = (atom_token*)0;
  const char* ptr = *str;
  if (ptr)
  {
    /*  /\* determine atom type, if possible *\/ */
    /* /\* 1) check if it is an integer  *\/ */
    /* token = read_atom_token_integer(&ptr); */
    /* if (!token) /\* 2) check if float *\/ */
    /*   token = read_atom_token_float(&ptr); */
    /* if (!token) /\* 3) check if string *\/ */
    /*   token = read_atom_token_string(&ptr); */
    /* if (!token) /\* 4) check if symbol or special(NIL) *\/ */
    /*   token = read_atom_token_symbol(&ptr); */

    /* determine atom type, if possible */
    /* 1) check if it is an integer  */
    token = read_atom_token_symbol(&ptr);
    if (!token) 
      token = read_atom_token_string(&ptr);
    if (!token) 
      token = read_atom_token_float(&ptr);
    if (!token)
      token = read_atom_token_integer(&ptr);
    *str = ptr;
  }
  return token;
}

