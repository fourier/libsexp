/* -*- Mode: C; tab-width: 2; indent-tabs-mode: nil; c-basic-offset: 2 -*- */
#include "sexplexer.h"
#include "sexputils.h"


sexp_token* read_sexp_token(char** str)
{
  sexp_token* token = (sexp_token*)0;
  atom_token* atom  = (atom_token*)0;
  char* ptr = *str;
  if (ptr)
  {
    ptr = skip_whitespaces(ptr);
    ptr = skip_comment(ptr);
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

static atom_token* read_atom_token_integer(char** str)
{
  atom_token* token = (atom_token*)0;
  char* ptr = *str;
  if (ptr)
  {
    ptr = find_end_of_integer_number(ptr);
    if ( ptr != *str)           /* integer */
      token = atom_token_integer_alloc(*str,ptr);
    *str = ptr;
  }
  return token;
}

static atom_token* read_atom_token_float(char** str)
{
  atom_token* token = (atom_token*)0;
  char* ptr = *str;
  if (ptr)
  {
    ptr = find_end_of_floating_point_number(ptr);
    if ( ptr != *str)           /* float */
      token = atom_token_float_alloc(*str,ptr);
    *str = ptr;
  }
  return token;
}

static atom_token* read_atom_token_string(char** str)
{
  atom_token* token = (atom_token*)0;
  char* ptr = *str;
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
static int check_if_nil(char* begin, char* end)
{
  int result = 0;
  if ( end - begin == 3)
    result = (begin[0] == 'N' || begin[0] == 'n')
      && (begin[1] == 'I' || begin[1] == 'i')
      && (begin[2] == 'L' || begin[2] == 'l');
  return result;
}

static atom_token* read_atom_token_symbol(char** str)
{
  atom_token* token = (atom_token*)0;
  char* ptr = *str;
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


atom_token* read_atom_token(char** str)
{
  atom_token* token = (atom_token*)0;
  char* ptr = *str;
  if (ptr)
  {
     /* determine atom type, if possible */
    /* 1) check if it is an integer  */
    token = read_atom_token_integer(&ptr);
    if (!token) /* 2) check if float */
      token = read_atom_token_float(&ptr);
    if (!token) /* 3) check if string */
      token = read_atom_token_string(&ptr);
    if (!token) /* 4) check if symbol or special(NIL) */
      token = read_atom_token_symbol(&ptr);
    *str = ptr;
  }
  return token;
}

