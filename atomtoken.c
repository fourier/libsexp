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
#include <stdio.h>
#include <string.h>
#include <assert.h>
#include <ctype.h>

#include "atomtoken.h"

/*
 * Enum specifying atom-level token types as
 * integer, float, string, symbol
 */
typedef enum
{
  EIntegerNumber,
  EFloatNumber,
  EString,
  ESymbol,
  ENil
} AtomTokenType;

/* Structure holding atoms */
struct atom_token
{
  AtomTokenType type;
  union Value
  {
    int int_number;
    double float_number;
    char* string;
    char* symbol;
  } value;
};


/* Print the information about Atom token in simple format */
void atom_token_print(atom_token* token)
{
  if (token)
  {
    switch (token->type)
    {
    case EIntegerNumber:
      printf("%d",token->value.int_number);
      break;
    case EFloatNumber:
      printf("%f",token->value.float_number);
      break;
    case EString:
      printf("%s",token->value.string);
      break;
    case ESymbol:
      printf("%s",token->value.symbol);
      break;
    case ENil:
      printf("NIL");
    default:
      break;
    }
  }
}

/* Allocate memory for Atom token and empty necessary fields */
static atom_token* atom_token_alloc(AtomTokenType type)
{
  atom_token* token = calloc(1, sizeof(atom_token));
  if (token)
    token->type = type;
  return token;
}

atom_token* atom_token_free(atom_token* token)
{
  if (token)
  {
    switch (token->type)
    {
    case EString:
      free(token->value.string);
      break;
    case ESymbol:
      free(token->value.symbol);
      break;
    case EIntegerNumber:
    case EFloatNumber:
    case ENil:
    default:
      break;
    }
    free(token);
  }
  return (atom_token*)0;
}

atom_token* atom_token_integer_alloc(const char* begin, const char* end)
{
  atom_token* token = atom_token_alloc(EIntegerNumber);
  if ( begin != end)
  {
    token->value.int_number = atoi(begin);
  }
  return token;
}

atom_token* atom_token_float_alloc(const char* begin, const char* end)
{
  /*
   * TODO: fix possible underflow, etc
   */
  atom_token* token = atom_token_alloc(EFloatNumber);
  int size = end - begin + 1;
  char* buf = (char*)0;
  const char** parsed_end = &end;
  token->value.float_number = strtod(begin,(char**)parsed_end);
  if ( *parsed_end == begin)
  {
    buf = calloc(size,1);
    memcpy(buf,begin,size - 1);
    fprintf(stderr,"number: '%s' incorrectly parsed",buf);
    free(buf);
    atom_token_free(token);
  }
  return token;
}

static void unescaped_copy_string(char* to, const char* from, int size)
{
  while(size--)
  {
    if (*from != '\\')
      *to++ = *from;
    from++;
  }
}

atom_token* atom_token_string_alloc(const char* begin, const char* end)
{
  atom_token* token = atom_token_alloc(EString);
  int size = end - begin + 1;
  token->value.string = calloc(size, 1);
  /* memcpy(token->value.string,begin,size - 1); */
  unescaped_copy_string(token->value.string,begin,size-1);
  return token;
}

atom_token* atom_token_symbol_alloc(const char* begin, const char* end)
{
  atom_token* token = atom_token_alloc(ESymbol);
  char* ptr;
  int size = end - begin + 1;
  token->value.string = calloc(size, 1);
  ptr = token->value.string;
  while(begin != end)
    *ptr++ = toupper(*begin++);
  return token;
}

atom_token* atom_token_nil_alloc()
{
  atom_token* token = atom_token_alloc(ENil);
  return token;
}

int atom_token_is_integer(atom_token* token)
{
  return token->type == EIntegerNumber;
}

int atom_token_is_float(atom_token* token)
{
  return token->type == EFloatNumber;
}

int atom_token_is_string(atom_token* token)
{
  return token->type == EString;
}

int atom_token_is_symbol(atom_token* token)
{
  return token->type == ESymbol;
}

int atom_token_is_nil(atom_token* token)
{
  return token->type == ENil;
}

int atom_token_integer(atom_token* token)
{
  return token->value.int_number;
}

double atom_token_float(atom_token* token)
{
  return token->value.float_number;
}

char* atom_token_string(atom_token* token)
{
  return token->value.string;
}

char* atom_token_symbol(atom_token* token)
{
  return token->value.symbol;
}
