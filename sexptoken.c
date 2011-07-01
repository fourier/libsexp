/* -*- Mode: C; tab-width: 2; indent-tabs-mode: nil; c-basic-offset: 2 -*- */
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <assert.h>

#include "sexptoken.h"
#include "sexpcontainers.h"

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


atom_token* atom_token_alloc(AtomTokenType type)
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
  int size = end - begin + 1;
  token->value.string = calloc(size, 1);
  memcpy(token->value.symbol,begin,size - 1);
  return token;
}

atom_token* atom_token_nil_alloc()
{
  atom_token* token = atom_token_alloc(ENil);
  return token;
}


sexp_token* sexp_token_alloc(TerminalType type, atom_token* atom)
{
  sexp_token* token = calloc(1, sizeof(sexp_token));
  token->type = type;
  token->atom = atom;
  return token;
}


sexp_token* sexp_token_free(sexp_token* token)
{
  if (token)
  {
    switch (token->type)
    {
    case EATOM:
      token->atom = atom_token_free(token->atom);
      break;
    case EOPENPAREN:
    case ECLOSEPAREN:
    case EEND:
    default:
      break;
    }
    free(token);
  }
  return (sexp_token*)0;
}
