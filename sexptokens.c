/* -*- Mode: C; tab-width: 2; indent-tabs-mode: nil; c-basic-offset: 2 -*- */
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include "sexptokens.h"



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
    default:
      break;
    }
    free(token);
  }
  return (atom_token*)0;
}

atom_token* atom_token_integer_alloc(char* begin, char* end)
{
  atom_token* token = atom_token_alloc(EIntegerNumber);
  if ( begin != end)
  {
    token->value.int_number = atoi(begin);
  }
  return token;
}

atom_token* atom_token_float_alloc(char* begin, char* end)
{
  atom_token* token = atom_token_alloc(EFloatNumber);
  char* parsed_end = end;
  char* buf = (char*)0;
  int size = end - begin + 1;
  token->value.float_number = strtod(begin,&parsed_end);
  if ( parsed_end != end)
  {
    buf = calloc(size,1);
    memcpy(buf,begin,size - 1);
    fprintf(stderr,"number: '%s' incorrectly parsed",buf);
    free(buf);
  }
  return token;
}

atom_token* atom_token_string_alloc(char* begin, char* end)
{
  atom_token* token = atom_token_alloc(EString);
  int size = end - begin + 1;
  token->value.string = calloc(size, 1);
  memcpy(token->value.string,begin,size - 1);
  return token;
}

atom_token* atom_token_symbol_alloc(char* begin, char* end)
{
  atom_token* token = atom_token_alloc(ESymbol);
  int size = end - begin + 1;
  token->value.string = calloc(size, 1);
  memcpy(token->value.symbol,begin,size - 1);
  return token;
}

void atom_token_verbose_print(atom_token* token)
{
  if (token)
  {
    switch (token->type)
    {
    case EIntegerNumber:
      printf("type: Integer\n");
      printf("value: %d\n",token->value.int_number);
      break;
    case EFloatNumber:
      printf("type: Float\n");
      printf("value: %f\n",token->value.float_number);
      break;
    case EString:
      printf("type: String\n");
      printf("value: %s\n",token->value.string);
      break;
    case ESymbol:
      printf("type: Symbol\n");
      printf("value: %s\n",token->value.symbol);
      break;
    default:
      break;
    }
  }
}

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
    default:
      break;
    }
  }
}



sexp_token* sexp_token_alloc(SexpTokenType type)
{
  sexp_token* token = calloc(1, sizeof(sexp_token));
  if (token)
    token->type = type;
  return token;
}


sexp_token* sexp_token_free(sexp_token* token)
{
  if (token)
  {
    switch (token->type)
    {
    case EAtom:
      token->atom = atom_token_free(token->atom);
      break;
    case EListOpenParen:
    case EListCloseParen:
    default:
      break;
    }
    free(token);
  }
  return (sexp_token*)0;
}


void sexp_token_verbose_print(sexp_token* token)
{
  if (token)
  {
    switch ( token->type)
    {
    case EListOpenParen:
      printf("type: OpenParen\n");
      printf("value: '('\n");
      break;
    case EListCloseParen:
      printf("type: CloseParen\n");
      printf("value: ')'\n");
      break;
    case EAtom:
      atom_token_verbose_print(token->atom);
      break;
    default:
      break;
    }
  }
}

void sexp_token_print(sexp_token* token)
{
  if (token)
  {
    switch ( token->type)
    {
    case EListOpenParen:
      printf("( ");
      break;
    case EListCloseParen:
      printf(") ");
      break;
    case EAtom:
      atom_token_print(token->atom);
      printf(" ");
      break;
    default:
      break;
    }
  }  
}


