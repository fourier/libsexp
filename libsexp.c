/* -*- Mode: C; tab-width: 2; indent-tabs-mode: nil; c-basic-offset: 2 -*- */
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <stdio.h>
#include <math.h>
#include <assert.h>

#include "libsexp.h"
#include "sexplexer.h"
#include "sexpparser.h"
#include "sexptoken.h"

double atom_token_number(atom_token* token)
{
  double result = NAN;
  assert(token);
  switch(token->type)
  {
  case EIntegerNumber:
    result = token->value.int_number;
    break;
  case EFloatNumber:
    result = token->value.int_number;
    break;
  case ESymbol:
  case EString:
  case ENil:
  default:
    break;
  }
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
    result = sexp_item_is_symbol(car,symbol);
  }
  return result;
}

int sexp_item_is_symbol(sexp_item* item, const char* symbol)
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

sexp_item* sexp_parse(const char* text)
{
  const char* p = text;

  /* list of tokens */
  sexp_token_cont_item *head = 0;
  sexp_token* token = 0;

  /* parser result */
  sexp_item* sexp;

  /* 1. Lexer */
  token = read_sexp_token(&p);  
  if (token)
    head = sexp_token_cont_item_alloc(token);
  while ( (token = read_sexp_token(&p)))
    sexp_token_list_add(head,token);

  /* 2. Parser */
  sexp = parse_sexp_token_list(head,0);
  if (!sexp)
    fprintf(stderr,"Unable to parse input!\n");
  /* 3. Free the allocated memory  */
  head = sexp_token_cont_list_free(head);
  return sexp;
}
