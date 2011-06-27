/* -*- Mode: C; tab-width: 2; indent-tabs-mode: nil; c-basic-offset: 2 -*- */
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <assert.h>

#include "sexptokens.h"
#include "sexpcontainers.h"


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



sexp_token* sexp_token_alloc(TerminalType type)
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


void sexp_token_verbose_print(sexp_token* token)
{
  if (token)
  {
    switch ( token->type)
    {
    case EOPENPAREN:
      printf("type: OpenParen\n");
      printf("value: '('\n");
      break;
    case ECLOSEPAREN:
      printf("type: CloseParen\n");
      printf("value: ')'\n");
      break;
    case EATOM:
      atom_token_verbose_print(token->atom);
      break;
    case EEND:
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
    case EOPENPAREN:
      printf("( ");
      break;
    case ECLOSEPAREN:
      printf(") ");
      break;
    case EATOM:
      atom_token_print(token->atom);
      printf(" ");
      break;
    case EEND:
      printf("$");
    default:
      break;
    }
  }  
}

sexp_cons* sexp_cons_alloc(sexp_item* car, sexp_cons* cdr)
{
  sexp_cons* result = calloc(sizeof(sexp_cons),1);
  result->car = car;
  result->cdr = cdr;
  return result;
}

sexp_cons* sexp_cons_free(sexp_cons* cons)
{
  /*
   * TODO: implement free procedure, maybe not recursively
   * hint: use the iterative versions of
   * pre-order/in-order/post-order tree traversal functions
   * see http://en.wikipedia.org/wiki/Tree_traversal
   */
  switch(cons->car->type)
  {
  case EAtom:
    atom_token_free(cons->car->value.atom);
    break;
  case ECons:
    /* TODO: rewrite this not recursively  */
    /* sexp_cons_free(cons->car->value.cons); */
  break;
  default:
    break;
  }
  /* TODO: rewrite this not recursively */
  sexp_cons_free(cons->cdr);
  return (sexp_cons*)0;
}


sexp_item* sexp_item_create_atom(sexp_token* from)
{
  sexp_item* result = calloc(sizeof(sexp_item),1);
  result->type = EAtom;
  result->value.atom = from->atom;
  from->atom = 0;
  return result;
}

sexp_item* sexp_item_create_cons(sexp_item* car, sexp_cons* cdr)
{
  sexp_item* result = calloc(sizeof(sexp_item),1);
  result->type = ECons;
  result->value.cons = sexp_cons_alloc(car,cdr);
  return result;
}

sexp_item* sexp_item_create_cons_plain(sexp_cons* cons)
{
  sexp_item* result = 0;
  if ( cons)
  {
    result = calloc(sizeof(sexp_item),1);
    result->type = ECons;
    result->value.cons = cons;
  }
  return result;
}

#if 0
sexp_item* sexp_item_free(sexp_item* item)
{
  if (item)
  {
    switch(item->type)
    {
    case EAtom:
      atom_token_free(item->value.atom);
      break;
    case ECons:
      sexp_cons_free(item->value.cons);
      break;
    default:
      break;
    }
    free(item);
  }
  return (sexp_item*)0;
}
#endif
sexp_item* sexp_item_free(sexp_item* item)
{
  if (item)
  {
    switch(item->type)
    {
    case EAtom:
      atom_token_free(item->value.atom);
      break;
    case ECons:
      
      break;
    default:
      break;
    }
    free(item);
  }
  return (sexp_item*)0;
}
    

sexp_item* sexp_item_car(sexp_item* item)
{
  sexp_item* result = 0;
  if (item)
  {
    assert(item->type == ECons);
    result = item->value.cons->car;
  }
  return result;
}

sexp_cons* sexp_item_cdr(sexp_item* item)
{
  sexp_cons* result = 0;
  if ( item)
  {
    result = item->value.cons->cdr;
  }
  return result;
}



void sexp_item_print(sexp_item* item)
{
  sexp_item_cont_item* stack;
  sexp_item *current, *tmp;
  if (item)
  {
    /* initialize stack */
    stack = sexp_item_cont_item_alloc(item);
    while (stack)
    {
      stack = sexp_item_stack_pop(stack,&current);
      if (!current)
      {
        printf(") ");
      }
      else
      {
        if ( current->type == ECons )
        {
          stack = sexp_item_stack_push(stack,0);
          printf("Cons( ");
          if (sexp_item_cdr(current))
          {
            tmp = sexp_item_create_cons_plain(sexp_item_cdr(current));
            stack = sexp_item_stack_push(stack,tmp);
          }

          if ( sexp_item_car(current))
            stack = sexp_item_stack_push(stack, sexp_item_car(current));
        }
        if ( current->type == EAtom)
        {
          atom_token_print(current->value.atom);
          printf(" ");
        }
      }
    }
  }
  printf("\n");
}

