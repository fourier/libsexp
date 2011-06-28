/* -*- Mode: C; tab-width: 2; indent-tabs-mode: nil; c-basic-offset: 2 -*- */
#ifndef _SEXPPARSER_H_
#define _SEXPPARSER_H_

#include "sexptoken.h"
#include "sexpcontainers.h"


typedef enum
{
  EStackItemTerminal,
  EStackItemNonterminal,
  EStackItemState
} StackItemType;

typedef struct
{
  StackItemType type;
  int value;
  sexp_item* item_value;
} parser_stack_item;

typedef struct
{
  int top;
  /* do not use this field! */
  int _allocated;
  parser_stack_item* items;
} parser_stack;

/* create parser stack item by given parameters */
parser_stack_item parser_stack_item_create(StackItemType type,
                                           int value,
                                           sexp_item* item_value);

/* Allocate memory for the stack and initialize it with zeros */
parser_stack* parser_stack_alloc();

/* Free all allocated memory for the stack */
parser_stack* parser_stack_free(parser_stack* stack);

/* Push item to the top of the stack */
void parser_stack_push(parser_stack* stack, parser_stack_item item);

/*
 * Pop item from the stack(item shall not be 0).
 * Returns -1 when stack is empty; stack size otherwize
 */
int parser_stack_pop(parser_stack* stack, parser_stack_item* item);

/*
 * Returns a pointer to the stack's top. if no elements in stack,
 * return value is 0
 */
parser_stack_item* parser_stack_peek(parser_stack* stack);

/*
 * Parse the list of token started by head
 * if do_print != print the parse rules button-up
 * returns sexp_itep pointing to the root of the parse tree
 * returns 0 in case of parse errors
 */
sexp_item* sexp_parse(sexp_token_cont_item* head, int do_print);

#endif /* _SEXPPARSER_H_ */
