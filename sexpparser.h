/* -*- Mode: C; tab-width: 2; indent-tabs-mode: nil; c-basic-offset: 2 -*- */
#ifndef _SEXPPARSER_H_
#define _SEXPPARSER_H_

#include "sexptokens.h"

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
} parser_stack_item;

typedef struct
{
  int top;
  /* do not use this field! */
  int allocated;
  parser_stack_item* items;
} parser_stack;

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



#endif /* _SEXPPARSER_H_ */
