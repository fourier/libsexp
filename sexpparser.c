/* -*- Mode: C; tab-width: 2; indent-tabs-mode: nil; c-basic-offset: 2 -*- */
#include <stdlib.h>
#include <string.h>

#include "sexpparser.h"

const int parser_stack_step = 50;

parser_stack* parser_stack_alloc()
{
  parser_stack* stack = calloc(1, sizeof(parser_stack));
  return stack;
}

parser_stack* parser_stack_free(parser_stack* stack)
{
  if (stack)
  {
    free(stack->items);
  }
  return (parser_stack*)0;
}


void parser_stack_push(parser_stack* stack, parser_stack_item item)
{
  /* Check if stack is not empty. Create items otherwize */
  if ( !stack->items)
  {
    stack->items = calloc(parser_stack_step,sizeof(parser_stack_item));
    stack->items[0] = item;
    stack->allocated = parser_stack_step;
    /* top is always pointing to the next item */
    stack->top = 1;
  }
  /* if stack is not empty */
  else
  {
    /* if there is no need to reallocate stack */
    if ( stack->top < stack->allocated)
    {
      stack->items[stack->top++] = item;
    }
    /* Sad but we need to reallocate the stack */
    else                        
    {
      stack->items =
        (parser_stack_item*)realloc(stack->items,
                                    stack->allocated + parser_stack_step);
      stack->allocated += parser_stack_step;
      stack->items[stack->top++] = item;
    }
  }
}

int parser_stack_pop(parser_stack* stack, parser_stack_item* item)
{
  int result = -1;
  if (stack->top)
  {
    memcpy(item,&stack->items[--stack->top],sizeof(parser_stack_item));
    result = stack->top;
  }
  return result;
}


