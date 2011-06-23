/* -*- Mode: C; tab-width: 2; indent-tabs-mode: nil; c-basic-offset: 2 -*- */
/* TODO: remove this include after finishing of the parse function */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <assert.h>

#include "sexpparser.h"


const int parser_stack_step = 50;

parser_stack_item parser_stack_item_create(StackItemType type, int value)
{
  parser_stack_item item;
  item.type = type;
  item.value = value;
  return item;
}

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

parser_stack_item* parser_stack_peek(parser_stack* stack)
{
  return stack->top ? &stack->items[stack->top-1] : (parser_stack_item*) 0;
}

static parser_stack_item* parser_stack_peek_state(parser_stack* stack)
{
  parser_stack_item* pitem = parser_stack_peek(stack);
  assert(pitem && pitem->type == EStackItemState);
  assert(pitem->value >= 0 && pitem->value < ACTION_TABLE_ROWS);
  return pitem;
}

void sexp_parser(sexp_token_cont_item* head)
{
  parser_stack* stack = parser_stack_alloc();
  parser_stack_item* pitem;
  parser_stack_item item;
  sexp_token_cont_item* current = head;
  int column,action_type,number,i,A;

  /* initialize stack with the beginning state = 0 */
  parser_stack_push(stack,parser_stack_item_create(EStackItemState,0));
  
  /* endless parse loop, will finish either in accepted */
  /* state or in invalid state */
  while(1)
  {
    /* get the value of the stack item. it shall be the parser state */
    pitem = parser_stack_peek_state(stack);
    /* calculate colum */
    column = current ? current->token->type : EEND;
    /* get action from the action table */
    action_type = action_table[pitem->value][column].type;
    number = action_table[pitem->value][column].number;
    switch(action_type)
    {
    case EACCEPT:
      printf("parser finished successfully\n");
      return;
      break;
    case EINVALID:
      printf("parser finished in invalid state\n");
      return;
      break;
    case ESHIFT:              /* shift to the state 'number' */
      /* push current terminal and state to the stack */
      parser_stack_push(stack, parser_stack_item_create(EStackItemTerminal,
                                                        current->token->type));
      parser_stack_push(stack,parser_stack_item_create(EStackItemState,
                                                       number));
      current = current->next;
      break;
    case EREDUCE:      /* reduce by rule (number) A->rule */
      /* remove 2*|rule| symbols from stack  */
      assert(number > 0 && number < GRAMMAR_RULES_LIST_SIZE);
      for ( i = 0; i < grammar_rules_list[number].size*2; ++ i)
        parser_stack_pop(stack,&item);
      /* get current state into the number */
      pitem = parser_stack_peek_state(stack);
      i = pitem->value;
      /* push the A nonterminal to the stack */
      A = grammar_rules_list[number].start_symbol;
      parser_stack_push(stack,
                        parser_stack_item_create(EStackItemNonterminal,A));
      /* push the GOTO(i,A) to the stack (as a state) */
      parser_stack_push(stack,
                        parser_stack_item_create(EStackItemState,
                                                 goto_table[i][A]));
      printf("%s\n",grammar_rules_list[number].print_form);
      /*  */
      break;
    default:
      assert(0);
      break;
    }
  }
}
