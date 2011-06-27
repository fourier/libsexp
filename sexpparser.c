/* -*- Mode: C; tab-width: 2; indent-tabs-mode: nil; c-basic-offset: 2 -*- */
/* TODO: remove this include after finishing of the parse function */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <assert.h>

#include "sexpparser.h"


const int parser_stack_step = 50;

parser_stack_item parser_stack_item_create(StackItemType type,
                                           int value,
                                           sexp_item* item_value)
{
  parser_stack_item item;
  item.type = type;
  item.value = value;
  item.item_value = item_value;
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

/*
 * Handle rule before reduction
 * rule - rule number ( starting from 1 since 0 is the dummy-rule S1->S
 * items - array of values for every reduced symbol
 * size - items size
 */
static sexp_item* handle_rule_reduction(int rule, sexp_item** items, int size)
{
  int i;
  sexp_item* result = 0;
  /* for ( i = 0; i < size; ++ i) */
  /*   if (items[i] && items[i]->type == EAtom) */
  /*     atom_token_print(items[i]->value.atom); */
  /*
   * Rules:
   * (1) S -> atom: S.val = atom
   * (2) S -> L:    S.val = L.val
   * (3) E -> S:    E.val = cons(S.val, nil)
   * (4) E -> SE:   E.val = cons(S.val, E.val)
   * (5) L -> ():   L.val = nil
   * (6) L -> (E):  L.val = E.val
   */
  switch(rule)
  {
  case 1:
  case 2:
    result = items[0];
    break;
  case 3:
    result = sexp_item_create_cons(items[0],0);
    break;
  case 4:
    assert(items[1]->type == ECons);
    result = sexp_item_create_cons(items[0],items[1]->value.cons);
    break;
  case 5:
    break;
  case 6:
    result = items[1];
    break;
  case 0:
  default:
    assert(0);
    break;
  }
  return result;
}



sexp_item* sexp_parser(sexp_token_cont_item* head)
{
  sexp_item *result = 0;
  parser_stack* stack = parser_stack_alloc();
  parser_stack_item* pitem;
  parser_stack_item item;
  sexp_token_cont_item* current = head;
  sexp_item** items;
  sexp_item* item_value;
  int column,action_type,number,i,j,A;
  int do_exit = 0;

  /* initialize stack with the beginning state = 0 */
  parser_stack_push(stack,parser_stack_item_create(EStackItemState,0,0));
  
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
#define PUSH_PARSER_STACK(the_type,the_value,the_item_value)        \
    parser_stack_push(stack,                                        \
                      parser_stack_item_create((the_type),          \
                                               (the_value),         \
                                               (the_item_value)));

    switch(action_type)
    {
    case EACCEPT:
      parser_stack_pop(stack,&item);
      result = parser_stack_peek(stack)->item_value;
      printf("parser finished successfully\n");
      do_exit = 1;
      break;
    case EINVALID:
      /* parser_stack_pop(stack,&item);         */
      printf("parser finished in invalid state\n");
      do_exit = 1;
      break;
    case ESHIFT:              /* shift to the state 'number' */
      /* push current terminal and state to the stack */
      PUSH_PARSER_STACK(EStackItemTerminal,
                        current->token->type,
                        sexp_item_create_atom(current->token->atom));
      PUSH_PARSER_STACK(EStackItemState,number,0);
      current = current->next;
      break;
    case EREDUCE:      /* reduce by rule (number) A->rule */
      /* remove 2*|rule| symbols from stack  */
      assert(number > 0 && number < GRAMMAR_RULES_LIST_SIZE);
      items = calloc(sizeof(sexp_item*),grammar_rules_list[number].size);
      for ( i = 0; i < grammar_rules_list[number].size*2; ++ i)
      {
        parser_stack_pop(stack,&item);        
        if ( i&1 )
        {
          j = grammar_rules_list[number].size - (i >> 1) - 1;
          items[j] = item.item_value;
        }
      }
      /* handle grammar rule */
      item_value = handle_rule_reduction(number,
                                         items,
                                         grammar_rules_list[number].size);
      /* get current state into the number */
      pitem = parser_stack_peek_state(stack);
      i = pitem->value;
      /* push the A nonterminal to the stack */
      A = grammar_rules_list[number].start_symbol;
      PUSH_PARSER_STACK(EStackItemNonterminal,
                        A,
                        item_value);
      /* push the GOTO(i,A) to the stack (as a state) */
      PUSH_PARSER_STACK(EStackItemState,goto_table[i][A],0);
      printf("%s\n",grammar_rules_list[number].print_form);
      /*  */
      break;
    default:
      assert(0);
      break;
    }
    if ( do_exit)
      break;
  }
#undef PUSH_PARSER_STACK
  parser_stack_free(stack);
  return result;
}


