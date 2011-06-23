/* -*- Mode: C; tab-width: 2; indent-tabs-mode: nil; c-basic-offset: 2 -*- */
#include <stdio.h>
#include <string.h>
#include <ctype.h>

#include "sexputils.h"
#include "sexptokens.h"
#include "sexplexer.h"
#include "sexpcontainers.h"
#include "sexpparser.h"

int main ()
{
  char* float_examples[] = {"0.0",
                            "0E0",
                            "-.0",
                            "0.",                          
                            "0.0E0",                       
                            "0E0",                         
                            "3.1415926535897932384d0",
                            "6.02E+23",
                            "602E+21",
                            "asdasd",
                            "",
                            "1223",
                            "z32sd",
                            "+-asd",
                            "123e?",
                            "e+10",
                            0};
  char* integer_examples[] = {"100",
                              "100500 ",
                              "-1000",
                              "+999",
                              "- 100",
                              "-",
                              0};
  
  char* string_examples[] = {"\"hello world 111\"",
                             "\"\"",
                             "\"this is a string with \\\"escaped\\\" sustr\"",
                             0};
  char* s_expression_example = " (defun factorial (x) \
   (if (zerop x) \
       1 \
       (* x (factorial (- x 1)))))";

  char** ptr;
  char* p;
  sexp_token_cont_item *head, *top;
  sexp_token* token;
  parser_stack* pstack;
  parser_stack_item pstack_item;
  
  /* test floats */
  ptr = float_examples;
  while (*ptr)
  {
    p = find_end_of_floating_point_number(*ptr);
    printf("String: %s, ptr = 0x%x, found = 0x%x, is float = %s\n",
           *ptr,(unsigned int)(*ptr),(unsigned int)p,
           (p == *ptr ? "No!" : "Yes!"));
    ptr++;
  }
  printf ("===============================================================\n");

  /* test integers */
  ptr = integer_examples;
  while (*ptr)
  {
    p = find_end_of_integer_number(*ptr);
    printf("String: %s, ptr = 0x%x, found = 0x%x, is integer = %s\n",
           *ptr,(unsigned int)(*ptr),(unsigned int)p,
           (p == *ptr ? "No!" : "Yes!"));
    ptr++;
  }
  printf ("===============================================================\n");
  
  /* test strings */
  ptr = string_examples;
  while (*ptr)
  {
    p = find_end_of_quoted_string(*ptr);
    printf("String: '%s'\n",*ptr);
    printf("%s string, defined length = %d, calculated length = %d\n",
           (p == *ptr ? "Incorrect" : "Correct"), strlen(*ptr),p - *ptr);
    ptr++;
  }

  printf ("===============================================================\n");
  /* create a list: '(', 0, ')' */
  head = sexp_token_cont_alloc(sexp_token_alloc(EOPENPAREN));
  top = sexp_token_list_add(head,
                            sexp_token_alloc(EATOM));
  top = sexp_token_list_add(head,
                            sexp_token_alloc(ECLOSEPAREN));
  top = head;
  while(top)
  {
    printf("0x%x\n",(unsigned int)top);
    top = top->next;
  }
  head = sexp_token_cont_list_free(head);
  printf ("===============================================================\n");

  p = s_expression_example;
  token = read_sexp_token(&p);  
  if (token)
    head = sexp_token_cont_alloc(token);
  while ( (token = read_sexp_token(&p)))
    sexp_token_list_add(head,token);

  top = head;
  while(top)
  {
    /* sexp_token_verbose_print(top->token); */
    sexp_token_print(top->token);
    top = top->next;
  }
  printf("\n");
  head = sexp_token_cont_list_free(head);

  printf ("===============================================================\n");
  p = s_expression_example;
  token = read_sexp_token(&p);  
  if (token)
    head = sexp_token_cont_alloc(token);
  while ( (token = read_sexp_token(&p)))
    head = sexp_token_stack_push(head,token);
  top = head;
  while(top)
  {
    sexp_token_print(top->token);
    top = sexp_token_stack_pop(top);
  }
  printf("\n");

  /* test parser stack */
  pstack = parser_stack_alloc();

  pstack_item.type = EStackItemTerminal;
  parser_stack_push(pstack, pstack_item);
  pstack_item.type = EStackItemNonterminal;
  parser_stack_push(pstack, pstack_item);
  pstack_item.type = EStackItemNonterminal;

  parser_stack_pop(pstack,&pstack_item);
  parser_stack_pop(pstack,&pstack_item);
  parser_stack_pop(pstack,&pstack_item);
  
  pstack = parser_stack_free(pstack);

  /* test the parser function */
  p = "(setf a (+ 1 2 ))";
  token = read_sexp_token(&p);  
  if (token)
    head = sexp_token_cont_alloc(token);
  while ( (token = read_sexp_token(&p)))
    sexp_token_list_add(head,token);

  sexp_parser(head);
  
  head = sexp_token_cont_list_free(head);
  return 0;
}

