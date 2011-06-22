/* -*- Mode: C; tab-width: 2; indent-tabs-mode: nil; c-basic-offset: 2 -*- */
#ifndef __SEXPGRAMMAR_H__
#define __SEXPGRAMMAR_H__

typedef enum
{
  EINVALID,
  ESHIFT,
  EREDUCE,
  EACCEPT
} ParserState;

typedef enum
{
  EATOM,
  EOPENPAREN,
  ECLOSEPAREN,
  EEND
} TerminalType;

typedef enum
{
  ENONTERM_S1,
  ENONTERM_S,
  ENONTERM_E,
  ENONTERM_L
} NonterminalType;


typedef struct
{
  ParserState type;
  int number;
} action;

typedef struct
{
  NonterminalType start_symbol;
  int size;
  const char* print_form;
} grammar_rule;

#define TERMINALS_LIST_SIZE 4
extern const int terminals_list[TERMINALS_LIST_SIZE];

#define NONTERMINALS_LIST_SIZE 4
extern const int nonterminals_list[NONTERMINALS_LIST_SIZE];

#define GRAMMAR_RULES_LIST_SIZE 7
extern const grammar_rule grammar_rules_list[GRAMMAR_RULES_LIST_SIZE];

#define ACTION_TABLE_SIZE 4
extern const action action_table[][ACTION_TABLE_SIZE];

#define GOTO_TABLE_SIZE 4
extern const int goto_table[][GOTO_TABLE_SIZE];


#endif /* __SEXPGRAMMAR_H__ */
