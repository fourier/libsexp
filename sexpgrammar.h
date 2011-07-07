/* -*- Mode: C; tab-width: 2; indent-tabs-mode: nil; c-basic-offset: 2 -*- */
/*
 Copyright (C) 2011 Alexey Veretennikov (alexey dot veretennikov at gmail.com)
 
 This file is part of Libsexp.

 Libsexp is free software: you can redistribute it and/or modify
 it under the terms of the GNU Lesser General Public License as published
 by the Free Software Foundation, either version 3 of the License, or
 (at your option) any later version.

 Libsexp is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 GNU Lesser General Public License for more details.

 You should have received a copy of the GNU Lesser General Public License
 along with Libsexp.  If not, see <http://www.gnu.org/licenses/>.
*/
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

#define ACTION_TABLE_ROWS 10
#define ACTION_TABLE_COLS 4
extern const action action_table[ACTION_TABLE_ROWS][ACTION_TABLE_COLS];

#define GOTO_TABLE_ROWS 10
#define GOTO_TABLE_COLS 4
extern const int goto_table[GOTO_TABLE_ROWS][GOTO_TABLE_COLS];


#endif /* __SEXPGRAMMAR_H__ */
