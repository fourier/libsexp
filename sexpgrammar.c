/* -*- Mode: C; tab-width: 2; indent-tabs-mode: nil; c-basic-offset: 2 -*- */
#include "sexpgrammar.h"

const int terminals_list[TERMINALS_LIST_SIZE] = {EATOM, EOPENPAREN, ECLOSEPAREN, EEND};


const int nonterminals_list[NONTERMINALS_LIST_SIZE] = {ENONTERM_S1, ENONTERM_S, ENONTERM_E, ENONTERM_L};


const grammar_rule grammar_rules_list[GRAMMAR_RULES_LIST_SIZE] = {{ENONTERM_S1, 1, "S1 -> S"}, {ENONTERM_S, 1, "S -> ATOM"}, {ENONTERM_S, 1, "S -> L"}, {ENONTERM_E, 1, "E -> S"}, {ENONTERM_E, 2, "E -> S E "}, {ENONTERM_L, 2, "L -> OPENPAREN CLOSEPAREN "}, {ENONTERM_L, 3, "L -> OPENPAREN E CLOSEPAREN "}};


const action action_table[ACTION_TABLE_ROWS][ACTION_TABLE_COLS] =  
{
  {{ESHIFT, 3}, {ESHIFT, 4}, {EINVALID, -1}, {EINVALID, -1}},
  {{EINVALID, -1}, {EINVALID, -1}, {EINVALID, -1}, {EACCEPT, 0}},
  {{EREDUCE, 2}, {EREDUCE, 2}, {EREDUCE, 2}, {EREDUCE, 2}},
  {{EREDUCE, 1}, {EREDUCE, 1}, {EREDUCE, 1}, {EREDUCE, 1}},
  {{ESHIFT, 3}, {ESHIFT, 4}, {ESHIFT, 7}, {EINVALID, -1}},
  {{ESHIFT, 3}, {ESHIFT, 4}, {EREDUCE, 3}, {EINVALID, -1}},
  {{EINVALID, -1}, {EINVALID, -1}, {ESHIFT, 9}, {EINVALID, -1}},
  {{EREDUCE, 5}, {EREDUCE, 5}, {EREDUCE, 5}, {EREDUCE, 5}},
  {{EINVALID, -1}, {EINVALID, -1}, {EREDUCE, 4}, {EINVALID, -1}},
  {{EREDUCE, 6}, {EREDUCE, 6}, {EREDUCE, 6}, {EREDUCE, 6}}
};

const int goto_table[GOTO_TABLE_ROWS][GOTO_TABLE_COLS] = 
{
  {-1, 1, -1, 2},
  {-1, -1, -1, -1},
  {-1, -1, -1, -1},
  {-1, -1, -1, -1},
  {-1, 5, 6, 2},
  {-1, 5, 8, 2},
  {-1, -1, -1, -1},
  {-1, -1, -1, -1},
  {-1, -1, -1, -1},
  {-1, -1, -1, -1}
};

