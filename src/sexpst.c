/* -*- Mode: C; tab-width: 2; indent-tabs-mode: nil; c-basic-offset: 2 -*- */
/*
  Copyright (C) 2013 Alexey Veretennikov (alexey dot veretennikov at gmail.com)
 
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

#include "sexpst.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>

#define SEXPST_INITIAL_SIZE 10

struct sexpst_internal
{
  int size;
  const char** strings;
  int nexp_index;
};

struct sexpst_internal g_sexpst = {0,0,-1};

static void sexpst_fini();

/*
 * Initialize the String Table with initial (pre-allocated) number of
 * lines size
 */
static void sexpst_init(int size)
{
  if (!g_sexpst.size)           /* not initialized */
  {
    g_sexpst.size = size;
    g_sexpst.strings = malloc(size*sizeof(const char*));
    assert(g_sexpst.strings);
    g_sexpst.nexp_index = 0;
    atexit(sexpst_fini);
  }
}

static void sexpst_init_on_demand()
{
  if (!g_sexpst.size)
    sexpst_init(SEXPST_INITIAL_SIZE);
}

static void sexpst_fini()
{
  int i;
  if (g_sexpst.size)
  {
    for (i = 0; i < g_sexpst.nexp_index; ++ i)
      free((char*)g_sexpst.strings[i]);
    free(g_sexpst.strings);
    /*
    printf("Destroying the String table, size = %d, number of strings = %d\n",
           g_sexpst.size, g_sexpst.nexp_index);
    */
    g_sexpst.size = 0;
  }
}

static void sexpst_grow()
{
  sexpst_init_on_demand();
  /* grow by 20% */
  g_sexpst.size = g_sexpst.size + g_sexpst.size/5;
  g_sexpst.strings = realloc(g_sexpst.strings, g_sexpst.size*sizeof(const char*));
  assert(g_sexpst.strings);
}


int sexpst_add(const char* string)
{
  int i;
  sexpst_init_on_demand();
  for (i = 0; i < g_sexpst.nexp_index; ++ i)
    if (!strcmp(string, g_sexpst.strings[i]))
    {
      free((char*)string);
      return i;
    }
  if (g_sexpst.nexp_index == g_sexpst.size)
    sexpst_grow();
  g_sexpst.strings[g_sexpst.nexp_index] = string;
  return g_sexpst.nexp_index++;
}

const char* sexpst_get(int string_id)
{
  sexpst_init_on_demand();
  return (string_id >= 0 && string_id < g_sexpst.nexp_index) ?
    g_sexpst.strings[string_id] : 0;
}

