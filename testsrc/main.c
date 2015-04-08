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

#include <stdio.h>
#include <stdlib.h>

#include "libsexp.h"

#define READ_FILE_TO_MEMORY 0

const int block_size = 16384;

static void item_print(sexp_item* item, void* data)
{
  (void)data;                     /* to reduce compiler warnings */
  if (sexp_item_is_atom(item))
  {
    sexp_item_print(item);
    printf(" ");
  }
  else
  {
    printf("Cons (");
  }
}

#if READ_FILE_TO_MEMORY
static char* read_file(FILE* file)
{
  /* buffer to read to */
  char* read_buffer = 0;
  /* auxulary counters */
  int read_chunk = 0,read = 0;

  if (file)
  {
    read_buffer = calloc(block_size+1,1);
    /* read file contents  */
    while(!feof(file))
    {
      read_chunk = fread(read_buffer+read,1,block_size, file);
      read += read_chunk;
    
      read_buffer = (char*)realloc(read_buffer,read + block_size);
    }
    read_buffer[read] = '\0';
  }
  return read_buffer;
}
#endif

static void parse_file(FILE* file)
{
  /* parser result */
  sexp_item* sexp;
  /* buffer to read to */
#if READ_FILE_TO_MEMORY
  char* read_buffer = 0;
#endif
  if (file)
  {
#if READ_FILE_TO_MEMORY
    /* read file contents  */
    read_buffer = read_file(file);
    
    if (read_buffer)
    {
      /* parse read contents */
      sexp = sexp_parse_str(read_buffer);
#else
      sexp = sexp_parse_file(file);
#endif
      if (sexp)
      {
        printf("\n");    
        sexp_item_traverse(sexp,item_print,(void*)0);
        printf("\n");
        /* printf("size: %d\n", sexp_item_size(sexp)); */
        sexp_item_free(sexp);
      }
#if READ_FILE_TO_MEMORY      
      free(read_buffer);
    }
#endif
  }
}

int main(int argc, const char* argv[])
{
  /* input file */
  FILE* file = stdin;
  int i = 1;
  
  /* set the file to read from */
  if ( argc > 1 )
  {
    for (; i < argc; ++ i)
    {
      file = fopen(argv[i],"rt");
      if (file)
      {
        parse_file(file);
        /* close the file if necessary */
        fclose(file);
      }
      else
        fprintf(stderr,"Unable to open file %s\n", argv[i]);
    }
  }
  else
  {
    parse_file(file);
  }
  return 0;
}
