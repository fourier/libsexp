/* -*- Mode: C; tab-width: 2; indent-tabs-mode: nil; c-basic-offset: 2 -*- */
#include <stdio.h>
#include <stdlib.h>

#include "sexpgrammar.h"
#include "sexplexer.h"
#include "sexpparser.h"

const int block_size = 255;

int main(int argc, const char* argv[])
{
  /* input file */
  FILE* file = stdin;

  /* buffer to read to */
  char* read_buffer = calloc(block_size+1,1);
  char* p;
  
  /* auxulary counters */
  int read_chunk = 0,read = 0;
  int read_from_file = argc > 1;

  /* list of tokens */
  sexp_token_cont_item *head = 0;
  sexp_token* token = 0;

  /* parser result */
  sexp_item* sexp;
  
  /* set the file to read from */
  if ( read_from_file )
    file = fopen(argv[1],"rt");

  /* read file contents  */
  while(!feof(file))
  {
    read_chunk = fread(read_buffer+read,1,block_size, file);
    read += read_chunk;
    
    read_buffer = (char*)realloc(read_buffer,read + block_size);
  }
  read_buffer[read] = '\0';
  
  /* close the file if necessary */
  if ( read_from_file)
    fclose(file);

  /* 1. Lexer */
  p = read_buffer;
  token = read_sexp_token(&p);  
  if (token)
    head = sexp_token_cont_item_alloc(token);
  while ( (token = read_sexp_token(&p)))
    sexp_token_list_add(head,token);

  /* 2. Parser */
  sexp = sexp_parse(head,1);
  /* 3. Print output */
  sexp_item_print(sexp);
  /* free allocated memory */
  sexp = sexp_item_free(sexp);
  free(read_buffer);    
  head = sexp_token_cont_list_free(head);
  
  return 0;
}
