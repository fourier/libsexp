/* -*- Mode: C; tab-width: 2; indent-tabs-mode: nil; c-basic-offset: 2 -*- */
#include <stdio.h>
#include <stdlib.h>

#include "libsexp.h"

const int block_size = 255;


static void item_print(sexp_item* item, void* data)
{
  data = 0;                     /* to reduce compiler warnings */
  if (item->atom)
  {
    atom_token_print(item->atom);
    printf(" ");
  }
  else
  {
    printf("Cons (");
  }
}


int main(int argc, const char* argv[])
{
  /* input file */
  FILE* file = stdin;

  /* buffer to read to */
  char* read_buffer = calloc(block_size+1,1);

  /* parser result */
  sexp_item* sexp;
  
  /* auxulary counters */
  int read_chunk = 0,read = 0;
  int read_from_file = argc > 1;

  /* set the file to read from */
  if ( read_from_file )
    file = fopen(argv[1],"rt");

  if (file)
  {
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

    sexp = sexp_parse(read_buffer);
    free(read_buffer);
    printf("\n");    
    sexp_item_traverse(sexp,item_print,(void*)0);
    printf("\n");
    sexp_item_free(sexp);
  }
  else
  {
    printf("Unable to open file\n");
  }
  return 0;
}
