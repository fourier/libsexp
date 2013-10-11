%{
#include <stdio.h>

#include "libsexp.h"
#include "sexptoken.h"
#include "sexpitem.h"
         
void yyerror (char const *s);
int yylex(void);

static void item_print(sexp_item* item, void* data);

static sexp_item* g_parsed = 0;

%}

%debug
%code requires {#include "libsexp.h"}

%union {
  atom_token* atom;
  sexp_item*  sexp;
}

%token<atom> ATOM
%token OPENPAREN CLOSEPAREN
%type <sexp> sexp list list_contents
%start program

%%
program       :
            {}
     | sexp {g_parsed = $1;};
sexp          :
       ATOM   {$$ = sexp_item_create_atom($1);}
     | list   {$$ = $1;};
list          :
       OPENPAREN CLOSEPAREN {$$ = sexp_item_create_atom(atom_token_nil_alloc()); }
     | OPENPAREN list_contents CLOSEPAREN { $$ = $2; };
list_contents :
       sexp { $$ = sexp_item_create_cons($1,sexp_item_create_atom(atom_token_nil_alloc()));}
     | sexp list_contents { $$ = sexp_item_create_cons($1,$2); }

%%

void yyerror (char const *s)
{
  fprintf (stderr, "%s\n", s);
}

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

#if 0
int main( int argc, char *argv[] )
{
#if 0    
    extern FILE *yyin;
     ++argv; --argc;
     yyin = stdin;/*fopen( argv[0], "r" );*/
#endif
     /*yydebug = 1;*/
     yyparse ();
     printf("\n");sexp_item_traverse(parsed,item_print,(void*)0);printf("\n");
}
#endif

sexp_item* sexp_parse(const char* read_buffer)
{
    yy_scan_string(read_buffer);
    if (yyparse() == 0) return g_parsed;
    return 0;
}








