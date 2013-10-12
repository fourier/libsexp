/*
 * Lexer for SEXP library
 */

%top{
   /* This code goes at the "top" of the generated file. */
   #include <stdint.h>
   #include <inttypes.h>
   #include <stdio.h>

   #include "libsexp.tab.h"
   #include "sexptoken.h"
}

sign                  "+"|"-"
decimal-point         "."
exponent-marker       "e"|"E"
exponent              {exponent-marker}{sign}?[[:digit:]]+
integer-number        {sign}?[[:digit:]]+
floating-point-number {sign}?[[:digit:]]*{decimal-point}[[:digit:]]*{exponent}?|{sign}?[[:digit:]]+({decimal-point}[[:digit:]]*)?{exponent}

escape                "\\"
double-quote          "\""
escaped-double-quote {escape}{double-quote}
string               {double-quote}([[:graph:]]|[[:space:]]|{escaped-double-quote})*{double-quote}

symbol-nil           (?i:nil)
symbol-initial       [[:alpha:]]|"!"|"$"|"%"|"&"|"*"|"/"|":"|"<"|"="|">"|"?"|"^"|"_"|"~"
symbol-constituent   {symbol-initial}|[[:digit:]]|"+"|"-"|"."|"@"
peculiar-symbol      "+"|"-"|"..."
symbol               {symbol-initial}{symbol-constituent}*|{peculiar-symbol}

open-paren           "("
close-paren          ")"

%%

{integer-number}        {
/*    
    add_to_head(sexp_token_alloc(EATOM,
                                 atom_token_integer_alloc(yytext,yytext+yyleng)));
*/
      yylval.atom = atom_token_integer_alloc(yytext,yytext+yyleng);
      return ATOM;  
    }
    
{floating-point-number} {
/*
    add_to_head(sexp_token_alloc(EATOM,
                                 atom_token_float_alloc(yytext,yytext+yyleng)));
*/
      yylval.atom = atom_token_float_alloc(yytext,yytext+yyleng);
      return ATOM;
    }
    
{string}                {
/*
    add_to_head(sexp_token_alloc(EATOM,
                                 atom_token_string_alloc(yytext,yytext+yyleng)));
*/
      yylval.atom = atom_token_string_alloc(yytext,yytext+yyleng);
      return ATOM;
    }

{symbol-nil}            {
/*
    add_to_head(sexp_token_alloc(EATOM,
                                 atom_token_nil_alloc()));
*/
      yylval.atom = atom_token_nil_alloc();
      return ATOM;
    }

{symbol}                {
/*
    add_to_head(sexp_token_alloc(EATOM,
                                 atom_token_symbol_alloc(yytext,yytext+yyleng)));
*/
      yylval.atom = atom_token_symbol_alloc(yytext,yytext+yyleng);
      return ATOM;    
    }
    
{open-paren}            {
/*
    add_to_head(sexp_token_alloc(EOPENPAREN,0));
*/
      return OPENPAREN;
    }

{close-paren}           {
/*
    add_to_head(sexp_token_alloc(ECLOSEPAREN,0));
*/
      return CLOSEPAREN;
    }

;.*(\r?\n)   {}

[[:space:]] {}

%%

/*    
sexp_token_cont_item* tokenize(const char* str)
{

  yy_scan_string(str);
  while(yylex()>0);
  return head;

}

*/