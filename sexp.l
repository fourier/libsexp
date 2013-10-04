/*
 * Lexer for SEXP library
 */

%top{
   /* This code goes at the "top" of the generated file. */
   #include <stdint.h>
   #include <inttypes.h>
   #include <stdio.h>

   #include "sexpcontainers.h"
   #include "sexptoken.h"

   sexp_token_cont_item *head = 0;

   static void add_to_head(sexp_token* token)
   {
     if (token)
     {
       if (!head)
         head = sexp_token_cont_item_alloc(token);
       else
         sexp_token_list_add(head,token);
     }
   }
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

{integer-number}    {printf("Integer %d", atoi(yytext));}
{floating-point-number} {printf("Float %f", atof(yytext));}
{string} {printf("String: '%s'", yytext);}
{symbol-nil} {printf("Nil");}
{symbol} {printf("Symbol: '%s'", yytext);}
{open-paren} {printf("Open paren");}
{close-paren} {printf("Close paren");}


[[:space:]] {}

%%
    
sexp_token_cont_item* tokenize()
{
  while(yylex()>0);
  return head;
}
