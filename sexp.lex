/*
 * Lexer for SEXP library
 */

%top{
   /* This code goes at the "top" of the generated file. */
   #include <stdint.h>
   #include <inttypes.h>
   #include <stdio.h>
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

%%

{integer-number}    {printf("Integer %d", atoi(yytext));}
{floating-point-number} {printf("Float %f", atof(yytext));}
{string} {printf("String: '%s'", yytext);}

[[:space:]] {}

%%
    
int main()
{
  yylex();
  return 0;
}
