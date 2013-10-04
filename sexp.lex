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
digit                 [0-9]
exponent-marker       "e"|"E"
exponent              {exponent-marker}{sign}?{digit}+
integer-number        {sign}?{digit}+
floating-point-number {sign}?{digit}*{decimal-point}{digit}*{exponent}?|{sign}?{digit}+({decimal-point}{digit}*)?{exponent}

space " "|"\n"|"\r"|"\t"

%%

{integer-number}    {printf("Integer %d", atoi(yytext));}
{floating-point-number} {printf("Float %f", atof(yytext));}

%%
    
int main()
{
  yylex();
  return 0;
}
