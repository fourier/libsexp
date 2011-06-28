/* -*- Mode: C; tab-width: 2; indent-tabs-mode: nil; c-basic-offset: 2 -*- */
#include <stdio.h>
#include <string.h>
#include <ctype.h>

#include "sexputils.h"
#include "sexptoken.h"

static char whitespaces[] = " \n\r\t";
 

/*
 * Auxulary functions used only in this source file
 */

static int is_from(char c, char* from)
{
  if (from && c)
  {
    while (*from)
    {
      if (c == *from)
        return 1;
      from++;
    }
  }
  return 0;
}


static char* skip_sign(char* str)
{
  static char signs[] = "+-";
  if (str)
  {
    if (is_from(*str,signs))
      str++;
  }
  return str;
}

static char* skip_decimal(char* str)
{
  if (str)
  {
    if (*str == '.')
      str++;
  }
  return str;
}

static char* skip_digits(char* str)
{
  if (str)
  {
    while (isdigit(*str))
      str++;
  }
  return str;
}

static char* skip_exponent(char* str)
{
  static char exponent_marker[] = "eE";
  char* begin = str;
  int counter = 0;
  if (str)
  {
    if (is_from(*str,exponent_marker))
    {
      str++;
      str = skip_sign(str);
      counter = str - begin - 1;      /* 0 if no sign, 1 otherwise */
      str = skip_digits(str);
      /* at least 2 chars: 'e' and digint */
      if (str - begin - counter  < 2) 
        str = begin;              
    }
  }
  return str;
}


/*
 * Actual implementation of declared functions
 */

char* skip_whitespaces(char* str)
{
  if (str)
  {
    while (is_from(*str, whitespaces))
      str++;
  }
  return str;
}
                      

char* find_end_of_floating_point_number(char* str)
{
  /* variables used */
  char* begin = str;
  char* ptr;
  int digits_skipped = 0;
  int decimal_skipped = 0;
  /* 1) - test for sign */
  str = skip_sign(str);
  /* 2) - skip digits */
  ptr = skip_digits(str);
  digits_skipped = ptr - str;
  str = ptr;
  /* 3) - determine decimal point or exponent */
  ptr = skip_decimal(str);
  decimal_skipped = ptr - str;
  str = ptr;
  /* 4) skip digits */
  str = skip_digits(str);
  /* 5) determine if exponencial form */
  if (!decimal_skipped &&        /* exponent required when no decimal */
      digits_skipped)            /* point and at least 1 digit*/
                                           
  {
    ptr = skip_exponent(str);
    /* check if no exponent found - error */
    str = (ptr == str ? begin : ptr);
  }
  else if (decimal_skipped)    /* when decimal present
                                * the exponent is optional  */
    str = skip_exponent(str);
  else                          /* looks like integer or something else */
    str = begin;                /* report error */
  return str;
}


char* find_end_of_integer_number(char* str)
{
  char* begin = str;
  int sign_skipped = 0;
  /* 1) skip sign */
  str = skip_sign(str);
  sign_skipped = str - begin;
  /* 2) skip digits */
  str = skip_digits(str);
  /* at least 1 digit shall be in place */
  return (str - begin - sign_skipped <= 0) ? begin : str;
}

char* find_end_of_quoted_string(char* str)
{
  char* begin = str;
  int do_continue = 1;
  if (str)
  {
    /* 1) starting character - doublequote */
    if (*str == '"')
    {
      str++;
      /* loop by all printable characters, spaces and escaped doublequote */
      while (*str && (isgraph(*str) || is_from(*str,whitespaces)))
      {
        /* 2) check for last doublequote*/
        if (*str == '"')          /* doublequote found */
        {
          /*
           * check if it is escaped doubleqoute
           * the escape character '\' goes after beginning doublequote
           */
          do_continue = str > begin + 1 && *(str-1) == '\\';
        }
        str++;
        if (!do_continue)
          break;
      }
    }
  }
  return str;
}

char* find_end_of_symbol(char* str)
{
  static char symbol_initial[] = "!$%&*/:<=>?^_~";
  static char symbol_constituent[] = "+-.@";
  static char peculiar_symbol[] = "+-";
  if (str)
  {
    if (is_from(*str,peculiar_symbol))
      str++;
    else if (isalpha(*str) || is_from(*str,symbol_initial))
    {
      /* initial symbol found, skip to next */
      str++;
      while (*str && (isalpha(*str) || is_from(*str,symbol_initial) ||
                      isdigit(*str) || is_from(*str,symbol_constituent)))
        str++;
    }
  }
  return str;
}


