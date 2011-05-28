/* -*- Mode: C; tab-width: 2; indent-tabs-mode: nil; c-basic-offset: 2 -*- */
#ifndef SEXPUTILS_H
#define SEXPUTILS_H

/*
 * skips whitespaces from the input stream
 */
char* skip_whitespaces(char* str);

/*
 * returns pointer to the next-to-the-last character
 * if str is a floating point number
 *
 * Grammatics:
 * floating-point-number ::= [sign] {digit}* decimal-point {digit}* [exponent]
 *                       | [sign] {digit}+ [decimal-point {digit}*] exponent
 * sign ::= "+" | "-"
 * decimal-point ::= "."
 * digit ::= "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9"
 * exponent ::= exponent-marker [sign] {digit}+
 * exponent-marker ::= "e" | "E"
 */
char* find_end_of_floating_point_number(char* str);


/*
 * returns pointer to the next-to-the-last character
 * if str is a decimal integer
 *
 * Grammatics:
 * integer-number ::= [sign]{digit}+
 * sign ::= "+" | "-"
 * digit ::= "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9"
 */
char* find_end_of_integer_number(char* str);

/*
 * returns pointer to the next-to-the-last character
 * if str is a quoted string
 *
 * Grammatics:
 * space ::= " " | "\n" | "\r" "\t"
 * punct = "!" | "#" | "$" | "%" | "&" | "'" | "(" | ")" | "*" | "+" | ","
 *       | "-" | "." | "/" | ":" | ";" | "<" | "=" | ">" | "?" | "@" | "["
 *       | "\" | "]" | "^" | "_" | "`" | "{" | "|" | "}" | "~"
 * escape = "\"
 * double-quote = """
 * escaped-double-quote = escape double-quote
 * # not formal :)
 * alpha ::= a-zA-Z
 * printable ::= digit | punct | alpha
 * string ::= double-quote {printable|space|escaped-double-quote}* double-quote
 */
char* find_end_of_quoted_string(char* str);

/*
 * returns pointer to the next-to-the-last character
 * if str is a symbol. For convenience will account max
 * symbol size = 255
 *
 * Grammatics:
 * symbol ::= symbol-initial {symbol-constituent}*
 *         | peculiar-symbol
 * symbol-initial ::= alpha | "!" | "$" | "%" | "&" | "*" | "/" | ":" | "<"
 *       | "=" | ">" | "?" | "^" |  "_" | "~"
 * symbol-constituent :== symbol-initial | digit | "+" | "-" | "." | "@"
 * peculiar-symbol ::= "+" | "-"
 */
char* find_end_of_symbol(char* str);



#endif /* SEXPUTILS_H */
