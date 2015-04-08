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

#ifndef SEXPUTILS_H
#define SEXPUTILS_H


/*
 * skips whitespaces from the input stream
 */
const char* skip_whitespaces(const char* str);

/*
 * skips one-line comments starting with ';' up to the end of the string */
const char* skip_comment(const char* str);

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
const char* find_end_of_floating_point_number(const char* str);


/*
 * returns pointer to the next-to-the-last character
 * if str is a decimal integer
 *
 * Grammatics:
 * integer-number ::= [sign]{digit}+
 * sign ::= "+" | "-"
 * digit ::= "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9"
 */
const char* find_end_of_integer_number(const char* str);

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
const char* find_end_of_quoted_string(const char* str);

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
const char* find_end_of_symbol(const char* str);



#endif /* SEXPUTILS_H */
