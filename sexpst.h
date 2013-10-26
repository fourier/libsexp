/* -*- Mode: C; tab-width: 2; indent-tabs-mode: nil; c-basic-offset: 2 -*- */
/*
 Copyright (C) 2013 Alexey Veretennikov (alexey dot veretennikov at gmail.com)
 
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

#ifndef SEXPST_H
#define SEXPST_H

/*
 * String table singleton used in libsexp
 * Created once at the beginning of the parsing and then reused
 * Operations supported: add string, get string by id
 */


/*
 * Add the string to the string table, returning the string id
 * If string is already exists, its id returned
 * This function takes ownership of the string. If string is
 * not found, it is added to the table. If string is found,
 * the original string pointer deallocated.
 * In both cases, one MUST NOT use the pointer after calling
 * the sexpst_add function! Get it if necessary by calling
 * the sexpst_get function.
 */
int sexpst_add(const char* string);

/*
 * Get the string from string table by id if exists
 * Returns 0 if not found
 */
const char* sexpst_get(int string_id);

#endif
