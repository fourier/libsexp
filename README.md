Libsexp
=======
Copyright (C) 2011-2013 Alexey Veretennikov (alexey dot veretennikov at gmail dot com)


Description.
------------
Libsexp is a tiny project to create a library for parsing/operating
with [S-Expressions](http://en.wikipedia.org/wiki/S-expression).
It can be used instead of XML parsing libraries when
someone needs the simple tree-like text format of the input data for the
application.
It supports the parsing of the simple atomic S-expressions, being either
atom, like:
<pre>
3.14
</pre>
or single list:
<pre>
(hello
  (the world))
</pre>
It doesn't support set of lists in its input data, like:
<pre>
(hello)
(the world)
</pre>
so use it carefully with this assumption.


Usage.
------

In order to use the library build it and statically link against **libsexp.a**. Typically it shall be enough to use the only header **libsexp.h**.

In order to compile one need to have the C compiler only. However if it is necessary to regenerate lexer and parser files(which are part of the source package), the **GNU Bison** and **FLEX** shall also be installed.

Usage example can be found in **main.c**.

Typical usage is:

0. Include library header:

    ```c
#include <libsexp.h>
```
1. Define some traversal function, like:

    ```c
void traverse(sexp_item* item, void* data)
{
/* do something with item */
}
```
2. Create a pointer to sexp_item variable:

    ```c
sexp_item* item;
```
3. Parse the file or zero-terminated buffer and store parsed tree to this variable, like this:

    ```c
file = fopen("myfile.sexp","rt");
item = sexp_parse_file(file);
```
or
    ```c
item = sexp_parse_str("(:test-strings number 1 is (\"hello\" \"world\"))");
```

4. Traverse through the parsed tree

    ```c
sexp_item_traverse(item,traverse,(void*)data);
```
5. Free allocated memory

    ```c
sexp_item_free(item);
```

Supported data.
---------------

The data stored in sexp_item structure is either atom or list. The structure is forward-declared in ```libsexp.h``` and declared in ```sexpitem.h```:
```c
typedef struct sexp_item;
```
In order to determine if sexp is a list (or particularly cons-cell), one can use the function
```c
int sexp_item_is_cons(sexp_item* item);
```
List is a pair of pointers(so called CONS-cell): to the first element (CAR) and to the list of rest
elements (CDR). In order to access them there are 2 convenient functions:
```c
sexp_item* sexp_item_car(sexp_item* item);
sexp_item* sexp_item_cdr(sexp_item* item);
```
Also one can use the following functions for manipulating with lists:
```c
int sexp_item_length(sexp_item* item);
```
returns the length of the list

```c
sexp_item* sexp_item_nth(sexp_item* item, int i);
```
returns the nth element of the list

However since the list itself is a linked list it can be convenient to use
the following pattern to traverse the list:
```c
sexp_item* next = sexp_item_cdr(item);

while(!sexp_item_is_nil(next)
{
  item = sexp_item_car(next);
  /* do something with item */
  next = sexp_item_cdr(next);
}
```
In order to determine if item is of type atom, the following function provided:
```c
int sexp_item_is_atom(sexp_item* item);
```
ATOM type of the SEXP can be of the following 5 types:
integer number, float number, string, symbol(and special type NIL).
The type of atom can be found by using the following functions:
```c
/* return non-zero if item is atom of type nil */
int sexp_item_is_nil(sexp_item* item);

/* return non-zero if item is atom of type integer */
int sexp_item_is_integer(sexp_item* item);

/* return non-zero if item is atom of type float */
int sexp_item_is_float(sexp_item* item);

/* return non-zero if item is atom of type string */
int sexp_item_is_string(sexp_item* item);

/* return non-zero if item is atom of type symbol */
int sexp_item_is_symbol(sexp_item* item);
```
String is the double-quoted array of characters while symbol is the unquoted array
of characters. Currently only ASCII-characters supported (for simplicity)
Symbols are always stored in the upper-case.

In order to access appropriate type of the atom, use the following construction,
for example for integer numbers:
```c
extern sexp_item* item;
int i;
if (sexp_item_is_integer(item))
  i = sexp_item_inumber(item);
```
To extract floating point value from the atom which hase either integer or float
type, use the following function:
```c
double sexp_item_fnumber(sexp_item* item);
```
To extract only integer value:
```c
int sexp_item_inumber(sexp_item* item);
```
For strings and symbols:
```c
const char* sexp_item_string(sexp_item* item);
const char* sexp_item_symbol(sexp_item* item);
```

Where are also set of functions to help in analysis of the given S-expression:
```c
int sexp_item_is_symbol_like(sexp_item* item, const char* symbol);
```
This function returns not-zero value if iten is of type symbol (and symbol
argument is 0), and if symbol argument points to some string(no matter upper-cased
or down-cased) it will also compare item to this symbol name. For example if
sexp_item* item is of type Symbol 'HELLO', the following call will return non-zero
value:

```c
sexp_item_is_symbol_like(item,"hello")
```

```c
int sexp_item_starts_with_symbol(sexp_item* item, const char* symbol);
```
By given the item of type list compare returns nonzero value if the first
element is of type Symbol, and, if symbol argument is not zero, compares the
first element with its value (regardless of the case).
Example: given ```sexp_item*``` item with contents <pre>(Hello 0 1)</pre>
```sexp_item_starts_with_symbol(item,"hello")``` will return nonzero value.

```c
sexp_item* sexp_item_attribute(sexp_item* item, const char* attribute);
```
Search in the list by 'attributes'. Attributes are subsequent pair of list
elements, first of them is of type Symbol starting with ':', and second of any
type. Lists with attributes for example are: (:hello 1 :the 2). Here we have
2 attribute pairs: ":hello" with the value 1 and ":the" with the value 2.
This function searches through the list for a given attribute without
preceding ':' character. For example by given
```sexp_item* item = (:hello 1 :the 1)``` the following call
```c
sexp_item_attribute(item,"Hello")
```
will return the **sexp_item** pointing to 1.


