---
title: stringlist
---
# Lists of strings

[TOC]

## Introduction

Fortran has supported variable-length strings since the 2003 standard,
but it does not have a native type to handle collections of strings of
different lengths. Such collections are quite useful though and the
language allows us to define a derived type that can handle such
collections.

The `stdlib_stringlist` module defines a derived type that is capable of
storing a list of strings and of manipulating them.

Methods include:

* inserting strings at a given position
* replacing strings at a given position
* deleting a single string or a range of strings
* retrieving a string or a range of strings at a given position
* finding the position of a particular string or a string which contains some substring
* sorting the list

## Positions in a list of strings

The module implements what are effectively infinitely long lists: a position is
represented as a positive integer, but there is no "out-of-bound" index. That is,
the following piece of code will simply work:

```fortran
type(stringlist_type) :: list

! Add two strings ...
call list%insert( list_head, "The first string" )
call list%insert( 20, "The last string" )

write(*,*) 'The last:    ', list%get(list_end)
write(*,*) 'Beyond that: ', list%get(30)
```
The special position `list_head` represents the position *before* the first element. Likewise,
the special position `list_end` represents the position of the *last* element. You can
use these positions to insert a string before the first string that is already in the
list or to insert after the last string that has been inserted.

If you specify a position beyond the last, the `list%get()` method simply returns an empty
string.

You can also specify *negative* positions, but they are interpreted as going back from the
last inserted string. If you need the last but one string, you can do so innthis way:

```fortran
write(*,*) 'The last but onw: ', list%get(list_end-1)
```

So, it is possible to do simple arithmetic.

*Note:* this does not work for the head of the list.

## The derived type: stringlist_type

### Status

Experimental

### Description

The type holds a small number of components and gives access to a number of procedures,
some of which are implemented as subroutines, others as functions or as operations.


### Public `stringlist_type` methods

The following methods are defined:

Method               | Class      | Description
---------------------|------------|------------
[`delete`](./stdlib_stringlist.html#delete-delete_one_or_more_strings)                                                 | Subroutine | Delete one or more strings from the list
[`destroy`](./stdlib_stringlist.html#destroy_destroy_all_strings_in_the_list)                                          | Subroutine | Destroy the contents of the list
[`get`](./stdlib_stringlist.html#get-get_a_single_string_from_a_list)                                                  | Function   | Get a string from a particular position
[`index`](./stdlib_stringlist.html#index-find_the_index_of_a_particular_string_in_the_list)                            | Function   | Find the index of a string in a list
[`index_sub`](./stdlib_stringlist.html#index_sub-find_the_index_of_a_particular_string_containing_the_given_substring) | Function   | Find the index of a string containing a partilcar substring
[`insert`](./stdlib_stringlist.html#insert-insert_one_or_more_strings_after_a_given_position)                          | Subroutine | Insert a string or a list after a given position
[`length`](./stdlib_stringlist.html#length-return_the_length_of_the_list)                                              | Function   | Return the index of the last set position
[`range`](./stdlib_stringlist.html#range-retrieve_a_range_of_string_from_the_list)                                     | Function   | Retrieve a range of strings from the list
[`replace`](./stdlib_stringlist.html#replace-replace_one_or_more_strings_between_two_given_positions)                  | Subroutine | Replace one or more stringa between two positions
[`sort`](./stdlib_stringlist.html#sort-return_a_sorted_list)                                                           | Function   | Sort the list and return the result as a new list
[`=`](./stdlib_stringlist.html#assign-copy_the_contents_of_a_list)                                                     | Assignment | Copy a list
[`//`](./stdlib_stringlist.html#//-concatenate_a_list_with_one_or_more_strings)                                        | Operation  | Concatenate a list with a string or concatenate two lists


## Details of the methods

### `delete` - delete one or more strings

#### Status

Experimental

#### Description

Delete one or more strings from the list via a given position or positions.

#### Syntax

`call list % [[stringlist_type(type):delete(bound)]]( first [, last] )`

#### Class

Subroutine

#### Arguments

`list`: the stringlist variable from which to delete one or more strings

`first`: the index of the first string to be deleted

`last` (optional): the index of the last string to be deleted. If left out, only one string is deleted.
If the value is lower than that of `first`, the range is considered to be empty and nothing is deleted.


### `destroy` - destroy all strings in the list

#### Status

Experimental

#### Description

Destroy the entire contents of the list. As the variable holding the list is simply a derived type, the variable
itself is not destroyed.

#### Syntax

`call list % [[stringlist_type(type):destroy(bound)]]`

#### Class

Subroutine

#### Arguments

`list`: the stringlist variable from which to delete all strings


### `get` - get a single string from the list

#### Status

Experimental

#### Description

Get the string at the given position.

#### Syntax

`string = list % [[stringlist_type(type):get(bound) ( idx )]]`

#### Class

Function

#### Arguments

`list`: the stringlist variable to retrieve a string from

`idx`: the index of the string to be retrieved (see [`the section on positions`](./stdlib_stringlist.html#position-in-a-list-of-strings)

#### Result value

A copy of the string stored at the indicated position.


### `index` - find the index of a particular string in the list

#### Status

Experimental

#### Description

Get the position of the first stored string that matches the given string, if `back` is not present or false. If `back` is
false, return the position of the last stored string that matches. Note that trailing blanks are ignored.

#### Syntax

`idx = list % [[stringlist_type(type):index(bound) ( string, back )]]`

#### Class

Function

#### Arguments

`list`: the stringlist variable to retrieve a string from

`string`: the string to be found in the list

`back` (optional): logical argument indicating the first occurrence should be returned (`false`) or the last (`true`)

#### Result value

The result is either the index of the string in the list or -1 if the string was not found

#### Example

Because trailing blanks are ignored, the following calls will give the same result:

```fortran
    write(*,*) list%index( 'A' )
    write(*,*) list%index( 'A    ' )
```


### `index_sub` - find the index of a string containing the given substring in the list

#### Status

Experimental

#### Description

Get the position of the first stored string that contains the given substring, if `back` is not present or false. If `back` is
false, return the position of the last stored string that contains it.

#### Syntax

`idx = list % [[stringlist_type(type):index_sub(bound) ( substring, back )]]`

#### Class

Function

#### Arguments

`list`: the stringlist variable to retrieve a string from

`substring`: the substring in question

`back` (optional): logical argument indicating the first occurrence should be returned (`false`) or the last (`true`)

#### Result value

The result is either the index of the string in the list or -1 if the string was not found


### `insert` - insert one or more strings after a given position

#### Status

Experimental

#### Description

Insert one or more strings after a given position. The position may be anything as explained in the section on positions.
A single string may be inserted, another list of strings or a plain array of strings. In all cases trailing blanks, if any,
are retained.

#### Syntax

`idx = list % [[stringlist_type(type):insert(bound) ( idx, string )]]`

#### Class

Subroutine

#### Arguments

`list`: the stringlist variable to insert the string(s) into

`idx`: the position after which the strings should be inserted

`string`: the string to be inserted, a list of strings or a plain array of strings


### `length` - return the length of the list

#### Status

Experimental

#### Description

Return the length of the list, defined as the highest index for which a string has been assigned. You can place strings
in any position without needing to fill in the intervening positions.

#### Syntax

`length = list % [[stringlist_type(type):length(bound) ()]]`

#### Class

Function

#### Arguments

`list`: the stringlist variable to retrieve the length from

#### Result value

Returns the highest index of a string that has been set.



### `range` - retrieve a range of strings from the list

#### Status

Experimental

#### Description

Retrieve the strings occurring between the given positions as a new list.

#### Syntax

`rangelist = list % [[stringlist_type(type):range(bound) ( first, last )]]`

#### Class

Function

#### Arguments

`list`: the stringlist variable to insert the string(s) into

`first`: the position of the first string to be retrieved

`last`: the position of the last string to be retrieved

#### Result value

The result is a new list containing all the strings that appear from the first to the last position, inclusively.



### `replace` - replace one or more strings between two given positions

#### Status

Experimental

#### Description

Replace one or more strings between two given positions. The new strings may be given as a single string, a list of
strings or a plain array.

#### Syntax

`call list % [[stringlist_type(type):replace(bound) ( first, last, string )]]`

#### Class

Subroutine

#### Arguments

`list`: the stringlist variable to replace the string(s) in


`first`: the position of the first string to be retrieved

`last`: the position of the last string to be retrieved

`string`: the string to be inserted, a list of strings or a plain array of strings



### `sort` - return a sorted list

#### Status

Experimental

#### Description

Create a new list consisting of the sorted strings of the given list. The strings are sorted according to ASCII, either
in ascending order or descending order.

#### Syntax

`sortedlist = list % [[stringlist_type(type):sort(bound) ( ascending )]]`

#### Class

Subroutine

#### Arguments

`list`: the stringlist variable of which the contents should be copied

`ascending` (optional): if not present or true, sort the list in ascending order, otherwise descending

#### Result value

The contents of the given list is sorted and then stored in the new list.


### `=` - copy the contents of a list

#### Status

Experimental

#### Description

Copy an existing list to a new one. The original list remains unchanged.

#### Syntax

`copylist = list`

#### Class

Assignment

#### Operands

`list`: the stringlist variable to be copied



### `//` - concatenate a list with one or more strings

#### Status

Experimental

#### Description

Concatenate a list with a string, a list of strings or a plain array

#### Syntax

`concatenatedlist = list // string`

`concatenatedlist = string // list`

#### Class

Assignment

#### Operands

`list`: the stringlist variable to be concatenated

`string`: the string to be concatenated, a list of strings or a plain array of strings

#### Result value

A stringlist that contains the concatenation of the two operands.



## TODO

Additional methods:

filter

map

Suggestions from the discussion
