---
title: stringlist_type
---

# `stdlib_stringlist_type` module (1-D list of strings)

[TOC]

## Introduction

The `stdlib_stringlist_type` module provides with 2 derived types to deal with lists of strings.
`stringlist_type` derived type represents one-dimensional list of variable-length strings (also referred as one-dimensional stringlist) which is compatible with Fortran intrinsic character and `stringlist_index_type` derived type represents an index to access, modify the elements of a stringlist, insert elements to a stringlist and much more.

## Derived types provided

<!-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -->
### `stringlist_type` derived type

The `stringlist_type` derived type represents one-dimensional list of strings (also referred as one-dimensional stringlist).  
The internal representation of the list is implementation dependent and is not visible to the user of the module.

Note: _stringlist_ is an abstract concept which is expressed through the derived type `stringlist_type`.

#### Status

Experimental


<!-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -->
### `stringlist_index_type` derived type

An instance of the derived type `stringlist_index_type` represents either a forward index OR a backward index. 
The internal representation is implementation dependent and is not visible to the user of the module.  
`list_head` and `list_tail` are 2 special instances of this type representing the head and the tail of a stringlist respectively.
An index is independent of the stringlist(or `stringlist_type`) it is used with and hence, an index can be used with multiple stringlists in the same program.

#### Status

Experimental


<!-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -->
### fidx/bidx

#### Description

`fidx`: Returns an instance which represents forward index `idx`.  
`bidx`: Returns an instance which represents backward index `idx`.

#### Syntax

For fidx: `res = ` [[stdlib_stringlist_type(module):fidx(interface)]] ` (idx)`
For bidx: `res = ` [[stdlib_stringlist_type(module):bidx(interface)]] ` (idx)`

#### Status

Experimental.

#### Class

Pure function.

#### Argument

- `idx`: Shall be of kind `integer`.
  This argument is `intent(in)`.

#### Result value

The result is of type `stringlist_index_type`.

#### Example

```fortran
{!example/stringlist_type/example_stringlist_type_fidx_bidx.f90!}
```

<!-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -->
### Constructor for `stringlist_type`(or stringlist)

#### Description

No arguments given: Initializes an empty stringlist(a stringlist containing no elements in it).

With argument: Initializes a stringlist equivalent to the input array `array` i.e. a stringlist containing all elements of the input array `array` in the same order.

#### Syntax

`res = ` [[stdlib_stringlist_type(module):stringlist_type(interface)]] ` ([array])`

#### Status

Experimental

#### Class

Pure function.

#### Argument

- `array`: Shall be an array of `character` scalar or array of [[stdlib_string_type(module):string_type(type)]].
 This argument is `intent(in)` and `optional`.

#### Result value

The result is an instance of type `stringlist_type`.

#### Example

```fortran
{!example/stringlist_type/example_stringlist_type_constructor.f90!}
```


<!-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -->
### insert_at

#### Description

Inserts the string `string` _AT_ the index `idx`, so that the newly added element is present at index `idx` after insertion. Inserting an element _AT_ index beyond `length + 1` inserts the element _AT_ `list_tail`, and likewise inserting _AT_ a non-positive index inserts the element _AT_ `list_head`.

#### Syntax

`call ` [[stdlib_stringlist_type(module):stringlist_type(type)]] `%` [[stringlist_type(type):insert_at(bound)]] ` (idx, string)`

#### Status

Experimental.

#### Class

Pure subroutine.

#### Argument

- `idx`: [[stdlib_stringlist_type(module):stringlist_index_type(type)]].
  This argument is `intent(in)`.

- `string`: Character scalar or [[stdlib_string_type(module):string_type(type)]].
  This argument is `intent(in)`.

#### Example

```fortran
{!example/stringlist_type/example_stringlist_type_insert_at.f90!}
```


<!-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -->
### get

#### Description

Returns the string present currently at the index `idx` in a stringlist. If index `idx` is out of bounds, then an empty string is returned.

#### Syntax

`res = ` [[stdlib_stringlist_type(module):stringlist_type(type)]] `%` [[stringlist_type(type):get(bound)]] ` (idx)`

#### Status

Experimental.

#### Class

Pure function.

#### Argument

- `idx`: [[stdlib_stringlist_type(module):stringlist_index_type(type)]].
  This argument is `intent(in)`.

#### Result value

The result is a string of type `string_type`.

#### Example

```fortran
{!example/stringlist_type/example_stringlist_type_get.f90!}
```


<!-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -->
### len

#### Description

Returns the number of elements present currently in the stringlist.

#### Syntax

`res = ` [[stdlib_stringlist_type(module):stringlist_type(type)]] `%` [[stringlist_type(type):len(bound)]] ` ()`

#### Status

Experimental.

#### Class

Pure function.

#### Argument

No arguments.

#### Result value

The result is of type `integer`.

#### Example

```fortran
{!example/stringlist_type/example_stringlist_type_len.f90!}
```


<!-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -->
### clear

#### Description

Removes all elements from a stringlist.

#### Syntax

`call ` [[stdlib_stringlist_type(module):stringlist_type(type)]] `%` [[stringlist_type(type):clear(bound)]] ` ()`

#### Status

Experimental.

#### Class

Pure subroutine.

#### Argument

No arguments.

#### Example

```fortran
{!example/stringlist_type/example_stringlist_type_clear.f90!}
```


<!-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -->
### Comparison operator equal

#### Description

Compares left hand side (lhs) with right hand side (rhs) for equality.

#### Syntax

`res = lhs == rhs`

`res = lhs .eq. rhs`

#### Status

Experimental.

#### Class

Pure function, `operator(==)` and `operator(.eq.)`.

#### Argument

- `lhs`: Shall be an array of `character` scalar or of [[stdlib_string_type(module):string_type(type)]] OR 
a [[stdlib_stringlist_type(module):stringlist_type(type)]].
 This argument is `intent(in)`.
 
- `rhs`: Shall be an array of `character` scalar or of [[stdlib_string_type(module):string_type(type)]] OR 
a [[stdlib_stringlist_type(module):stringlist_type(type)]].
 This argument is `intent(in)`.

#### Result value

The result is a default `logical` scalar value.

#### Example

```fortran
{!example/stringlist_type/example_stringlist_type_equality_operator.f90!}
```


<!-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -->
### Comparison operator not equal

#### Description

Compares left hand side (lhs) with right hand side (rhs) for inequality.

#### Syntax

`res = lhs /= rhs`

`res = lhs .ne. rhs`

#### Status

Experimental.

#### Class

Pure function, `operator(/=)` and `operator(.ne.)`.

#### Argument

- `lhs`: Shall be an array of `character` scalar or of [[stdlib_string_type(module):string_type(type)]] OR 
a [[stdlib_stringlist_type(module):stringlist_type(type)]].
 This argument is `intent(in)`.
 
- `rhs`: Shall be an array of `character` scalar or of [[stdlib_string_type(module):string_type(type)]] OR 
a [[stdlib_stringlist_type(module):stringlist_type(type)]].
 This argument is `intent(in)`.

#### Result value

The result is a default `logical` scalar value.

#### Example

```fortran
{!example/stringlist_type/example_stringlist_type_inequality_operator.f90!}
```


<!-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -->
### Concatenation operator(//)

#### Description

Returns the concatenated output of left hand side (lhs) and right hand side (rhs).

#### Syntax

`res = lhs // rhs`

#### Status

Experimental.

#### Class

Pure function, `operator(//)`.

#### Argument

- `lhs`: Shall be a `character` scalar or [[stdlib_string_type(module):string_type(type)]] OR an array of `character` scalar or of [[stdlib_string_type(module):string_type(type)]] OR 
a [[stdlib_stringlist_type(module):stringlist_type(type)]].
 This argument is `intent(in)`.
 
- `rhs`: Shall be a `character` scalar or [[stdlib_string_type(module):string_type(type)]] OR an array of `character` scalar or of [[stdlib_string_type(module):string_type(type)]] OR 
a [[stdlib_stringlist_type(module):stringlist_type(type)]].
 This argument is `intent(in)`.

#### Result value

The result is an instance of [[stdlib_stringlist_type(module):stringlist_type(type)]].

#### Example

```fortran
{!example/stringlist_type/example_stringlist_type_concatenate_operator.f90!}
```
