---
title: stringlist
---

# `stdlib_stringlist` module (1-D List of Strings)

[TOC]

## Introduction

The `stdlib_stringlist` module provides with 2 derived types to deal with list of strings.
`stringlist_type` derived type represents a one-dimensional list of variable-length strings which is compatible with Fortran intrinsic character and `stringlist_type` derived type represents an index to access, modify the elements of a stringlist and insert strings to a stringlist.

## Derived type provided

<!-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -->
### `stringlist_type` derived type

The `stringlist_type` derived type represents one-dimensional list of strings. 
The internal representation of the list is implementation dependent and is not visible to the user of the module.

#### Status

Experimental


<!-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -->
### `stringlist_index_type` derived type

An instance of the derived type `stringlist_index_type` represents either a forward index OR a backward index. 
The internal representation is implementation dependent and is not visible to the user of the module.  
`list_head` and `list_tail` are 2 special instances of this type representing the head and the tail of a stringlist respectively.
An instance is independent of the stringlist it is used with and hence, an index can be used with multiple stringlists in the same program.

#### Status

Experimental


<!-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -->
### insert_at

#### Description

Inserts the string `string` at the index `idx` such that after insertion is done the newly added element is at index `idx`.

#### Syntax

`call [[stdlib_stringlist(module):stringlist_type(type)]]%[[stdlib_stringlist(module):insert_at(generic)]] (idx, string)`

#### Status

Experimental.

#### Class

Pure subroutine.

#### Argument

- `idx`: [[stdlib_stringlist(module):stringlist_index_type(type)]].
  This argument is intent(in).

- `string`: Character scalar or [[stdlib_string_type(module):string_type(type)]].
  This argument is intent(in).

#### Result value

No result.

#### Example

```fortran
program demo_insert_at
  use stdlib_stringlist, only: stringlist_type, insert_at, stringlist_index_type, fidx, bidx
  use stdlib_string_type, only: string_type
  implicit none

  type(stringlist_type)       :: stringlist
  type(stringlist_index_type) :: index

  index = fidx(1)
  call stringlist%insert_at( index, "Element No. one" )
    ! stringlist <-- {"Element No. one"}

  index = bidx(1)
  call stringlist%insert_at( index, string_type( "Element No. two" ) )
    ! stringlist <-- {"Element No. one", "Element No. two"}

  call stringlist%insert_at( fidx(2), string_type( "Element No. three" ) )
    ! stringlist <-- {"Element No. one", "Element No. three", "Element No. two"}

  call stringlist%insert_at( bidx(1), "Element No. four" )
    ! stringlist <-- {"Element No. one", "Element No. three", "Element No. two", "Element No. four"}

end program demo_insert_at
```


<!-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -->
### get

#### Description

Returns the string present currently at the index `idx` in the given stringlist. If `idx` is out of bounds an empty string is returned.

#### Syntax

`res = [[stdlib_stringlist(module):stringlist_type(type)]]%[[stdlib_stringlist(module):get(generic)]] (idx)`

#### Status

Experimental.

#### Class

Pure function.

#### Argument

- `idx`: [[stdlib_stringlist(module):stringlist_index_type(type)]].
  This argument is intent(in).

#### Result value

The result is a string of type `string_type`.

#### Example

```fortran
program demo_get
  use stdlib_stringlist, only: stringlist_type, insert_at, fidx, get
  use stdlib_string_type, only: string_type
  implicit none

  type(stringlist_type) :: stringlist
  type(string_type)     :: output

  !> adding 4 elements to the stringlist
  call stringlist%insert_at( fidx(1), "Element No. one" )
  call stringlist%insert_at( fidx(1), "Element No. two" )
  call stringlist%insert_at( fidx(1), "Element No. three" )
  call stringlist%insert_at( fidx(1), "Element No. four" )
    ! stringlist <-- {"Element No. four", "Element No. three", "Element No. two", "Element No. one"}

  output = stringlist%get( fidx(1) )
    ! output <-- "Element No. four"
  
  output = stringlist%get( bidx(1) )
    ! output <-- "Element No. one"

  !> accessing out of bounds index
  output = stringlist%get( bidx(5) )
    ! output <-- ""
  output = stringlist%get( fidx(0) )
    ! output <-- ""

end program demo_get
```


<!-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -->
### len

#### Description

Returns the number of elements present in the stringlist.

#### Syntax

`res = [[stdlib_stringlist(module):stringlist_type(type)]]%[[stdlib_stringlist(module):len()]] ()`

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
program demo_len
  use stdlib_stringlist, only: stringlist_type, insert_at, bidx, len
  implicit none

  type(stringlist_type) :: stringlist
  integer               :: output

  output = stringlist%len()
    ! output <-- 0

  !> adding 2 elements to the stringlist
  call stringlist%insert_at( bidx(1), "Element No. one" )
  call stringlist%insert_at( bidx(1), "Element No. two" )
    ! stringlist <-- {"Element No. one", "Element No. two"}

  output = stringlist%len()
    ! output <-- 2

end program demo_len
```
