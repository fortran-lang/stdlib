---
title: stringlist type
---

# `stdlib_stringlist_type` module (1-D List of strings)

[TOC]

## Introduction

The `stdlib_stringlist_type` module provides with 2 derived types to deal with lists of strings.
`stringlist_type` derived type represents a one-dimensional list of variable-length strings which is compatible with Fortran intrinsic character and `stringlist_index_type` derived type represents an index to access, modify the elements of a stringlist and insert strings to a stringlist.

## Derived types provided

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
An instance is independent of the `stringlist_type` it is used with and hence, an index can be used with multiple stringlists in the same program.

#### Status

Experimental


<!-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -->
### fidx/bidx

#### Description

`fidx`: Returns an instance which represents forward index `idx`.  
`bidx`: Returns an instance which represents backward index `idx`.

#### Syntax

For fidx: `res = [[stdlib_stringlist_type(module):fidx()]] (idx)`.  
For bidx: `res = [[stdlib_stringlist_type(module):bidx()]] (idx)`.

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
program demo_fidx_bidx
  use stdlib_stringlist_type, only: stringlist_type, stringlist_index_type, fidx, bidx
  implicit none

  type(stringlist_type) :: stringlist
  type(stringlist_index_type) :: index

  index = fidx(1)

  !> inserting 2 elements to the stringlist
  call stringlist%insert_at( fidx(1), "Element No. one" )
  call stringlist%insert_at( index, "Element No. two" )
    ! stringlist <-- {"Element No. two", "Element No. one"}

  index = bidx(3)

  !> inserting 2 elements to the stringlist
  call stringlist%insert_at( bidx(1), "Element No. three" )
  call stringlist%insert_at( index, "Element No. four" )
    ! stringlist <-- {"Element No. two", "Element No. four", 
    "Element No. one", "Element No. three"}

end program demo_fidx_bidx
```

<!-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -->
### Constructor for `stringlist_type`

#### Description

No arguments given: Initializes an empty stringlist(a list containing no elements in it).

With argument: Initializes a stringlist equivalent to the input array `array` i.e. a stringlist containing all elements of the input array `array` in the same order.

#### Syntax

`res = [[stdlib_stringlist_type(module):stringlist_type(interface)]] ([array])`

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
program demo_constructor
  use stdlib_stringlist_type, only: stringlist_type
  use stdlib_string_type, only: string_type
  implicit none

  type(stringlist_type) :: stringlist

  stringlist = stringlist_type()
    ! stringlist <-- { } (empty stringlist)

  stringlist = stringlist_type(["#1", "#2", "#3"])
    ! stringlist <-- {"#1", "#2", "#3"}

  stringlist = stringlist_type([string_type("#1"), string_type("#2")])
    ! stringlist <-- {"#1", "#2"}

end program demo_constructor
```


<!-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -->
### insert_at

#### Description

Inserts the string `string` _AT_ the index `idx`, so that the newly added element is present at index `idx` after insertion. Inserting an element _AT_ index beyond `length + 1` inserts the element _AT_ `list_tail`, and likewise inserting _AT_ a non-positive index inserts the element _AT_ `list_head`.

#### Syntax

`call [[stdlib_stringlist_type(module):stringlist_type(type)]]%[[stdlib_stringlist_type(module):insert_at(generic)]] (idx, string)`

#### Status

Experimental.

#### Class

Pure subroutine.

#### Argument

- `idx`: [[stdlib_stringlist_type(module):stringlist_index_type(type)]].
  This argument is `intent(in)`.

- `string`: Character scalar or [[stdlib_string_type(module):string_type(type)]].
  This argument is `intent(in)`.

#### Result value

No result.

#### Example

```fortran
program demo_insert_at
  use stdlib_stringlist_type, only: stringlist_type, stringlist_index_type, fidx, bidx
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

Returns the string present currently at the index `idx` in a stringlist. If index `idx` is out of bounds, then an empty string is returned.

#### Syntax

`res = [[stdlib_stringlist_type(module):stringlist_type(type)]]%[[stdlib_stringlist_type(module):get(generic)]] (idx)`

#### Status

Experimental.

#### Class

Pure function.

#### Argument

- `idx`: [[stdlib_stringlist_type(module):stringlist_index_type(type)]].
  This argument is intent(in).

#### Result value

The result is a string of type `string_type`.

#### Example

```fortran
program demo_get
  use stdlib_stringlist_type, only: stringlist_type, fidx, bidx
  use stdlib_string_type, only: string_type
  implicit none

  type(stringlist_type) :: stringlist
  type(string_type)     :: output

  !> inserting 4 elements to the stringlist
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

Returns the number of elements present currently in the stringlist.

#### Syntax

`res = [[stdlib_stringlist_type(module):stringlist_type(type)]]%[[stdlib_stringlist_type(module):len()]] ()`

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
  use stdlib_stringlist_type, only: stringlist_type, bidx
  implicit none

  type(stringlist_type) :: stringlist
  integer               :: output

  output = stringlist%len()
    ! output <-- 0

  !> inserting 2 elements to the stringlist
  call stringlist%insert_at( bidx(1), "Element No. one" )
  call stringlist%insert_at( bidx(1), "Element No. two" )
    ! stringlist <-- {"Element No. one", "Element No. two"}

  print'(a)', stringlist%len()
    ! 2

end program demo_len
```


<!-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -->
### clear

#### Description

Removes all elements from a stringlist.

#### Syntax

`call [[stdlib_stringlist_type(module):stringlist_type(type)]]%[[stdlib_stringlist_type(module):clear()]] ()`

#### Status

Experimental.

#### Class

Pure subroutine.

#### Argument

No arguments.

#### Result value

No result.

#### Example

```fortran
program demo_clear
  use stdlib_stringlist_type, only: stringlist_type, fidx
  implicit none

  type(stringlist_type) :: stringlist

  !> inserting 2 elements to the stringlist
  call stringlist%insert_at( fidx(1), "Element No. one" )
  call stringlist%insert_at( fidx(1), "Element No. two" )
    ! stringlist <-- {"Element No. two", "Element No. one"}

  call stringlist%clear()
    ! stringlist <-- { } (empty stringlist)

  !> inserting 1 element to the stringlist
  call stringlist%insert_at( fidx(1), "Element No. one" )
    ! stringlist <-- {"Element No. one"}

end program demo_clear
```