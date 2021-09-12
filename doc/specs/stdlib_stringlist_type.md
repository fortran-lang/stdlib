---
title: stringlist type
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
  use stdlib_stringlist_type, only: stringlist_index_type, fidx, bidx
  implicit none

  type(stringlist_index_type) :: index

  index = fidx(1)
    ! forward index 1

  index = bidx(3)
    ! backward index 3

end program demo_fidx_bidx
```

<!-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -->
### Constructor for `stringlist_type`(or stringlist)

#### Description

No arguments given: Initializes an empty stringlist(a stringlist containing no elements in it).

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
  This argument is `intent(in)`.

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
program demo_equality_operator
  use stdlib_stringlist_type, only: stringlist_type, fidx, list_head, operator(==)
  use stdlib_string_type, only: string_type
  implicit none

  type(stringlist_type)          :: stringlist
  type(string_type), allocatable :: stringarray(:)
  logical                        :: res

  !> inserting 4 elements to the stringlist
  call stringlist%insert_at( fidx(1), "#1" )
  call stringlist%insert_at( list_head, "#2" )
  call stringlist%insert_at( fidx(1), "#3" )
  call stringlist%insert_at( list_head, "#4" )
    ! stringlist <-- {"#4", "#3", "#2", "#1"}

  !> creating an array of 4 string_type elements
    stringarray = [string_type("#4"), string_type("#3"), string_type("#2"), string_type("#1")]
  
  res = ( stringarray == stringlist )
    ! res <-- .true.
  
  res = ( stringlist == ["#4", "#3", "#2", "#1"] )
    ! res <-- .true.

  print *, stringlist == ["#4", "#3", "#1"]
    ! .false.

end program demo_equality_operator
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
program demo_inequality_operator
  use stdlib_stringlist_type, only: stringlist_type, bidx, list_tail, operator(/=)
  use stdlib_string_type, only: string_type
  implicit none

  type(stringlist_type)          :: stringlist
  type(string_type), allocatable :: stringarray(:)
  logical                        :: res

  !> inserting 4 elements to the stringlist
  call stringlist%insert_at( bidx(1), "#1" )
  call stringlist%insert_at( list_tail, "#2" )
  call stringlist%insert_at( bidx(1), "#3" )
  call stringlist%insert_at( list_tail, "#4" )
    ! stringlist <-- {"#1", "#2", "#3", "#4"}

  !> creating an array of 4 string_type elements
    stringarray = [string_type("#1"), string_type("#2"), string_type("#3"), string_type("#4")]
  
  res = ( stringarray /= stringlist )
    ! res <-- .false.
  
  res = ( stringlist /= ["#111", "#222", "#333", "#444"] )
    ! res <-- .true.

  print *, stringlist /= ["#4", "#3", "#1"]
    ! .true.

end program demo_inequality_operator
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

The result is an instance of `stringlist_type`.

#### Example

```fortran
program demo_concatenate_operator
  use stdlib_stringlist_type, only: stringlist_type, operator(//)
  use stdlib_string_type, only: string_type
  implicit none

  type(stringlist_type)          :: first_stringlist, second_stringlist
  type(string_type), allocatable :: stringarray(:)

  first_stringlist = first_stringlist // "Element No. one"
    ! first_stringlist <-- {"Element No. one"}

  second_stringlist = string_type("Element No. two") // first_stringlist
    ! second_stringlist <-- {Element No. two, "Element No. one"}

  !> Creating an array of 2 string_type elements
  stringarray = [string_type("Element No. three"), string_type("Element No. four")]

  second_stringlist = first_stringlist // stringarray
    ! second_stringlist <-- {"Element No. one", "Element No. three", "Element No. four"}

  second_stringlist =  ["#1", "#2"] // second_stringlist
    ! second_stringlist <-- {"#1", "#2", "Element No. one", "Element No. three", "Element No. four"}

  first_stringlist = first_stringlist // second_stringlist
    ! first_stringlist <-- {"Element No. one", "#1", "#2", "Element No. one", "Element No. three", "Element No. four"}

end program demo_concatenate_operator
```