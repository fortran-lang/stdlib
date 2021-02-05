---
title: string type
---

# The `stdlib_string_type` module

[TOC]

## Introduction

The `stdlib_string_type` provides a derived type holding an arbitrary sequence
of characters compatible with most Fortran intrinsic character procedures as
well as operators for working with character variables and constants.


## Derived types provided


<!-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -->
### The `string_type` derived type

The `string_type` is defined as a non-extenable derived type representing a
sequence of characters. The internal representation of the character sequence
is implementation dependent and not visible for the user of the module.

#### Status

Experimental


## Procedures and methods provided

Procedures returning `string_type` instances can usually used in elemental
context, while procedures returning scalar character values can only be
used in a pure way.


<!-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -->
### Constructor for empty string

The module defines a default constructor to create an empty string type.

#### Description

Creates a string instance representing an empty string.

#### Syntax

`res = [[stdlib_string_type(module):string_type(interface)]] ()`

#### Status

Experimental

#### Class

Elemental function.

#### Argument

None.

#### Result value

The result is an instance of `string_type`.

#### Example

```fortran
use stdlib_string_type
implicit none
type(string_type) :: string
string = string_type()
! len(string) == 0
end
```


<!-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -->
### Constructor from character scalar

The module defines a default constructor to create a string type
from a character scalar.

#### Description

Creates a string instance representing the input character scalar value.
The constructor shall create an empty string if an unallocated deferred-length
character variable is passed.

#### Syntax

`res = [[stdlib_string_type(module):string_type(interface)]] (string)`

#### Status

Experimental

#### Class

Elemental function.

#### Argument

`string`: shall be a scalar character value. It is an `intent(in)` argument.

#### Result value

The result is an instance of `string_type`.

#### Example

```fortran
use stdlib_string_type
implicit none
type(string_type) :: string
string = string_type("Sequence")
! len(string) == 8
string = string_type(" S p a c e d ")
! len(string) == 9
end
```


<!-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -->
### Assignment of character scalar

The module defines an assignment operations, `=`, to create a string type
from a character scalar.

#### Description

Creates a string instance representing the right-hand-side character scalar value.

#### Syntax

`lhs = rhs`

#### Status

Experimental

#### Class

Elemntal subroutine, `assignment(=)`.

#### Example

```fortran
use stdlib_string_type
implicit none
type(string_type) :: string
! len(string) == 0
string = "Sequence"
! len(string) == 8
end
```


<!-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -->
### Len function

#### Description

Returns the length of the character sequence represented by the string.

#### Syntax

`res = [[stdlib_string_type(module):len(interface)]] (string)`

#### Status

Experimental

#### Class

Elemental function.

#### Argument

`string`: Instance of a `string_type`. This argument is `intent(in)`.

#### Result value

The result is a default integer scalar value.

#### Example

```fortran
use stdlib_string_type
implicit none
type(string_type) :: string
integer :: length

string = "Some longer sentence for this example."
length = len(string)
! length == 38

string = "Whitespace                            "
length = len(string)
! length == 38
end
```


<!-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -->
### Len\_trim function

#### Description

Returns the length of the character sequence without trailing spaces
represented by the string.

#### Syntax

`res = [[stdlib_string_type(module):len_trim(interface)]] (string)`

#### Status

Experimental

#### Class

Elemental function.

#### Argument

`string`: Instance of a `string_type`. This argument is `intent(in)`.

#### Result value

The result is a default integer scalar value.

#### Example

```fortran
use stdlib_string_type
implicit none
type(string_type) :: string
integer :: length

string = "Some longer sentence for this example."
length = len_trim(string)
! length == 38

string = "Whitespace                            "
length = len_trim(string)
! length == 10
end
```


<!-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -->
### Trim function

#### Description

Returns the character sequence hold by the string without trailing spaces
represented by a `string_type`.

#### Syntax

`res = [[stdlib_string_type(module):trim(interface)]] (string)`

#### Status

Experimental

#### Class

Elemental function.

#### Argument

- `string`: Instance of a `string_type`. This argument is `intent(in)`.

#### Result value

The result is a scalar `string_type` value.

#### Example

```fortran
use stdlib_string_type
implicit none
type(string_type) :: string

string = "Whitespace                            "
string = trim(string)
! len(string) == 10
end
```


<!-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -->
### Adjustl function

#### Description

Left-adjust the character sequence represented by the string.
The length of the character sequence remains unchanged.

#### Syntax

`res = [[stdlib_string_type(module):adjustl(interface)]] (string)`

#### Status

Experimental

#### Class

Elemental function.

#### Argument

- `string`: Instance of a `string_type`. This argument is `intent(in)`.

#### Result value

The result is a scalar `string_type` value.

#### Example

```fortran
use stdlib_string_type
implicit none
type(string_type) :: string

string = "                            Whitespace"
string = adjustl(string)
! char(string) == "Whitespace                            "
end
```


<!-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -->
### Adjustr function

#### Description

Right-adjust the character sequence represented by the string.
The length of the character sequence remains unchanged.

#### Syntax

`res = [[stdlib_string_type(module):adjustr(interface)]] (string)`

#### Status

Experimental

#### Class

Elemental function.

#### Argument

- `string`: Instance of a `string_type`. This argument is `intent(in)`.

#### Result value

The result is a scalar `string_type` value.

#### Example

```fortran
use stdlib_string_type
implicit none
type(string_type) :: string

string = "Whitespace                            "
string = adjustr(string)
! char(string) == "                            Whitespace"
end
```


<!-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -->
### Repeat function

#### Description

Repeats the character sequence hold by the string by the number of
specified copies.

#### Syntax

`res = [[stdlib_string_type(module):repeat(interface)]] (string, ncopies)`

#### Status

Experimental

#### Class

Elemental function.

#### Argument

- `string`: Instance of a `string_type`. This argument is `intent(in)`.
- `ncopies`: Integer of default type. This argument is `intent(in)`.

#### Result value

The result is a scalar `string_type` value.

#### Example

```fortran
use stdlib_string_type
implicit none
type(string_type) :: string

string = "What? "
string = repeat(string, 3)
! string == "What? What? What? "
end
```


<!-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -->
### Char function

#### Description

Return the character sequence represented by the string.

#### Syntax

`res = [[stdlib_string_type(module):char(interface)]] (string)`

#### Status

Experimental

#### Class

Pure function.

#### Argument

- `string`: Instance of a `string_type`. This argument is `intent(in)`.

#### Result value

The result is a scalar character value.

#### Example

```fortran
use stdlib_string_type
implicit none
type(string_type) :: string
character(len=:), allocatable :: dlc

string = "Character sequence"
dlc = char(string)
! dlc == "Character sequence"
end
```


<!-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -->
### Char function (position variant)

#### Description

Return the character sequence represented by the string.

#### Syntax

`res = [[stdlib_string_type(module):char(interface)]] (string, pos)`

#### Status

Experimental

#### Class

Elemental function.

#### Argument

- `string`: Instance of a `string_type`. This argument is `intent(in)`.
- `pos`: Integer of default type. This argument is `intent(in)`.

#### Result value

The result is a scalar character value.

#### Example

```fortran
use stdlib_string_type
implicit none
type(string_type) :: string
character(len=:), allocatable :: dlc
character(len=1), allocatable :: chars(:)

string = "Character sequence"
dlc = char(string, 3)
! dlc == "a"
chars = char(string, [3, 5, 8, 12, 14, 15, 18])
! chars == ["a", "a", "e", "e", "u", "e", "e"]
end
```


<!-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -->
### Char function (range variant)

#### Description

Return the character sequence represented by the string.

#### Syntax

`res = [[stdlib_string_type(module):char(interface)]] (string, start, last)`

#### Status

Experimental

#### Class

Pure function.

#### Argument

- `string`: Instance of a `string_type`. This argument is `intent(in)`.
- `start`: Integer of default type. This argument is `intent(in)`.
- `last`: Integer of default type. This argument is `intent(in)`.

#### Result value

The result is a scalar character value.

#### Example

```fortran
use stdlib_string_type
implicit none
type(string_type) :: string
character(len=:), allocatable :: dlc

string = "Fortran"
dlc = char(string, 1, 4)
! dlc == "Fort"
end
```


<!-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -->
### Ichar function

Character-to-integer conversion function.

#### Description

Returns the code for the character in the first character position of the
character sequence in the system's native character set.

#### Syntax

`res = [[stdlib_string_type(module):ichar(interface)]] (string)`

#### Status

Experimental

#### Class

Elemental function.

#### Argument

- `string`: Instance of a `string_type`. This argument is `intent(in)`.

#### Result value

The result is a default integer scalar value.

#### Example

```fortran
use stdlib_string_type
implicit none
type(string_type) :: string
integer :: code

string = "Fortran"
code = ichar(string)
end
```


<!-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -->
### Iachar function

Code in ASCII collating sequence.

#### Description

Returns the code for the ASCII character in the first character position of
the character sequences represent by the string.

#### Syntax

`res = [[stdlib_string_type(module):iachar(interface)]] (string)`

#### Status

Experimental

#### Class

Elemental function.

#### Argument

- `string`: Instance of a `string_type`. This argument is `intent(in)`.

#### Result value

The result is a default integer scalar value.

#### Example

```fortran
use stdlib_string_type
implicit none
type(string_type) :: string
integer :: code

string = "Fortran"
code = iachar(string)
end
```


<!-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -->
### Index function

Position of a *substring* within a *string*.

#### Description

Returns the position of the start of the leftmost or rightmost occurrence
of string *substring* in *string*, counting from one. If *substring* is not
present in *string*, zero is returned.

#### Syntax

`res = [[stdlib_string_type(module):index(interface)]] (string, substring[, back])`

#### Status

Experimental

#### Class

Elemental function.

#### Argument

- `string`: Either scalar character value or string type. This argument is `intent(in)`.
- `substring`: Either scalar character value or string type. This argument is `intent(in)`.
- `back`: Either absent or a scalar logical value. This argument is `intent(in)`.

#### Result value

The result is a default integer scalar value.

#### Example

```fortran
use stdlib_string_type
implicit none
type(string_type) :: string
integer :: pos

string = "Search this string for this expression"
pos = index(string, "this")
! pos == 8

pos = index(string, "this", back=.true.)
! pos == 24

pos = index(string, "This")
! pos == 0
end
```


<!-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -->
### Scan function

Scan a *string* for the presence of a *set* of characters. Scans a *string* for
any of the characters in a *set* of characters.

#### Description

If *back* is either absent or *false*, this function returns the position
of the leftmost character of *string* that is in *set*. If *back* is *true*,
the rightmost position is returned. If no character of *set* is found in
*string*, the result is zero.

#### Syntax

`res = [[stdlib_string_type(module):scan(interface)]] (string, set[, back])`

#### Status

Experimental

#### Class

Elemental function.

#### Argument

- `string`: Either scalar character value or string type. This argument is `intent(in)`.
- `set`: Either scalar character value or string type. This argument is `intent(in)`.
- `back`: Either absent or a scalar logical value. This argument is `intent(in)`.

#### Result value

The result is a default integer scalar value.

#### Example

```fortran
use stdlib_string_type
implicit none
type(string_type) :: string
integer :: pos

string = "fortran"
pos = scan(string, "ao")
! pos == 2

pos = scan(string, "ao", .true.)
! pos == 6

pos = scan(string, "c++")
! pos == 0
end
```


<!-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -->
### Verify function

Scan a string for the absence of a set of characters. Verifies that all
the characters in string belong to the set of characters in set.

#### Description

If *back* is either absent or *false*, this function returns the position
of the leftmost character of *string* that is not in *set*. If *back* is *true*,
the rightmost position is returned. If all characters of *string* are found
in *set*, the result is zero.

#### Syntax

`res = [[stdlib_string_type(module):verify(interface)]] (string, set[, back])`

#### Status

Experimental

#### Class

Elemental function.

#### Argument

- `string`: Either scalar character value or string type. This argument is `intent(in)`.
- `set`: Either scalar character value or string type. This argument is `intent(in)`.
- `back`: Either absent or a scalar logical value. This argument is `intent(in)`.

#### Result value

The result is a default integer scalar value.

#### Example

```fortran
use stdlib_string_type
implicit none
type(string_type) :: string
integer :: pos

string = "fortran"
pos = verify(string, "ao")
! pos == 1

pos = verify(string, "fo")
! pos == 3

pos = verify(string, "c++")
! pos == 1

pos = verify(string, "c++", back=.true.)
! pos == 7

pos = verify(string, string)
! pos == 0
end
```


<!-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -->
### Lgt function (lexical greater)

Lexically compare the order of two character sequences being greater.

#### Description

The left-hand side, the right-hand side or both character sequences can
be represented by a string type.
This defines three procedures overloading the intrinsic `lgt` procedure.

#### Syntax

`res = [[stdlib_string_type(module):lgt(interface)]] (lhs, rhs)`

#### Status

Experimental

#### Class

Elemental function.

#### Argument

- `lhs`: Either scalar character value or string type. This argument is `intent(in)`.
- `rhs`: Either scalar character value or string type. This argument is `intent(in)`.

#### Result value

The result is a default logical scalar value.

#### Example

```fortran
use stdlib_string_type
implicit none
type(string_type) :: string
logical :: res

string = "bcd"
res = lgt(string, "abc")
! res .eqv. .true.

res = lgt(string, "bcd")
! res .eqv. .false.

res = lgt(string, "cde")
! res .eqv. .false.
end
```


<!-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -->
### Llt function (lexical less)

Lexically compare the order of two character sequences being less.

#### Description

The left-hand side, the right-hand side or both character sequences can
be represented by a string type.
This defines three procedures overloading the intrinsic `llt` procedure.

#### Syntax

`res = [[stdlib_string_type(module):llt(interface)]] (lhs, rhs)`

#### Status

Experimental

#### Class

Elemental function.

#### Argument

- `lhs`: Either scalar character value or string type. This argument is `intent(in)`.
- `rhs`: Either scalar character value or string type. This argument is `intent(in)`.

#### Result value

The result is a default logical scalar value.

#### Example

```fortran
use stdlib_string_type
implicit none
type(string_type) :: string
logical :: res

string = "bcd"
res = llt(string, "abc")
! res .eqv. .false.

res = llt(string, "bcd")
! res .eqv. .false.

res = llt(string, "cde")
! res .eqv. .true.
end
```


<!-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -->
### Lge function (lexical greater or equal)

Lexically compare the order of two character sequences being greater or equal.

#### Description

The left-hand side, the right-hand side or both character sequences can
be represented by a string type.
This defines three procedures overloading the intrinsic `lge` procedure.

#### Syntax

`res = [[stdlib_string_type(module):lge(interface)]] (lhs, rhs)`

#### Status

Experimental

#### Class

Elemental function.

#### Argument

- `lhs`: Either scalar character value or string type. This argument is `intent(in)`.
- `rhs`: Either scalar character value or string type. This argument is `intent(in)`.

#### Result value

The result is a default logical scalar value.

#### Example

```fortran
use stdlib_string_type
implicit none
type(string_type) :: string
logical :: res

string = "bcd"
res = lge(string, "abc")
! res .eqv. .true.

res = lge(string, "bcd")
! res .eqv. .true.

res = lge(string, "cde")
! res .eqv. .false.
end
```


<!-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -->
### Lle function (lexical less or equal)

Lexically compare the order of two character sequences being less or equal.

#### Description

The left-hand side, the right-hand side or both character sequences can
be represented by a string type.
This defines three procedures overloading the intrinsic `lle` procedure.

#### Syntax

`res = [[stdlib_string_type(module):lle(interface)]] (lhs, rhs)`

#### Status

Experimental

#### Class

Elemental function.

#### Argument

- `lhs`: Either scalar character value or string type. This argument is `intent(in)`.
- `rhs`: Either scalar character value or string type. This argument is `intent(in)`.

#### Result value

The result is a default logical scalar value.

#### Example

```fortran
use stdlib_string_type
implicit none
type(string_type) :: string
logical :: res

string = "bcd"
res = lle(string, "abc")
! res .eqv. .false.

res = lle(string, "bcd")
! res .eqv. .true.

res = lle(string, "cde")
! res .eqv. .true.
end
```


<!-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -->
### Comparison operator greater

Compare the order of two character sequences being greater.

#### Description

The left-hand side, the right-hand side or both character sequences can
be represented by a string type.
This defines three procedures overloading the intrinsic `operator(.gt.)`.

#### Syntax

`res = lhs > rhs`

#### Status

Experimental

#### Class

Elemental function, `operator(.gt.)`.

#### Argument

- `lhs`: Either scalar character value or string type. This argument is `intent(in)`.
- `rhs`: Either scalar character value or string type. This argument is `intent(in)`.

#### Result value

The result is a default logical scalar value.

#### Example

```fortran
use stdlib_string_type
implicit none
type(string_type) :: string
logical :: res

string = "bcd"
res = string > "abc"
! res .eqv. .true.

res = string > "bcd"
! res .eqv. .false.

res = string > "cde"
! res .eqv. .false.
end
```


<!-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -->
### Comparison operator less

Compare the order of two character sequences being less.

#### Description

The left-hand side, the right-hand side or both character sequences can
be represented by a string type.
This defines three procedures overloading the intrinsic `operator(.lt.)`.

#### Syntax

`res = lhs < rhs`

#### Status

Experimental

#### Class

Elemental function, `operator(.lt.)`.

#### Argument

- `lhs`: Either scalar character value or string type. This argument is `intent(in)`.
- `rhs`: Either scalar character value or string type. This argument is `intent(in)`.

#### Result value

The result is a default logical scalar value.

#### Example

```fortran
use stdlib_string_type
implicit none
type(string_type) :: string
logical :: res

string = "bcd"
res = string < "abc"
! res .eqv. .false.

res = string < "bcd"
! res .eqv. .false.

res = string < "cde"
! res .eqv. .true.
end
```


<!-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -->
### Comparison operator greater or equal

Compare the order of two character sequences being greater or equal.

#### Description

The left-hand side, the right-hand side or both character sequences can
be represented by a string type.
This defines three procedures overloading the intrinsic `operator(.ge.)`.

#### Syntax

`res = lhs >= rhs`

#### Status

Experimental

#### Class

Elemental function, `operator(.ge.)`.

#### Argument

- `lhs`: Either scalar character value or string type. This argument is `intent(in)`.
- `rhs`: Either scalar character value or string type. This argument is `intent(in)`.

#### Result value

The result is a default logical scalar value.

#### Example

```fortran
use stdlib_string_type
implicit none
type(string_type) :: string
logical :: res

string = "bcd"
res = string >= "abc"
! res .eqv. .true.

res = string >= "bcd"
! res .eqv. .true.

res = string >= "cde"
! res .eqv. .false.
end
```


<!-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -->
### Comparison operator less or equal

Compare the order of two character sequences being less or equal.

#### Description

The left-hand side, the right-hand side or both character sequences can
be represented by a string type.
This defines three procedures overloading the intrinsic `operator(.le.)`.

#### Syntax

`res = lhs <= rhs`

#### Status

Experimental

#### Class

Elemental function, `operator(.le.)`.

#### Argument

- `lhs`: Either scalar character value or string type. This argument is `intent(in)`.
- `rhs`: Either scalar character value or string type. This argument is `intent(in)`.

#### Result value

The result is a default logical scalar value.

#### Example

```fortran
use stdlib_string_type
implicit none
type(string_type) :: string
logical :: res

string = "bcd"
res = string <= "abc"
! res .eqv. .false.

res = string <= "bcd"
! res .eqv. .true.

res = string <= "cde"
! res .eqv. .true.
end
```


<!-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -->
### Comparison operator equal

Compare two character sequences for equality.

#### Description

The left-hand side, the right-hand side or both character sequences can
be represented by a string type.
This defines three procedures overloading the intrinsic `operator(.eq.)`.

#### Syntax

`res = lhs == rhs`

#### Status

Experimental

#### Class

Elemental function, `operator(.eq.)`.

#### Argument

- `lhs`: Either scalar character value or string type. This argument is `intent(in)`.
- `rhs`: Either scalar character value or string type. This argument is `intent(in)`.

#### Result value

The result is a default logical scalar value.

#### Example

```fortran
use stdlib_string_type
implicit none
type(string_type) :: string
logical :: res

string = "bcd"
res = string == "abc"
! res .eqv. .false.

res = string == "bcd"
! res .eqv. .true.

res = string == "cde"
! res .eqv. .false.
end
```


<!-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -->
### Comparison operator not equal

Compare two character sequences for inequality.

#### Description

The left-hand side, the right-hand side or both character sequences can
be represented by a string type.
This defines three procedures overloading the intrinsic `operator(.ne.)`.

#### Syntax

`res = lhs /= rhs`

#### Status

Experimental

#### Class

Elemental function, `operator(.ne.)`.

#### Argument

- `lhs`: Either scalar character value or string type. This argument is `intent(in)`.
- `rhs`: Either scalar character value or string type. This argument is `intent(in)`.

#### Result value

The result is a default logical scalar value.

#### Example

```fortran
use stdlib_string_type
implicit none
type(string_type) :: string
logical :: res

string = "bcd"
res = string /= "abc"
! res .eqv. .true.

res = string /= "bcd"
! res .eqv. .false.

res = string /= "cde"
! res .eqv. .true.
end
```


<!-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -->
### Concatenation operator

Concatenate two character sequences.

#### Description

The left-hand side, the right-hand side or both character sequences can
be represented by a string type.
This defines three procedures overloading the intrinsic `operator(//)`.

#### Syntax

`res = lhs // rhs`

#### Status

Experimental

#### Class

Elemental function, `operator(//)`.

#### Argument

- `lhs`: Either scalar character value or string type. This argument is `intent(in)`.
- `rhs`: Either scalar character value or string type. This argument is `intent(in)`.

#### Result value

The result is an instance of `string_type`.

#### Example

```fortran
use stdlib_string_type
implicit none
type(string_type) :: string

string = "Hello, "
string = string // "World!"
! len(string) == 13
end
```


<!-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -->
### Unformatted write

#### Description

Write the character sequence hold by the string to a connected unformatted unit.
The character sequences is represented by an 64 bit signed integer record,
holding the length of the following character record.

#### Syntax

`write(unit, iostat=iostat, iomsg=iomsg) string`

#### Status

Experimental

#### Class

Unformatted user defined derived type output.

#### Argument

- `string`: Instance of the string type to read. This argument is `intent(inout)`.
- `unit`: Formatted unit for output. This argument is `intent(in)`.
- `iostat`: Status identifier to indicate success of output operation.
  This argument is `intent(out)`.
- `iomsg`: Buffer to return error message in case of failing output operation.
  This argument is `intent(inout)`.

#### Example

```fortran
use stdlib_string_type
implicit none
type(string_type) :: string
integer :: io
string = "Important saved value"

open(newunit=io, form="unformatted", status="scratch")
write(io) string

rewind(io)

read(io) string
close(io)
end
```

<!-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -->
### Formatted write

#### Description

Write the character sequence hold by the string to a connected formatted unit.

The current implementation is limited to list directed output and `dt` formatted
output. Requesting namelist output will raise an error.

#### Syntax

`write(unit, fmt, iostat=iostat, iomsg=iomsg) string`

#### Status

Experimental

#### Class

Formatted user defined derived type output.

#### Argument

- `string`: Instance of the string type to read. This argument is `intent(inout)`.
- `unit`: Formatted unit for output. This argument is `intent(in)`.
- `iotype`: Type of formatted data transfer, has the value `"LISTDIRECTED"` for `fmt=*`,
  `"NAMELIST"` for namelist output or starts with `"DT"` for derived type output.
  This argument is `intent(in)`.
- `v_list`: Rank one array of default integer type containing the edit descriptors for
  derived type output.
  This argument is `intent(in)`.
- `iostat`: Status identifier to indicate success of output operation.
  This argument is `intent(out)`.
- `iomsg`: Buffer to return error message in case of failing output operation.
  This argument is `intent(inout)`.

#### Example

```fortran
use stdlib_string_type
implicit none
type(string_type) :: string
integer :: io
string = "Important saved value"

open(newunit=io, form="formatted", status="scratch")
write(io, *) string
write(io, *)

rewind(io)

read(io, *) string
close(io)
end
```


<!-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -->
### Unformatted read

#### Description

Read a character sequence from a connected unformatted unit into the string.
The character sequences is represented by an 64 bit signed integer record,
holding the length of the following character record.

On failure the state the read variable is undefined and implementation dependent.

#### Syntax

`read(unit, iostat=iostat, iomsg=iomsg) string`

#### Status

Experimental

#### Class

Unformatted derived type input.

#### Argument

- `string`: Instance of the string type to read. This argument is `intent(inout)`.
- `unit`: Formatted unit for input. This argument is `intent(in)`.
- `iostat`: Status identifier to indicate success of input operation.
  This argument is `intent(out)`.
- `iomsg`: Buffer to return error message in case of failing input operation.
  This argument is `intent(inout)`.

#### Example

```fortran
use stdlib_string_type
implicit none
type(string_type) :: string
integer :: io
string = "Important saved value"

open(newunit=io, form="unformatted", status="scratch")
write(io) string

rewind(io)

read(io) string
close(io)
end
```


<!-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -->
### Formatted read

#### Description

Read a character sequence from a connected formatted unit into the string.
List-directed input will retrieve the complete record into the string.

On failure the state the read variable is undefined and implementation dependent.

The current implementation is limited to list directed input.
Requesting `dt` formatted input or namelist output will raise an error.

#### Syntax

`read(unit, fmt, iostat=iostat, iomsg=iomsg) string`

#### Status

Experimental

#### Class

Formatted derived type input.

#### Argument

- `string`: Instance of the string type to read. This argument is `intent(inout)`.
- `unit`: Formatted unit for input. This argument is `intent(in)`.
- `iotype`: Type of formatted data transfer, has the value `"LISTDIRECTED"` for `fmt=*`,
  `"NAMELIST"` for namelist input or starts with `"DT"` for derived type input.
  This argument is `intent(in)`.
- `v_list`: Rank one array of default integer type containing the edit descriptors for
  derived type input.
  This argument is `intent(in)`.
- `iostat`: Status identifier to indicate success of input operation.
  This argument is `intent(out)`.
- `iomsg`: Buffer to return error message in case of failing input operation.
  This argument is `intent(inout)`.

#### Example

```fortran
use stdlib_string_type
implicit none
type(string_type) :: string
integer :: io
string = "Important saved value"

open(newunit=io, form="formatted", status="scratch")
write(io, *) string
write(io, *)

rewind(io)

read(io, *) string
close(io)
end
```
