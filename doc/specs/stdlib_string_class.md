---
title: string class
---

# The `stdlib_string_class` module

[TOC]

## Introduction

The `stdlib_string_class` provides an abstract base class (ABC) to create an
extendible string object to hold an arbitrary character sequence compatibile
with most Fortran intrinsic character procedures as well as compatibility
with the stdlib [[stdlib_string_type(module):string_type(type)]].


## Derived types provides


<!-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -->
### The `string_class` derived type

The `string_class` is defined as an abstract derived type representing a
sequence of characters. The internal representation of the character sequence
is decided by the class inheriting from the `string_class`.

@note
The module provides the abstract base class and overloaded function interfaces
for the respective intrinsic functions.
Implementations of the string class should import all overloaded function interfaces
and reexport them to ease usage of the string implementation.
A minimal implementation must at least provide a setter as `assignment(=)`,
three getter functions (for the whole string, a specific index and a range)
as well as the length and trimmed length getter functions.
All other functionality is implemented by using the getter and setter functions in
the abstract base class, but implementations are encouraged to overwrite those with
procedures specific and optimal for their character sequence representation.


#### Status

Experimental


#### Example

```fortran
!> Minimal implementation of a string based on the stdlib string abstract base class
module string_implementation
    use stdlib_string_class, only : string_class, &
        len, len_trim, trim, index, scan, verify, repeat, adjustr, adjustl, &
        lgt, lge, llt, lle, char, ichar, iachar
    implicit none
    private

    public :: my_string_type
    public :: len, len_trim, trim, index, scan, verify, repeat, adjustr, adjustl
    public :: lgt, lge, llt, lle, char, ichar, iachar

    !> Definition of a string class implementation
    type, extends(string_class) :: my_string_type
        private
        character(len=:), allocatable :: raw
    contains
        !> Assign a character sequence to a string object.
        procedure :: assign_object_char
        !> Returns the length of the character sequence represented by the string.
        procedure :: get_len
        !> Returns the length of the character sequence without trailing spaces
        !> represented by the string.
        procedure :: get_len_trim
        !> Return the character sequence represented by the string.
        procedure :: get_char
        !> Return the character sequence represented by the string.
        procedure :: get_char_pos
        !> Return the character sequence represented by the string.
        procedure :: get_char_range
    end type my_string_type

    !> Constructor for string class implementation
    interface my_string_type
        module procedure :: new_string
    end interface my_string_type

contains

    !> Constructor for new string instances from a scalar character value.
    elemental function new_string(string) result(new)
        character(len=*), intent(in), optional :: string
        type(my_string_type) :: new
        if (present(string)) then
            new%raw = string
        end if
    end function new_string

    !> Assign a character sequence to a string object.
    elemental subroutine assign_object_char(lhs, rhs)
        class(my_string_type), intent(inout) :: lhs
        character(len=*), intent(in) :: rhs
        lhs%raw = rhs
    end subroutine assign_object_char

    !> Returns the length of the character sequence represented by the string.
    elemental function get_len(self) result(val)
        class(my_string_type), intent(in) :: self
        integer :: val
        val = merge(len(self%raw), 0, allocated(self%raw))
    end function get_len

    !> Returns the length of the character sequence without trailing spaces
    !> represented by the string.
    elemental function get_len_trim(self) result(val)
        class(my_string_type), intent(in) :: self
        integer :: val
        val = merge(len_trim(self%raw), 0, allocated(self%raw))
    end function get_len_trim

    !> Return the character sequence represented by the string.
    pure function get_char(self) result(character_string)
        class(my_string_type), intent(in) :: self
        character(len=:), allocatable :: character_string
        if (allocated(self%raw)) then
            character_string = self%raw
        else
            character_string = ""
        end if
    end function get_char

    !> Return the character sequence represented by the string.
    elemental function get_char_pos(self, pos) result(character_string)
        class(my_string_type), intent(in) :: self
        integer, intent(in) :: pos
        character(len=1) :: character_string
        if (allocated(self%raw)) then
            character_string = self%raw(pos:pos)
        else
            character_string = ""
        end if
    end function get_char_pos

    !> Return the character sequence represented by the string.
    pure function get_char_range(self, start, last) result(character_string)
        class(my_string_type), intent(in) :: self
        integer, intent(in) :: start
        integer, intent(in) :: last
        character(len=last-start+1) :: character_string
        if (allocated(self%raw)) then
            character_string = self%raw(start:last)
        else
            character_string = ""
        end if
    end function get_char_range

end module string_implementation
```


## Procedures and methods provided


<!-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -->
### Assignment of character scalar

#### Description

The ABC defines an assignment operations, `=`, to create a string object
from a character scalar.

#### Syntax

`lhs = rhs`

#### Status

Experimental

#### Class

Elemental, deferred subroutine, `assignment(=)`.


<!-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -->
### Assignment of string

#### Description

The ABC defines an assignment operations, `=`, to create a string class
from a `string_type` instance.

#### Syntax

`lhs = rhs`

#### Status

Experimental

#### Class

Elemental subroutine, `assignment(=)`.


<!-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -->
### Len function

#### Description

Returns the length of the string object.

#### Syntax

`res = [[stdlib_string_class(module):len(interface)]] (string)`

#### Status

Experimental

#### Class

Elemental function.

#### Argument

`string`: Instance of a `string_type`. This argument is `intent(in)`.

#### Result value

The result is a default integer scalar value.


<!-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -->
### Len\_trim function

#### Description

Returns the length of the character sequence without trailing spaces
represented by the string.

#### Syntax

`res = [[stdlib_string_class(module):len_trim(interface)]] (string)`

#### Status

Experimental

#### Class

Elemental function.

#### Argument

`string`: Instance of a `string_type`. This argument is `intent(in)`.

#### Result value

The result is a default integer scalar value.


<!-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -->
### Trim function

#### Description

Returns the character sequence hold by the string without trailing spaces
represented by a `string_type`.

#### Syntax

`res = [[stdlib_string_class(module):trim(interface)]] (string)`

#### Status

Experimental

#### Class

Elemental function.

#### Argument

- `string`: Instance of a `string_type`. This argument is `intent(in)`.

#### Result value

The result is a scalar `string_type` value.


<!-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -->
### Adjustl function

#### Description

Left-adjust the character sequence represented by the string.
The length of the character sequence remains unchanged.

#### Syntax

`res = [[stdlib_string_class(module):adjustl(interface)]] (string)`

#### Status

Experimental

#### Class

Elemental function.

#### Argument

- `string`: Instance of a `string_type`. This argument is `intent(in)`.

#### Result value

The result is a scalar `string_type` value.


<!-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -->
### Adjustr function

#### Description

Right-adjust the character sequence represented by the string.
The length of the character sequence remains unchanged.

#### Syntax

`res = [[stdlib_string_class(module):adjustr(interface)]] (string)`

#### Status

Experimental

#### Class

Elemental function.

#### Argument

- `string`: Instance of a `string_type`. This argument is `intent(in)`.

#### Result value

The result is a scalar `string_type` value.


<!-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -->
### Repeat function

#### Description

Repeats the character sequence hold by the string by the number of
specified copies.

#### Syntax

`res = [[stdlib_string_class(module):repeat(interface)]] (string, ncopies)`

#### Status

Experimental

#### Class

Elemental function.

#### Argument

- `string`: Instance of a `string_type`. This argument is `intent(in)`.
- `ncopies`: Integer of default type. This argument is `intent(in)`.

#### Result value

The result is a scalar `string_type` value.


<!-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -->
### Char function

#### Description

Return the character sequence represented by the string.

#### Syntax

`res = [[stdlib_string_class(module):char(interface)]] (string)`

#### Status

Experimental

#### Class

Pure function.

#### Argument

- `string`: Instance of a `string_type`. This argument is `intent(in)`.

#### Result value

The result is a scalar character value.


<!-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -->
### Char function (position variant)

#### Description

Return the character at a certain position in the string.

#### Syntax

`res = [[stdlib_string_class(module):char(interface)]] (string, pos)`

#### Status

Experimental

#### Class

Elemental function.

#### Argument

- `string`: Instance of a `string_type`. This argument is `intent(in)`.
- `pos`: Integer of default type. This argument is `intent(in)`.

#### Result value

The result is a scalar character value.


<!-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -->
### Char function (range variant)

#### Description

Return a substring from the character sequence of the string.

#### Syntax

`res = [[stdlib_string_class(module):char(interface)]] (string, start, last)`

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


<!-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -->
### Ichar function

#### Description

Character-to-integer conversion function.

Returns the code for the character in the first character position of the
character sequence in the system's native character set.

#### Syntax

`res = [[stdlib_string_class(module):ichar(interface)]] (string)`

#### Status

Experimental

#### Class

Elemental function.

#### Argument

- `string`: Instance of a `string_type`. This argument is `intent(in)`.

#### Result value

The result is a default integer scalar value.


<!-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -->
### Iachar function

#### Description

Code in ASCII collating sequence.

Returns the code for the ASCII character in the first character position of
the character sequences represent by the string.

#### Syntax

`res = [[stdlib_string_class(module):iachar(interface)]] (string)`

#### Status

Experimental

#### Class

Elemental function.

#### Argument

- `string`: Instance of a `string_type`. This argument is `intent(in)`.

#### Result value

The result is a default integer scalar value.


<!-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -->
### Index function

#### Description

Position of a *substring* within a *string*.

Returns the position of the start of the leftmost or rightmost occurrence
of string *substring* in *string*, counting from one. If *substring* is not
present in *string*, zero is returned.

#### Syntax

`res = [[stdlib_string_class(module):index(interface)]] (string, substring[, back])`

#### Status

Experimental

#### Class

Elemental function.

#### Argument

- `string`: Either scalar character value, string type or string object.
  This argument is `intent(in)`.
- `substring`: Either scalar character value, string type or string object.
  This argument is `intent(in)`.
- `back`: Either absent or a scalar logical value. This argument is `intent(in)`.

#### Result value

The result is a default integer scalar value.


<!-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -->
### Scan function

#### Description

Scans a *string* for the  presence any of the characters in a *set* of characters.
If *back* is either absent or *false*, this function returns the position
of the leftmost character of *string* that is in *set*. If *back* is *true*,
the rightmost position is returned. If no character of *set* is found in
*string*, the result is zero.

#### Syntax

`res = [[stdlib_string_class(module):scan(interface)]] (string, set[, back])`

#### Status

Experimental

#### Class

Elemental function.

#### Argument

- `string`: Either scalar character value, string type or string object.
  This argument is `intent(in)`.
- `set`: Either scalar character value, string type or string object.
  This argument is `intent(in)`.
- `back`: Either absent or a scalar logical value. This argument is `intent(in)`.

#### Result value

The result is a default integer scalar value.


<!-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -->
### Verify function

#### Description

Verifies that all the characters in *string* belong to the set of characters in *set*.
If *back* is either absent or *false*, this function returns the position
of the leftmost character of *string* that is not in *set*. If *back* is *true*,
the rightmost position is returned. If all characters of *string* are found
in *set*, the result is zero.

#### Syntax

`res = [[stdlib_string_class(module):verify(interface)]] (string, set[, back])`

#### Status

Experimental

#### Class

Elemental function.

#### Argument

- `string`: Either scalar character value, string type or string object.
  This argument is `intent(in)`.
- `set`: Either scalar character value, string type or string object.
  This argument is `intent(in)`.
- `back`: Either absent or a scalar logical value. This argument is `intent(in)`.

#### Result value

The result is a default integer scalar value.


<!-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -->
### Lgt function (lexical greater than)

#### Description

Lexically compare the order of two character sequences being greater than.

The left-hand side, the right-hand side or both character sequences can
be represented by a string object.
This defines five procedures overloading the intrinsic `lgt` procedure.

#### Syntax

`res = [[stdlib_string_class(module):lgt(interface)]] (lhs, rhs)`

#### Status

Experimental

#### Class

Elemental function.

#### Argument

- `lhs`: Either scalar character value, string type or string object.
  This argument is `intent(in)`.
- `rhs`: Either scalar character value, string type or string object.
  This argument is `intent(in)`.

#### Result value

The result is a default logical scalar value.


<!-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -->
### Llt function (lexical less than)

#### Description

Lexically compare the order of two character sequences being less than.

The left-hand side, the right-hand side or both character sequences can
be represented by a string object.
This defines five procedures overloading the intrinsic `llt` procedure.

#### Syntax

`res = [[stdlib_string_class(module):llt(interface)]] (lhs, rhs)`

#### Status

Experimental

#### Class

Elemental function.

#### Argument

- `lhs`: Either scalar character value, string type or string object.
  This argument is `intent(in)`.
- `rhs`: Either scalar character value, string type or string object.
  This argument is `intent(in)`.

#### Result value

The result is a default logical scalar value.


<!-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -->
### Lge function (lexical greater than or equal)

#### Description

Lexically compare the order of two character sequences being greater than
or equal.

The left-hand side, the right-hand side or both character sequences can
be represented by a string object.
This defines five procedures overloading the intrinsic `lge` procedure.

#### Syntax

`res = [[stdlib_string_class(module):lge(interface)]] (lhs, rhs)`

#### Status

Experimental

#### Class

Elemental function.

#### Argument

- `lhs`: Either scalar character value, string type or string object.
  This argument is `intent(in)`.
- `rhs`: Either scalar character value, string type or string object.
  This argument is `intent(in)`.

#### Result value

The result is a default logical scalar value.


<!-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -->
### Lle function (lexical less than or equal)

#### Description

Lexically compare the order of two character sequences being less than
or equal.

The left-hand side, the right-hand side or both character sequences can
be represented by a string object.
This defines five procedures overloading the intrinsic `lle` procedure.

#### Syntax

`res = [[stdlib_string_class(module):lle(interface)]] (lhs, rhs)`

#### Status

Experimental

#### Class

Elemental function.

#### Argument

- `lhs`: Either scalar character value, string type or string object.
  This argument is `intent(in)`.
- `rhs`: Either scalar character value, string type or string object.
  This argument is `intent(in)`.

#### Result value

The result is a default logical scalar value.


<!-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -->
### Comparison operator greater

#### Description

Compare the order of two character sequences being greater.

The left-hand side, the right-hand side or both character sequences can
be represented by a string object.
This defines five procedures overloading the intrinsic `operator(>)`
and `operator(.gt.)`.

#### Syntax

`res = lhs > rhs`

`res = lhs .gt. rhs`

#### Status

Experimental

#### Class

Elemental function, `operator(>)` and `operator(.gt.)`.

#### Argument

- `lhs`: Either scalar character value, string type or string object.
  This argument is `intent(in)`.
- `rhs`: Either scalar character value, string type or string object.
  This argument is `intent(in)`.

#### Result value

The result is a default logical scalar value.


<!-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -->
### Comparison operator less

#### Description

Compare the order of two character sequences being less.

The left-hand side, the right-hand side or both character sequences can
be represented by a string object.
This defines five procedures overloading the intrinsic `operator(<)`
and `operator(.lt.)`.

#### Syntax

`res = lhs < rhs`

`res = lhs .lt. rhs`

#### Status

Experimental

#### Class

Elemental function, `operator(<)` and `operator(.lt.)`.

#### Argument

- `lhs`: Either scalar character value, string type or string object.
  This argument is `intent(in)`.
- `rhs`: Either scalar character value, string type or string object.
  This argument is `intent(in)`.

#### Result value

The result is a default logical scalar value.


<!-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -->
### Comparison operator greater or equal

#### Description

Compare the order of two character sequences being greater or equal.

The left-hand side, the right-hand side or both character sequences can
be represented by a string object.
This defines five procedures overloading the intrinsic `operator(>=)`
and `operator(.ge.)`.

#### Syntax

`res = lhs >= rhs`

`res = lhs .ge. rhs`

#### Status

Experimental

#### Class

Elemental function, `operator(>=)` and `operator(.ge.)`.

#### Argument

- `lhs`: Either scalar character value, string type or string object.
  This argument is `intent(in)`.
- `rhs`: Either scalar character value, string type or string object.
  This argument is `intent(in)`.

#### Result value

The result is a default logical scalar value.


<!-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -->
### Comparison operator less or equal

#### Description

Compare the order of two character sequences being less or equal.

The left-hand side, the right-hand side or both character sequences can
be represented by a string object.
This defines five procedures overloading the intrinsic `operator(<=)`
and `operator(.le.)`.

#### Syntax

`res = lhs <= rhs`

`res = lhs .le. rhs`

#### Status

Experimental

#### Class

Elemental function, `operator(<=)` and `operator(.le.)`.

#### Argument

- `lhs`: Either scalar character value, string type or string object.
  This argument is `intent(in)`.
- `rhs`: Either scalar character value, string type or string object.
  This argument is `intent(in)`.

#### Result value

The result is a default logical scalar value.


<!-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -->
### Comparison operator equal

#### Description

Compare two character sequences for equality.

The left-hand side, the right-hand side or both character sequences can
be represented by a string object.
This defines five procedures overloading the intrinsic `operator(==)`
and `operator(.eq.)`.

#### Syntax

`res = lhs == rhs`

`res = lhs .eq. rhs`

#### Status

Experimental

#### Class

Elemental function, `operator(==)` and `operator(.eq.)`.

#### Argument

- `lhs`: Either scalar character value, string type or string object.
  This argument is `intent(in)`.
- `rhs`: Either scalar character value, string type or string object.
  This argument is `intent(in)`.

#### Result value

The result is a default logical scalar value.


<!-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -->
### Comparison operator not equal

#### Description

Compare two character sequences for inequality.

The left-hand side, the right-hand side or both character sequences can
be represented by a string object.
This defines five procedures overloading the intrinsic `operator(/=)`
and `operator(.ne.)`.

#### Syntax

`res = lhs /= rhs`

`res = lhs .ne. rhs`

#### Status

Experimental

#### Class

Elemental function, `operator(/=)` and `operator(.ne.)`.

#### Argument

- `lhs`: Either scalar character value, string type or string object.
  This argument is `intent(in)`.
- `rhs`: Either scalar character value, string type or string object.
  This argument is `intent(in)`.

#### Result value

The result is a default logical scalar value.


<!-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -->
### Concatenation operator

#### Description

Concatenate two character sequences.

The left-hand side, the right-hand side or both character sequences can
be represented by a string object.
This defines five procedures overloading the intrinsic `operator(//)`.

#### Syntax

`res = lhs // rhs`

#### Status

Experimental

#### Class

Elemental function, `operator(//)`.

#### Argument

- `lhs`: Either scalar character value, string type or string object.
  This argument is `intent(in)`.
- `rhs`: Either scalar character value, string type or string object.
  This argument is `intent(in)`.

#### Result value

The result is an instance of `string_type`.


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

<!-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -->
### Formatted write

#### Description

Write the character sequence hold by the string to a connected formatted unit.

#### Syntax

`write(unit, fmt, iostat=iostat, iomsg=iomsg) string`

#### Status

Experimental

#### Class

Formatted user defined derived type output.

#### Argument

- `string`: Instance of the string object to read. This argument is `intent(inout)`.
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

- `string`: Instance of the string object to read. This argument is `intent(inout)`.
- `unit`: Formatted unit for input. This argument is `intent(in)`.
- `iostat`: Status identifier to indicate success of input operation.
  This argument is `intent(out)`.
- `iomsg`: Buffer to return error message in case of failing input operation.
  This argument is `intent(inout)`.


<!-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -->
### Formatted read

#### Description

Read a character sequence from a connected formatted unit into the string.
List-directed input will retrieve the complete record into the string.

On failure the state the read variable is undefined and implementation dependent.

#### Syntax

`read(unit, fmt, iostat=iostat, iomsg=iomsg) string`

#### Status

Experimental

#### Class

Formatted derived type input.

#### Argument

- `string`: Instance of the string object to read. This argument is `intent(inout)`.
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
