---
title: string_type
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

The `string_type` is defined as a non-extendible derived type representing a
sequence of characters. The internal representation of the character sequence
is implementation dependent and not visible for the user of the module.

#### Status

Experimental


## Procedures and methods provided

Procedures returning `string_type` instances can usually be used in elemental
context, while procedures returning scalar character values can only be
used in a pure way.


<!-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -->
### Constructor for empty string

#### Status

Experimental

#### Description

The module defines a constructor to create an empty string type.

Creates a string instance representing an empty string.

#### Syntax

`res = ` [[stdlib_string_type(module):string_type(interface)]] ` ()`

#### Class

Elemental function.

#### Argument

None.

#### Result value

The result is an instance of `string_type` with zero length.

#### Example

```fortran
{!example/string_type/example_constructor_empty.f90!}
```


<!-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -->
### Constructor from character scalar

#### Status

Experimental

#### Description

The module defines a constructor to create a string type from a character scalar.

Creates a string instance representing the input character scalar value.
The constructor shall create an empty string if an unallocated deferred-length
character variable is passed.

#### Syntax

`res = ` [[stdlib_string_type(module):string_type(interface)]] ` (string)`

#### Class

Elemental function.

#### Argument

`string`: shall be a scalar character value. It is an `intent(in)` argument.

#### Result value

The result is an instance of `string_type`.

#### Example

```fortran
{!example/string_type/example_constructor_scalar.f90!}
```


<!-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -->
### Constructor from integer scalar

#### Status

Experimental

#### Description

The module defines a constructor to create a string type from an integer scalar.

#### Syntax

`res = ` [[stdlib_string_type(module):string_type(interface)]] ` (string)`

#### Class

Elemental function.

#### Argument

`val`: shall be a scalar integer value. It is an `intent(in)` argument.

#### Result value

The result is an instance of `string_type`.

#### Example

```fortran
{!example/string_type/example_constructor_integer.f90!}
```


<!-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -->
### Constructor from logical scalar

#### Status

Experimental

#### Description

The module defines a constructor to create a string type from a logical scalar.

#### Syntax

`res = ` [[stdlib_string_type(module):string_type(interface)]] ` (string)`

#### Class

Elemental function.

#### Argument

`val`: shall be a scalar logical value. It is an `intent(in)` argument.

#### Result value

The result is an instance of `string_type`.

#### Example

```fortran
{!example/string_type/example_constructor_logical.f90!}
```


<!-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -->
### Assignment of character scalar

#### Status

Experimental

#### Description

The module defines an assignment operations, `=`, to create a string type
from a character scalar.

Creates a string instance representing the right-hand-side character scalar value.

#### Syntax

`lhs = rhs`

#### Class

Elemental subroutine, `assignment(=)`.

#### Example

```fortran
{!example/string_type/example_constructor_character.f90!}
```


<!-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -->
### Len function

#### Status

Experimental

#### Description

Returns the length of the string.

#### Syntax

`res = ` [[stdlib_string_type(module):len(interface)]] ` (string)`

#### Class

Elemental function.

#### Argument

`string`: Instance of a `string_type`. This argument is `intent(in)`.

#### Result value

The result is a default integer scalar value.

#### Example

```fortran
{!example/string_type/example_len.f90!}
```


<!-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -->
### Len\_trim function

#### Status

Experimental

#### Description

Returns the length of the character sequence without trailing spaces
represented by the string.

#### Syntax

`res = ` [[stdlib_string_type(module):len_trim(interface)]] ` (string)`

#### Class

Elemental function.

#### Argument

`string`: Instance of a `string_type`. This argument is `intent(in)`.

#### Result value

The result is a default integer scalar value.

#### Example

```fortran
{!example/string_type/example_len_trim.f90!}
```


<!-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -->
### Trim function

#### Status

Experimental

#### Description

Returns the character sequence hold by the string without trailing spaces
represented by a `string_type`.

#### Syntax

`res = ` [[stdlib_string_type(module):trim(interface)]] ` (string)`

#### Class

Elemental function.

#### Argument

- `string`: Instance of a `string_type`. This argument is `intent(in)`.

#### Result value

The result is a scalar `string_type` value.

#### Example

```fortran
{!example/string_type/example_trim.f90!}
```


<!-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -->
### Adjustl function

#### Status

Experimental

#### Description

Left-adjust the character sequence represented by the string.
The length of the character sequence remains unchanged.

#### Syntax

`res = ` [[stdlib_string_type(module):adjustl(interface)]] ` (string)`

#### Class

Elemental function.

#### Argument

- `string`: Instance of a `string_type`. This argument is `intent(in)`.

#### Result value

The result is a scalar `string_type` value.

#### Example

```fortran
{!example/string_type/example_adjustl.f90!}
```


<!-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -->
### Adjustr function

#### Status

Experimental

#### Description

Right-adjust the character sequence represented by the string.
The length of the character sequence remains unchanged.

#### Syntax

`res = ` [[stdlib_string_type(module):adjustr(interface)]] ` (string)`

#### Class

Elemental function.

#### Argument

- `string`: Instance of a `string_type`. This argument is `intent(in)`.

#### Result value

The result is a scalar `string_type` value.

#### Example

```fortran
{!example/string_type/example_adjustr.f90!}
```


<!-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -->
### Repeat function

#### Status

Experimental

#### Description

Repeats the character sequence hold by the string by the number of
specified copies.

#### Syntax

`res = ` [[stdlib_string_type(module):repeat(interface)]] ` (string, ncopies)`

#### Class

Elemental function.

#### Argument

- `string`: Instance of a `string_type`. This argument is `intent(in)`.
- `ncopies`: Integer of default type. This argument is `intent(in)`.

#### Result value

The result is a scalar `string_type` value.

#### Example

```fortran
{!example/string_type/example_repeat.f90!}
```


<!-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -->
### Char function

#### Status

Experimental

#### Description

Return the character sequence represented by the string.

#### Syntax

`res = ` [[stdlib_string_type(module):char(interface)]] ` (string)`

#### Class

Pure function.

#### Argument

- `string`: Instance of a `string_type`. This argument is `intent(in)`.

#### Result value

The result is a scalar character value.

#### Example

```fortran
{!example/string_type/example_char.f90!}
```


<!-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -->
### Char function (position variant)

#### Status

Experimental

#### Description

Return the character at a certain position in the string.

#### Syntax

`res = ` [[stdlib_string_type(module):char(interface)]] ` (string, pos)`

#### Class

Elemental function.

#### Argument

- `string`: Instance of a `string_type`. This argument is `intent(in)`.
- `pos`: Integer of default type. This argument is `intent(in)`.

#### Result value

The result is a scalar character value.

#### Example

```fortran
{!example/string_type/example_char_position.f90!}
```


<!-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -->
### Char function (range variant)

#### Status

Experimental

#### Description

Return a substring from the character sequence of the string.

#### Syntax

`res = ` [[stdlib_string_type(module):char(interface)]] ` (string, start, last)`

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
{!example/string_type/example_char_range.f90!}
```


<!-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -->
### Ichar function

#### Status

Experimental

#### Description

Character-to-integer conversion function.

Returns the code for the character in the first character position of the
character sequence in the system's native character set.

#### Syntax

`res = ` [[stdlib_string_type(module):ichar(interface)]] ` (string)`

#### Class

Elemental function.

#### Argument

- `string`: Instance of a `string_type`. This argument is `intent(in)`.

#### Result value

The result is a default integer scalar value.

#### Example

```fortran
{!example/string_type/example_ichar.f90!}
```


<!-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -->
### Iachar function

#### Status

Experimental

#### Description

Code in ASCII collating sequence.

Returns the code for the ASCII character in the first character position of
the character sequences represent by the string.

#### Syntax

`res = ` [[stdlib_string_type(module):iachar(interface)]] ` (string)`

#### Class

Elemental function.

#### Argument

- `string`: Instance of a `string_type`. This argument is `intent(in)`.

#### Result value

The result is a default integer scalar value.

#### Example

```fortran
{!example/string_type/example_iachar.f90!}
```


<!-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -->
### Index function

#### Status

Experimental

#### Description

Position of a *substring* within a *string*.

Returns the position of the start of the leftmost or rightmost occurrence
of string *substring* in *string*, counting from one. If *substring* is not
present in *string*, zero is returned.

#### Syntax

`res = ` [[stdlib_string_type(module):index(interface)]] ` (string, substring[, back])`

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
{!example/string_type/example_index.f90!}
```


<!-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -->
### Scan function

#### Status

Experimental

#### Description

Scans a *string* for the  presence any of the characters in a *set* of characters.
If *back* is either absent or *false*, this function returns the position
of the leftmost character of *string* that is in *set*. If *back* is *true*,
the rightmost position is returned. If no character of *set* is found in
*string*, the result is zero.

#### Syntax

`res = ` [[stdlib_string_type(module):scan(interface)]] ` (string, set[, back])`

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
{!example/string_type/example_scan.f90!}
```


<!-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -->
### Verify function

#### Status

Experimental

#### Description

Verifies that all the characters in *string* belong to the set of characters in *set*.
If *back* is either absent or *false*, this function returns the position
of the leftmost character of *string* that is not in *set*. If *back* is *true*,
the rightmost position is returned. If all characters of *string* are found
in *set*, the result is zero.

#### Syntax

`res = ` [[stdlib_string_type(module):verify(interface)]] ` (string, set[, back])`

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
{!example/string_type/example_verify.f90!}
```


<!-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -->
### Lgt function (lexical greater than)

#### Status

Experimental

#### Description

Lexically compare the order of two character sequences being greater than.

The left-hand side, the right-hand side or both character sequences can
be represented by a string type.
This defines three procedures overloading the intrinsic `lgt` procedure.

#### Syntax

`res = ` [[stdlib_string_type(module):lgt(interface)]] ` (lhs, rhs)`

#### Class

Elemental function.

#### Argument

- `lhs`: Either scalar character value or string type. This argument is `intent(in)`.
- `rhs`: Either scalar character value or string type. This argument is `intent(in)`.

#### Result value

The result is a default logical scalar value.

#### Example

```fortran
{!example/string_type/example_lgt.f90!}
```


<!-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -->
### Llt function (lexical less than)

#### Status

Experimental

#### Description

Lexically compare the order of two character sequences being less than.

The left-hand side, the right-hand side or both character sequences can
be represented by a string type.
This defines three procedures overloading the intrinsic `llt` procedure.

#### Syntax

`res = ` [[stdlib_string_type(module):llt(interface)]] ` (lhs, rhs)`

#### Class

Elemental function.

#### Argument

- `lhs`: Either scalar character value or string type. This argument is `intent(in)`.
- `rhs`: Either scalar character value or string type. This argument is `intent(in)`.

#### Result value

The result is a default logical scalar value.

#### Example

```fortran
{!example/string_type/example_llt.f90!}
```


<!-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -->
### Lge function (lexical greater than or equal)

#### Status

Experimental

#### Description

Lexically compare the order of two character sequences being greater than
or equal.

The left-hand side, the right-hand side or both character sequences can
be represented by a string type.
This defines three procedures overloading the intrinsic `lge` procedure.

#### Syntax

`res = ` [[stdlib_string_type(module):lge(interface)]] ` (lhs, rhs)`

#### Class

Elemental function.

#### Argument

- `lhs`: Either scalar character value or string type. This argument is `intent(in)`.
- `rhs`: Either scalar character value or string type. This argument is `intent(in)`.

#### Result value

The result is a default logical scalar value.

#### Example

```fortran
{!example/string_type/example_lge.f90!}
```


<!-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -->
### Lle function (lexical less than or equal)

#### Status

Experimental

#### Description

Lexically compare the order of two character sequences being less than
or equal.

The left-hand side, the right-hand side or both character sequences can
be represented by a string type.
This defines three procedures overloading the intrinsic `lle` procedure.

#### Syntax

`res = ` [[stdlib_string_type(module):lle(interface)]] ` (lhs, rhs)`

#### Class

Elemental function.

#### Argument

- `lhs`: Either scalar character value or string type. This argument is `intent(in)`.
- `rhs`: Either scalar character value or string type. This argument is `intent(in)`.

#### Result value

The result is a default logical scalar value.

#### Example

```fortran
{!example/string_type/example_lle.f90!}
```


<!-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -->
### To\_lower function

#### Status

Experimental

#### Description

Returns a new string_type instance which holds the lowercase version of the
character sequence hold by the input string.

#### Syntax

`lowercase_string = ` [[stdlib_string_type(module):to_lower(interface)]] ` (string)`

#### Class

Elemental function.

#### Argument

`string`: Instance of `string_type`. This argument is `intent(in)`.

#### Result Value

The result is a scalar `string_type` value.

#### Example

```fortran
{!example/string_type/example_to_lower.f90!}
```


<!-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -->
### To\_upper function

#### Status

Experimental

#### Description

Returns a new string_type instance which holds the uppercase version of the
character sequence hold by the input string.

#### Syntax

`uppercase_string = ` [[stdlib_string_type(module):to_upper(interface)]] ` (string)`

#### Class

Elemental function.

#### Argument

`string`: Instance of `string_type`. This argument is `intent(in)`.

#### Result Value

The result is a scalar `string_type` value.

#### Example

```fortran
{!example/string_type/example_to_upper.f90!}
```


<!-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -->
### To\_title function

#### Status

Experimental

#### Description

Returns a new string_type instance which holds the titlecase version
of the character sequence hold by the input string.
Title case: First character of every word in the sentence is converted to
uppercase and the rest of the characters are converted to lowercase.
A word is a contiguous sequence of character(s) which consists of alphabetical
character(s) and numeral(s) only and doesn't exclude any alphabetical character
or numeral present next to either of its 2 ends.

#### Syntax

`titlecase_string = ` [[stdlib_string_type(module):to_title(interface)]] ` (string)`

#### Class

Elemental function.

#### Argument

`string`: Instance of `string_type`. This argument is `intent(in)`.

#### Result Value

The result is a scalar `string_type` value.

#### Example

```fortran
{!example/string_type/example_to_title.f90!}
```

<!-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -->
### To\_sentence function

#### Status

Experimental

#### Description

Returns a new string_type instance which holds the sentencecase
version of the character sequence hold by the input string.
Sentencecase version: The first alphabetical character of the input character sequence
is transformed to uppercase unless it follows a numeral and the rest of the
characters in the sequence are transformed to lowercase.

#### Syntax

`sentencecase_string = ` [[stdlib_string_type(module):to_sentence(interface)]] ` (string)`

#### Class

Elemental function.

#### Argument

`string`: Instance of `string_type`. This argument is `intent(in)`.

#### Result Value

The result is a scalar `string_type` value.

#### Example

```fortran
{!example/string_type/example_to_sentence.f90!}
```

<!-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -->
### Reverse function

#### Status

Experimental

#### Description

Returns a new string_type instance which holds the reversed version of the
character sequence hold by the input string.

#### Syntax

`reverse_string = ` [[stdlib_string_type(module):reverse(interface)]] ` (string)`

#### Class

Elemental function.

#### Argument

`string`: Instance of `string_type`. This argument is `intent(in)`.

#### Result Value

The result is a scalar `string_type` value.

#### Example

```fortran
{!example/string_type/example_reverse.f90!}
```


<!-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -->
### Comparison operator greater

#### Status

Experimental

#### Description

Compare the order of two character sequences being greater.

The left-hand side, the right-hand side or both character sequences can
be represented by a string type.
This defines three procedures overloading the intrinsic `operator(>)`
and `operator(.gt.)`.

#### Syntax

`res = lhs > rhs`

`res = lhs .gt. rhs`

#### Class

Elemental function, `operator(>)` and `operator(.gt.)`.

#### Argument

- `lhs`: Either scalar character value or string type. This argument is `intent(in)`.
- `rhs`: Either scalar character value or string type. This argument is `intent(in)`.

#### Result value

The result is a default logical scalar value.

#### Example

```fortran
{!example/string_type/example_gt.f90!}
```


<!-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -->
### Comparison operator less

#### Status

Experimental

#### Description

Compare the order of two character sequences being less.

The left-hand side, the right-hand side or both character sequences can
be represented by a string type.
This defines three procedures overloading the intrinsic `operator(<)`
and `operator(.lt.)`.

#### Syntax

`res = lhs < rhs`

`res = lhs .lt. rhs`

#### Class

Elemental function, `operator(<)` and `operator(.lt.)`.

#### Argument

- `lhs`: Either scalar character value or string type. This argument is `intent(in)`.
- `rhs`: Either scalar character value or string type. This argument is `intent(in)`.

#### Result value

The result is a default logical scalar value.

#### Example

```fortran
{!example/string_type/example_lt.f90!}
```


<!-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -->
### Comparison operator greater or equal

#### Status

Experimental

#### Description

Compare the order of two character sequences being greater or equal.

The left-hand side, the right-hand side or both character sequences can
be represented by a string type.
This defines three procedures overloading the intrinsic `operator(>=)`
and `operator(.ge.)`.

#### Syntax

`res = lhs >= rhs`

`res = lhs .ge. rhs`

#### Class

Elemental function, `operator(>=)` and `operator(.ge.)`.

#### Argument

- `lhs`: Either scalar character value or string type. This argument is `intent(in)`.
- `rhs`: Either scalar character value or string type. This argument is `intent(in)`.

#### Result value

The result is a default logical scalar value.

#### Example

```fortran
{!example/string_type/example_ge.f90!}
```


<!-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -->
### Comparison operator less or equal

#### Status

Experimental

#### Description

Compare the order of two character sequences being less or equal.

The left-hand side, the right-hand side or both character sequences can
be represented by a string type.
This defines three procedures overloading the intrinsic `operator(<=)`
and `operator(.le.)`.

#### Syntax

`res = lhs <= rhs`

`res = lhs .le. rhs`

#### Class

Elemental function, `operator(<=)` and `operator(.le.)`.

#### Argument

- `lhs`: Either scalar character value or string type. This argument is `intent(in)`.
- `rhs`: Either scalar character value or string type. This argument is `intent(in)`.

#### Result value

The result is a default logical scalar value.

#### Example

```fortran
{!example/string_type/example_le.f90!}
```


<!-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -->
### Comparison operator equal

#### Status

Experimental

#### Description

Compare two character sequences for equality.

The left-hand side, the right-hand side or both character sequences can
be represented by a string type.
This defines three procedures overloading the intrinsic `operator(==)`
and `operator(.eq.)`.

#### Syntax

`res = lhs == rhs`

`res = lhs .eq. rhs`

#### Class

Elemental function, `operator(==)` and `operator(.eq.)`.

#### Argument

- `lhs`: Either scalar character value or string type. This argument is `intent(in)`.
- `rhs`: Either scalar character value or string type. This argument is `intent(in)`.

#### Result value

The result is a default logical scalar value.

#### Example

```fortran
{!example/string_type/example_eq.f90!}
```


<!-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -->
### Comparison operator not equal

#### Status

Experimental

#### Description

Compare two character sequences for inequality.

The left-hand side, the right-hand side or both character sequences can
be represented by a string type.
This defines three procedures overloading the intrinsic `operator(/=)`
and `operator(.ne.)`.

#### Syntax

`res = lhs /= rhs`

`res = lhs .ne. rhs`

#### Class

Elemental function, `operator(/=)` and `operator(.ne.)`.

#### Argument

- `lhs`: Either scalar character value or string type. This argument is `intent(in)`.
- `rhs`: Either scalar character value or string type. This argument is `intent(in)`.

#### Result value

The result is a default logical scalar value.

#### Example

```fortran
{!example/string_type/example_ne.f90!}
```


<!-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -->
### Concatenation operator

#### Status

Experimental

#### Description

Concatenate two character sequences.

The left-hand side, the right-hand side or both character sequences can
be represented by a string type.
This defines three procedures overloading the intrinsic `operator(//)`.

#### Syntax

`res = lhs // rhs`

#### Class

Elemental function, `operator(//)`.

#### Argument

- `lhs`: Either scalar character value or string type. This argument is `intent(in)`.
- `rhs`: Either scalar character value or string type. This argument is `intent(in)`.

#### Result value

The result is an instance of `string_type`.

#### Example

```fortran
{!example/string_type/example_cont.f90!}
```


<!-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -->
### Unformatted write

#### Status

Experimental

#### Description

Write the character sequence hold by the string to a connected unformatted unit.
The character sequences is represented by an 64 bit signed integer record,
holding the length of the following character record.

#### Syntax

`write(unit, iostat=iostat, iomsg=iomsg) string`

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
{!example/string_type/example_uwrite.f90!}
```

<!-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -->
### Formatted write

#### Status

Experimental

#### Description

Write the character sequence hold by the string to a connected formatted unit.

The current implementation is limited to list directed output and `dt` formatted
output. Requesting namelist output will raise an error.

#### Syntax

`write(unit, fmt, iostat=iostat, iomsg=iomsg) string`

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
{!example/string_type/example_fwrite.f90!}
```


<!-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -->
### Unformatted read

#### Status

Experimental

#### Description

Read a character sequence from a connected unformatted unit into the string.
The character sequences is represented by an 64 bit signed integer record,
holding the length of the following character record.

On failure the state the read variable is undefined and implementation dependent.

#### Syntax

`read(unit, iostat=iostat, iomsg=iomsg) string`

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
{!example/string_type/example_uread.f90!}
```


<!-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -->
### Formatted read

#### Status

Experimental

#### Description

Read a character sequence from a connected formatted unit into the string.
List-directed input will retrieve the complete record into the string.

On failure the state the read variable is undefined and implementation dependent.

The current implementation is limited to list directed input.
Requesting `dt` formatted input or namelist output will raise an error.

#### Syntax

`read(unit, fmt, iostat=iostat, iomsg=iomsg) string`

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
{!example/string_type/example_fread.f90!}
```


<!-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -->
### move

#### Status

Experimental

#### Description

Moves the allocation from `from` to `to`, consequently deallocating `from` in this process.
If `from` is not allocated before execution, `to` gets deallocated by the process.
An unallocated `string_type` instance is equivalent to an empty string.
If `from` and `to` are the same variable, then `from` remains unchanged.

#### Syntax

`call ` [[stdlib_string_type(module):move(interface)]] ` (from, to)`

#### Class

Pure subroutine (Elemental subroutine, only when both `from` and `to` are `type(string_type)`)

#### Argument

- `from`: Character scalar or [[stdlib_string_type(module):string_type(type)]].
  This argument is `intent(inout)`.
- `to`: Character scalar or [[stdlib_string_type(module):string_type(type)]].
  This argument is `intent(inout)` when both `from` and `to` are `type(string_type)`,
  otherwise `intent(out)`.

#### Example

```fortran
{!example/string_type/example_move.f90!}
```
