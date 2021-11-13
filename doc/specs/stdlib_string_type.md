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

`res = [[stdlib_string_type(module):string_type(interface)]] ()`

#### Class

Elemental function.

#### Argument

None.

#### Result value

The result is an instance of `string_type` with zero length.

#### Example

```fortran
program demo
  use stdlib_string_type
  implicit none
  type(string_type) :: string
  string = string_type()
  ! len(string) == 0
end program demo
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

`res = [[stdlib_string_type(module):string_type(interface)]] (string)`

#### Class

Elemental function.

#### Argument

`string`: shall be a scalar character value. It is an `intent(in)` argument.

#### Result value

The result is an instance of `string_type`.

#### Example

```fortran
program demo
  use stdlib_string_type
  implicit none
  type(string_type) :: string
  string = string_type("Sequence")
  ! len(string) == 8
  string = string_type(" S p a c e d ")
  ! len(string) == 13
end program demo
```


<!-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -->
### Constructor from integer scalar

#### Status

Experimental

#### Description

The module defines a constructor to create a string type from an integer scalar.

#### Syntax

`res = [[stdlib_string_type(module):string_type(interface)]] (string)`

#### Class

Elemental function.

#### Argument

`val`: shall be a scalar integer value. It is an `intent(in)` argument.

#### Result value

The result is an instance of `string_type`.

#### Example

```fortran
program demo
  use stdlib_string_type
  implicit none
  type(string_type) :: string
  string = string_type(42)
  ! len(string) == 2
  string = string_type(-289)
  ! len(string) == 4
end program demo
```


<!-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -->
### Constructor from logical scalar

#### Status

Experimental

#### Description

The module defines a constructor to create a string type from a logical scalar.

#### Syntax

`res = [[stdlib_string_type(module):string_type(interface)]] (string)`

#### Class

Elemental function.

#### Argument

`val`: shall be a scalar logical value. It is an `intent(in)` argument.

#### Result value

The result is an instance of `string_type`.

#### Example

```fortran
program demo
  use stdlib_string_type
  implicit none
  type(string_type) :: string
  string = string_type(.true.)
  ! len(string) == 1
  string = string_type(.false.)
  ! len(string) == 1
end program demo
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
program demo
  use stdlib_string_type
  implicit none
  type(string_type) :: string
  ! len(string) == 0
  string = "Sequence"
  ! len(string) == 8
end program demo
```


<!-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -->
### Len function

#### Status

Experimental

#### Description

Returns the length of the string.

#### Syntax

`res = [[stdlib_string_type(module):len(interface)]] (string)`

#### Class

Elemental function.

#### Argument

`string`: Instance of a `string_type`. This argument is `intent(in)`.

#### Result value

The result is a default integer scalar value.

#### Example

```fortran
program demo
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
end program demo
```


<!-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -->
### Len\_trim function

#### Status

Experimental

#### Description

Returns the length of the character sequence without trailing spaces
represented by the string.

#### Syntax

`res = [[stdlib_string_type(module):len_trim(interface)]] (string)`

#### Class

Elemental function.

#### Argument

`string`: Instance of a `string_type`. This argument is `intent(in)`.

#### Result value

The result is a default integer scalar value.

#### Example

```fortran
program demo
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
end program demo
```


<!-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -->
### Trim function

#### Status

Experimental

#### Description

Returns the character sequence hold by the string without trailing spaces
represented by a `string_type`.

#### Syntax

`res = [[stdlib_string_type(module):trim(interface)]] (string)`

#### Class

Elemental function.

#### Argument

- `string`: Instance of a `string_type`. This argument is `intent(in)`.

#### Result value

The result is a scalar `string_type` value.

#### Example

```fortran
program demo
  use stdlib_string_type
  implicit none
  type(string_type) :: string

  string = "Whitespace                            "
  string = trim(string)
  ! len(string) == 10
end program demo
```


<!-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -->
### Adjustl function

#### Status

Experimental

#### Description

Left-adjust the character sequence represented by the string.
The length of the character sequence remains unchanged.

#### Syntax

`res = [[stdlib_string_type(module):adjustl(interface)]] (string)`

#### Class

Elemental function.

#### Argument

- `string`: Instance of a `string_type`. This argument is `intent(in)`.

#### Result value

The result is a scalar `string_type` value.

#### Example

```fortran
program demo
  use stdlib_string_type
  implicit none
  type(string_type) :: string

  string = "                            Whitespace"
  string = adjustl(string)
  ! char(string) == "Whitespace                            "
end program demo
```


<!-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -->
### Adjustr function

#### Status

Experimental

#### Description

Right-adjust the character sequence represented by the string.
The length of the character sequence remains unchanged.

#### Syntax

`res = [[stdlib_string_type(module):adjustr(interface)]] (string)`

#### Class

Elemental function.

#### Argument

- `string`: Instance of a `string_type`. This argument is `intent(in)`.

#### Result value

The result is a scalar `string_type` value.

#### Example

```fortran
program demo
  use stdlib_string_type
  implicit none
  type(string_type) :: string

  string = "Whitespace                            "
  string = adjustr(string)
  ! char(string) == "                            Whitespace"
end program demo
```


<!-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -->
### Repeat function

#### Status

Experimental

#### Description

Repeats the character sequence hold by the string by the number of
specified copies.

#### Syntax

`res = [[stdlib_string_type(module):repeat(interface)]] (string, ncopies)`

#### Class

Elemental function.

#### Argument

- `string`: Instance of a `string_type`. This argument is `intent(in)`.
- `ncopies`: Integer of default type. This argument is `intent(in)`.

#### Result value

The result is a scalar `string_type` value.

#### Example

```fortran
program demo
  use stdlib_string_type
  implicit none
  type(string_type) :: string

  string = "What? "
  string = repeat(string, 3)
  ! string == "What? What? What? "
end program demo
```


<!-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -->
### Char function

#### Status

Experimental

#### Description

Return the character sequence represented by the string.

#### Syntax

`res = [[stdlib_string_type(module):char(interface)]] (string)`

#### Class

Pure function.

#### Argument

- `string`: Instance of a `string_type`. This argument is `intent(in)`.

#### Result value

The result is a scalar character value.

#### Example

```fortran
program demo
  use stdlib_string_type
  implicit none
  type(string_type) :: string
  character(len=:), allocatable :: dlc

  string = "Character sequence"
  dlc = char(string)
  ! dlc == "Character sequence"
end program demo
```


<!-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -->
### Char function (position variant)

#### Status

Experimental

#### Description

Return the character at a certain position in the string.

#### Syntax

`res = [[stdlib_string_type(module):char(interface)]] (string, pos)`

#### Class

Elemental function.

#### Argument

- `string`: Instance of a `string_type`. This argument is `intent(in)`.
- `pos`: Integer of default type. This argument is `intent(in)`.

#### Result value

The result is a scalar character value.

#### Example

```fortran
program demo
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
end program demo
```


<!-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -->
### Char function (range variant)

#### Status

Experimental

#### Description

Return a substring from the character sequence of the string.

#### Syntax

`res = [[stdlib_string_type(module):char(interface)]] (string, start, last)`

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
program demo
  use stdlib_string_type
  implicit none
  type(string_type) :: string
  character(len=:), allocatable :: dlc

  string = "Fortran"
  dlc = char(string, 1, 4)
  ! dlc == "Fort"
end program demo
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

`res = [[stdlib_string_type(module):ichar(interface)]] (string)`

#### Class

Elemental function.

#### Argument

- `string`: Instance of a `string_type`. This argument is `intent(in)`.

#### Result value

The result is a default integer scalar value.

#### Example

```fortran
program demo
  use stdlib_string_type
  implicit none
  type(string_type) :: string
  integer :: code

  string = "Fortran"
  code = ichar(string)
end program demo
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

`res = [[stdlib_string_type(module):iachar(interface)]] (string)`

#### Class

Elemental function.

#### Argument

- `string`: Instance of a `string_type`. This argument is `intent(in)`.

#### Result value

The result is a default integer scalar value.

#### Example

```fortran
program demo
  use stdlib_string_type
  implicit none
  type(string_type) :: string
  integer :: code

  string = "Fortran"
  code = iachar(string)
end program demo
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

`res = [[stdlib_string_type(module):index(interface)]] (string, substring[, back])`

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
program demo
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
end program demo
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

`res = [[stdlib_string_type(module):scan(interface)]] (string, set[, back])`

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
program demo
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
end program demo
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

`res = [[stdlib_string_type(module):verify(interface)]] (string, set[, back])`

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
program demo
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
end program demo
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

`res = [[stdlib_string_type(module):lgt(interface)]] (lhs, rhs)`

#### Class

Elemental function.

#### Argument

- `lhs`: Either scalar character value or string type. This argument is `intent(in)`.
- `rhs`: Either scalar character value or string type. This argument is `intent(in)`.

#### Result value

The result is a default logical scalar value.

#### Example

```fortran
program demo
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
end program demo
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

`res = [[stdlib_string_type(module):llt(interface)]] (lhs, rhs)`

#### Class

Elemental function.

#### Argument

- `lhs`: Either scalar character value or string type. This argument is `intent(in)`.
- `rhs`: Either scalar character value or string type. This argument is `intent(in)`.

#### Result value

The result is a default logical scalar value.

#### Example

```fortran
program demo
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
end program demo
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

`res = [[stdlib_string_type(module):lge(interface)]] (lhs, rhs)`

#### Class

Elemental function.

#### Argument

- `lhs`: Either scalar character value or string type. This argument is `intent(in)`.
- `rhs`: Either scalar character value or string type. This argument is `intent(in)`.

#### Result value

The result is a default logical scalar value.

#### Example

```fortran
program demo
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
end program demo
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

`res = [[stdlib_string_type(module):lle(interface)]] (lhs, rhs)`

#### Class

Elemental function.

#### Argument

- `lhs`: Either scalar character value or string type. This argument is `intent(in)`.
- `rhs`: Either scalar character value or string type. This argument is `intent(in)`.

#### Result value

The result is a default logical scalar value.

#### Example

```fortran
program demo
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
end program demo
```


<!-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -->
### To\_lower function

#### Status

Experimental

#### Description

Returns a new string_type instance which holds the lowercase version of the
character sequence hold by the input string.

#### Syntax

`lowercase_string = [[stdlib_string_type(module): to_lower(interface)]] (string)`

#### Class

Elemental function.

#### Argument

`string`: Instance of `string_type`. This argument is `intent(in)`.

#### Result Value

The result is a scalar `string_type` value.

#### Example

```fortran
program demo
  use stdlib_string_type
  implicit none
  type(string_type) :: string, lowercase_string

  string = "Lowercase This String"
  ! string <-- "Lowercase This String"

  lowercase_string = to_lower(string)
  ! string <-- "Lowercase This String"
  ! lowercase_string <-- "lowercase this string"
end program demo
```


<!-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -->
### To\_upper function

#### Status

Experimental

#### Description

Returns a new string_type instance which holds the uppercase version of the
character sequence hold by the input string.

#### Syntax

`uppercase_string = [[stdlib_string_type(module): to_upper(interface)]] (string)`

#### Class

Elemental function.

#### Argument

`string`: Instance of `string_type`. This argument is `intent(in)`.

#### Result Value

The result is a scalar `string_type` value.

#### Example

```fortran
program demo
  use stdlib_string_type
  implicit none
  type(string_type) :: string, uppercase_string

  string = "Uppercase This String"
  ! string <-- "Uppercase This String"

  uppercase_string = to_upper(string)
  ! string <-- "Uppercase This String"
  ! uppercase_string <-- "UPPERCASE THIS STRING"
end program demo
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

`titlecase_string = [[stdlib_string_type(module): to_title(interface)]] (string)`

#### Class

Elemental function.

#### Argument

`string`: Instance of `string_type`. This argument is `intent(in)`.

#### Result Value

The result is a scalar `string_type` value.

#### Example

```fortran
program demo_to_title
  use stdlib_string_type
  implicit none
  type(string_type) :: string, titlecase_string

  string = "titlecase this string."
  ! string <-- "titlecase this string."

  titlecase_string = to_title(string)
  ! string <-- "titlecase this string."
  ! titlecase_string <-- "Titlecase This String."
end program demo_to_title
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

`sentencecase_string = [[stdlib_string_type(module): to_sentence(interface)]] (string)`

#### Class

Elemental function.

#### Argument

`string`: Instance of `string_type`. This argument is `intent(in)`.

#### Result Value

The result is a scalar `string_type` value.

#### Example

```fortran
program demo_to_sentence
  use stdlib_string_type
  implicit none
  type(string_type) :: string, sentencecase_string

  string = "sentencecase this string."
  ! string <-- "sentencecase this string."

  sentencecase_string = to_sentence(string)
  ! string <-- "sentencecase this string."
  ! sentencecase_string <-- "Sentencecase this string."
end program demo_to_sentence
```

<!-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -->
### Reverse function

#### Status

Experimental

#### Description

Returns a new string_type instance which holds the reversed version of the
character sequence hold by the input string.

#### Syntax

`reverse_string = [[stdlib_string_type(module): reverse(interface)]] (string)`

#### Class

Elemental function.

#### Argument

`string`: Instance of `string_type`. This argument is `intent(in)`.

#### Result Value

The result is a scalar `string_type` value.

#### Example

```fortran
program demo
  use stdlib_string_type
  implicit none
  type(string_type) :: string, reverse_string

  string = "Reverse This String"
  ! string <-- "Reverse This String"

  reverse_string = reverse(string)
  ! string <-- "Reverse This String"
  ! reverse_string <-- "gnirtS sihT esreveR"
end program demo
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
program demo
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
end program demo
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
program demo
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
end program demo
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
program demo
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
end program demo
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
program demo
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
end program demo
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
program demo
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
end program demo
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
program demo
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
end program demo
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
program demo
  use stdlib_string_type
  implicit none
  type(string_type) :: string

  string = "Hello, "
  string = string // "World!"
  ! len(string) == 13
end program demo
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
program demo
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
end program demo
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
program demo
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
end program demo
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
program demo
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
end program demo
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
program demo
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
end program demo
```


<!-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -->
### move

#### Status

Experimental

#### Description

Moves the allocation from `from` to `to`, consequently deallocating `from` in this process.
If `from` is not allocated before execution, `to` gets deallocated by the process.
An unallocated `string_type` instance is equivalent to an empty string.

#### Syntax

`call [[stdlib_string_type(module):move(interface)]] (from, to)`

#### Class

Pure Subroutine.

#### Argument

- `from`: Character scalar or [[stdlib_string_type(module):string_type(type)]].
  This argument is `intent(inout)`.
- `to`: Character scalar or [[stdlib_string_type(module):string_type(type)]].
  This argument is `intent(out)`.

#### Example

```fortran
program demo_move
  use stdlib_string_type, only : string_type, assignment(=), move
  implicit none
  type(string_type) :: from_string
  character(len=:), allocatable :: from_char, to_char

  from_string = "move this string"
  from_char = "move this char"
  ! from_string <-- "move this string"
  ! from_char   <-- "move this char"
  ! to_char   <-- (unallocated)

  call move(from_string, to_char)
  ! from_string <-- ""
  ! to_char   <-- "move this string"

  call move(from_char, to_char)
  ! from_char <-- (unallocated)
  ! to_string <-- "move this char"

end program demo_move
```
