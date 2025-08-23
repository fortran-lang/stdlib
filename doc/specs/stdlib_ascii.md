---
title: ascii
---

# The `stdlib_ascii` module

[TOC]

## Introduction

The `stdlib_ascii` module provides procedures for handling and manipulating
intrinsic character variables and constants.


## Constants provided by `stdlib_ascii`

### `NUL`

Null character

### `SOH`

Start Of Heading Character

### `STX`

Start Of Text character

### `ETX`

End Of Text character

### `EOT`

End Of Transmission character

### `ENQ`

Enquiry character

### `ACK`

Acknowledge character

### `BEL`

Bell character

### `BS`

Backspace character

### `TAB`

Horizontal Tab character

### `LF`

Line Feed character

### `VT`

Vertical Tab character

### `FF`

Form Feed character

### `CR`

Carriage Return character

### `SO`

Shift Out character

### `SI`

Shift In character

### `DLE`

Data Link Escape character

### `DC1`

Device Control 1 character

### `DC2`

Device Control 2 character

### `DC3`

Device Control 3 character

### `DC4`

Device Control 4 character

### `NAK`

Negative Acknowledge character

### `SYN`

Synchronous Idle character

### `ETB`

End of Transmission Block character

### `CAN`

Cancel character

### `EM`

End of Medium character

### `SUB`

Substitute character

### `ESC`

Escape character

### `FS`

File separator character

### `GS`

Group Separator character

### `RS`

Record Separator character

### `US`

Unit separator character

### `DEL`

Delete character

### `fullhex_digits`

All the hexadecimal digits (0-9, A-F, a-f)

### `hex_digits`

All the numerical and uppercase hexadecimal digits (0-9, A-F)

### `lowerhex_digits`

All the numerical and lowercase hexadecimal digits (0-9, a-f)

### `digits`

base 10 digits (0-9)

### `octal_digits`

base 8 digits (0-7)

### `letters`

Uppercase and lowercase letters of the english alphabet (A-Z, a-z)

### `uppercase`

Uppercase english albhabets (A-Z)

### `lowercase`

Lowercase english albhabets (a-z)

### `whitespace`

All the ascii whitespace characters (space, horizontal tab, vertical tab, carriage return, line feed, form feed)

## Specification of the `stdlib_ascii` procedures

### `is_alpha`

#### Status

Experimental

#### Description

Checks whether input character is an ASCII letter (A-Z, a-z).

#### Syntax

`res =` [[stdlib_ascii(module):is_alpha(function)]] `(c)`

#### Class

Elemental function.

#### Argument

`c`: shall be an intrinsic `character(len=1)` type. It is an `intent(in)` argument.

#### Result value

The result is a `logical`.

### `is_alphanum`

#### Status

Experimental

#### Description

Checks whether input character is an ASCII letter or a number (A-Z, a-z, 0-9).

#### Syntax

`res =` [[stdlib_ascii(module):is_alphanum(function)]] `(c)`

#### Class

Elemental function.

#### Argument

`c`: shall be an intrinsic `character(len=1)` type. It is an `intent(in)` argument.

#### Result value

The result is a `logical`.

### `is_ascii`

#### Status

Experimental

#### Description

Checks whether input character is in the ASCII character set i.e in the range 0-128.

#### Syntax

`res =` [[stdlib_ascii(module):is_ascii(function)]] `(c)`

#### Class

Elemental function.

#### Argument

`c`: shall be an intrinsic `character(len=1)` type. It is an `intent(in)` argument.

#### Result value

The result is a `logical`.

### `is_control`

#### Status

Experimental

#### Description

Checks whether input character is a control character.

#### Syntax

`res =` [[stdlib_ascii(module):is_control(function)]] `(c)`

#### Class

Elemental function.

#### Argument

`c`: shall be an intrinsic `character(len=1)` type. It is an `intent(in)` argument.

#### Result value

The result is a `logical`.

### `is_digit`

#### Status

Experimental

#### Description

Checks whether input character is a digit (0-9).

#### Syntax

`res =` [[stdlib_ascii(module):is_digit(function)]] `(c)`

#### Class

Elemental function.

#### Argument

`c`: shall be an intrinsic `character(len=1)` type. It is an `intent(in)` argument.

#### Result value

The result is a `logical`.

### `is_octal_digit`

#### Status

Experimental

#### Description

Checks whether input character is an octal digit (0-7)

#### Syntax

`res =` [[stdlib_ascii(module):is_octal_digit(function)]] `(c)`

#### Class

Elemental function.

#### Argument

`c`: shall be an intrinsic `character(len=1)` type. It is an `intent(in)` argument.

#### Result value

The result is a `logical`.

### `is_hex_digit`

#### Status

Experimental

#### Description

Checks whether input character is a hexadecimal digit (0-9, A-F, a-f).

#### Syntax

`res =` [[stdlib_ascii(module):is_hex_digit(function)]] `(c)`

#### Class

Elemental function.

#### Argument

`c`: shall be an intrinsic `character(len=1)` type. It is an `intent(in)` argument.

#### Result value

The result is a `logical`.

### `is_punctuation`

#### Status

Experimental

#### Description

Checks whether input character is a punctuation character.

#### Syntax

`res =` [[stdlib_ascii(module):is_punctuation(function)]] `(c)`

#### Class

Elemental function.

#### Argument

`c`: shall be an intrinsic `character(len=1)` type. It is an `intent(in)` argument.

#### Result value

The result is a `logical`.

### `is_graphical`

#### Status

Experimental

#### Description

Checks whether input character is a graphical character (printable other than the space character).

#### Syntax

`res =` [[stdlib_ascii(module):is_graphical(function)]] `(c)`

#### Class

Elemental function.

#### Argument

`c`: shall be an intrinsic `character(len=1)` type. It is an `intent(in)` argument.

#### Result value

The result is a `logical`.

### `is_printable`

#### Status

Experimental

#### Description

Checks whether input character is a printable character (including the space character).

#### Syntax

`res =` [[stdlib_ascii(module):is_printable(function)]] `(c)`

#### Class

Elemental function.

#### Argument

`c`: shall be an intrinsic `character(len=1)` type. It is an `intent(in)` argument.

#### Result value

The result is a `logical`.

### `is_lower`

#### Status

Experimental

#### Description

Checks whether input character is a lowercase ASCII letter (a-z).

#### Syntax

`res =` [[stdlib_ascii(module):is_lower(function)]] `(c)`

#### Class

Elemental function.

#### Argument

`c`: shall be an intrinsic `character(len=1)` type. It is an `intent(in)` argument.

#### Result value

The result is a `logical`.

### `is_upper`

#### Status

Experimental

#### Description

Checks whether input character is an uppercase ASCII letter (A-Z).

#### Syntax

`res =` [[stdlib_ascii(module):is_upper(function)]] `(c)`

#### Class

Elemental function.

#### Argument

`c`: shall be an intrinsic `character(len=1)` type. It is an `intent(in)` argument.

#### Result value

The result is a `logical`.

### `is_white`

#### Status

Experimental

#### Description

Checks whether input character is a whitespace character (which includes space, horizontal tab, vertical tab,
carriage return, linefeed and form feed characters)

#### Syntax

`res =` [[stdlib_ascii(module):is_white(function)]] `(c)`

#### Class

Elemental function.

#### Argument

`c`: shall be an intrinsic `character(len=1)` type. It is an `intent(in)` argument.

#### Result value

The result is a `logical`.

### `is_blank`

#### Status

Experimental

#### Description

Checks whether input character is a blank character (which includes space and tabs).

#### Syntax

`res =` [[stdlib_ascii(module):is_blank(function)]] `(c)`

#### Class

Elemental function.

#### Argument

`c`: shall be an intrinsic `character(len=1)` type. It is an `intent(in)` argument.

#### Result value

The result is a `logical`.

### `to_lower`

#### Status

Experimental

#### Description

Converts input character variable to all lowercase.

#### Syntax

`res =` [[stdlib_ascii(module):to_lower(function)]] `(string)`

#### Class

Elemental function.

#### Argument

`string`: shall be an intrinsic character type. It is an `intent(in)` argument.

#### Result value

The result is an intrinsic character type of the same length as `string`.

#### Example

```fortran
{!example/ascii/example_ascii_to_lower.f90!}
```

### `to_upper`

#### Status

Experimental

#### Description

Converts input character variable to all uppercase.

#### Syntax

`res =` [[stdlib_ascii(module):to_upper(function)]] `(string)`

#### Class

Elemental function.

#### Argument

`string`: shall be an intrinsic character type. It is an `intent(in)` argument.

#### Result value

The result is an intrinsic character type of the same length as `string`.

#### Example

```fortran
{!example/ascii/example_ascii_to_upper.f90!}
```

### `to_title`

#### Status

Experimental

#### Description

Returns the titlecase version of the input character variable.  
Title case: First character of every word in the sentence is converted to 
uppercase and the rest of the characters are converted to lowercase.  
A word is a contiguous sequence of character(s) which consists of alphabetical 
character(s) and numeral(s) only and doesn't exclude any alphabetical character 
or numeral present next to either of its 2 ends.

#### Syntax

`res =` [[stdlib_ascii(module):to_title(function)]] `(string)`

#### Class

Elemental function.

#### Argument

`string`: shall be an intrinsic character type. It is an `intent(in)` argument.

#### Result value

The result is an intrinsic character type of the same length as `string`.

#### Example

```fortran
{!example/ascii/example_ascii_to_title.f90!}
```

### `to_sentence`

#### Status

Experimental

#### Description

Returns the sentencecase version of the input character variable.  
The first alphabetical character of the sequence is transformed to uppercase 
unless it follows a numeral. The rest of the characters in the sequence are 
transformed to lowercase.

#### Syntax

`res =` [[stdlib_ascii(module):to_sentence(function)]] `(string)`

#### Class

Elemental function.

#### Argument

`string`: shall be an intrinsic character type. It is an `intent(in)` argument.

#### Result value

The result is an intrinsic character type of the same length as `string`.

#### Example

```fortran
{!example/ascii/example_ascii_to_sentence.f90!}
```

### `reverse`

#### Status

Experimental

#### Description

Reverses the order of all characters in the input character type.

#### Syntax

`res =` [[stdlib_ascii(module):reverse(function)]] `(string)`

#### Class

Elemental function.

#### Argument

`string`: shall be an intrinsic character type. It is an `intent(in)` argument.

#### Result value

The result is an intrinsic character type of the same length as `string`.

#### Example

```fortran
{!example/ascii/example_ascii_reverse.f90!}
```
