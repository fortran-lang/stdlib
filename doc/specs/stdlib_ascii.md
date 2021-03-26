---
title: ASCII
---

# The `stdlib_ascii` module

[TOC]

## Introduction

The `stdlib_ascii` module provides procedures for handling and manipulating
intrinsic character variables and constants.


## Constants provided by `stdlib_ascii`

@note Specification of constants is currently incomplete.


## Specification of the `stdlib_ascii` procedures

@note Specification of procedures is currently incomplete.


### `to_lower`

#### Status

Experimental

#### Description

Converts input character variable to all lowercase.

#### Syntax

`res = [[stdlib_ascii(module):to_lower(function)]] (string)`

#### Class

Pure function.

#### Argument

`string`: shall be an intrinsic character type. It is an `intent(in)` argument.

#### Result value

The result is an intrinsic character type of the same length as `string`.

#### Example

```fortran
program demo_to_lower
    use stdlib_ascii, only : to_lower
    implicit none
    print'(a)', to_lower("HELLo!") ! returns "hello!"
 end program demo_to_lower
``` 

### `to_upper`

#### Status

Experimental

#### Description

Converts input character variable to all uppercase.

#### Syntax

`res = [[stdlib_ascii(module):to_upper(function)]] (string)`

#### Class

Pure function.

#### Argument

`string`: shall be an intrinsic character type. It is an `intent(in)` argument.

#### Result value

The result is an intrinsic character type of the same length as `string`.

#### Example

```fortran
program demo_to_upper
    use stdlib_ascii, only : to_upper
    implicit none
    print'(a)', to_upper("hello!") ! returns "HELLO!"
 end program demo_to_upper
``` 

### `to_title`

#### Status

Experimental

#### Description

Returns a capitalized version of an input character variable.
The first alphabetical character is transformed to uppercase unless it follows a numeral.
The rest of the character sequence is transformed to lowercase.

#### Syntax

`res = [[stdlib_ascii(module):to_title(function)]] (string)`

#### Class

Pure function.

#### Argument

`string`: shall be an intrinsic character type. It is an `intent(in)` argument.

#### Result value

The result is an intrinsic character type of the same length as `string`.

#### Example

```fortran
program demo_to_title
    use stdlib_ascii, only : to_title
    implicit none
    print*, to_title("hello!") ! returns "Hello!"
    print*, to_title("'enquoted'") ! returns "'Enquoted'"
    print*, to_title("1st")  ! returns "1st"
 end program demo_to_title
``` 

### `reverse`

#### Status

Experimental

#### Description

Reverses the order of all characters in the input character type.

#### Syntax

`res = [[stdlib_ascii(module):reverse(function)]] (string)`

#### Class

Pure function.

#### Argument

`string`: shall be an intrinsic character type. It is an `intent(in)` argument.

#### Result value

The result is an intrinsic character type of the same length as `string`.

#### Example

```fortran
program demo_reverse
    use stdlib_ascii, only : reverse
    implicit none
    print'(a)', reverse("Hello, World!") ! returns "!dlroW ,olleH"
end program demo_reverse
```

### `sort`

#### Status

Experimental

#### Description

Sorts all the characters in the input character type according to their ascii values.

#### Syntax

`res = [[stdlib_ascii(module):sort(function)]] (string)`

#### Class

Pure function.

#### Argument

`string`: shall be an intrinsic character type. It is an `intent(in)` argument.

#### Result value

The result is an intrinsic character type of the same length as `string`.

#### Example

```fortran
program demo_sort
    use stdlib_ascii, only : sort
    implicit none
    print'(a)', sort("Sort This String") ! returns "  SSTghiinorrstt"
end program demo_sort
```