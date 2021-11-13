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

Returns the titlecase version of the input character variable.  
Title case: First character of every word in the sentence is converted to 
uppercase and the rest of the characters are converted to lowercase.  
A word is a contiguous sequence of character(s) which consists of alphabetical 
character(s) and numeral(s) only and doesn't exclude any alphabetical character 
or numeral present next to either of its 2 ends.

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
    print*, to_title("hello there!") ! returns "Hello There!"
    print*, to_title("'enquoted'") ! returns "'Enquoted'"
    print*, to_title("1st")  ! returns "1st"
 end program demo_to_title
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

`res = [[stdlib_ascii(module):to_sentence(function)]] (string)`

#### Class

Pure function.

#### Argument

`string`: shall be an intrinsic character type. It is an `intent(in)` argument.

#### Result value

The result is an intrinsic character type of the same length as `string`.

#### Example

```fortran
program demo_to_sentence
    use stdlib_ascii, only : to_sentence
    implicit none
    print*, to_sentence("hello!") ! returns "Hello!"
    print*, to_sentence("'enquoted'") ! returns "'Enquoted'"
    print*, to_sentence("1st")  ! returns "1st"
 end program demo_to_sentence
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